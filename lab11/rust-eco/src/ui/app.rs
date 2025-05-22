use iced::{
    alignment, window, Application, Color, Command, Element, Length, Settings,
    Theme, Subscription,
};
use iced::widget::{
    button, column, container, row, text, slider, pick_list, toggler,
    scrollable, canvas,
};
use iced::widget::canvas::{Cache, Path, Stroke};
use std::time::Duration;

use crate::simulation::{EntityType, GRID_SIZE, Position, SENSING_RADIUS};
use crate::simulation::environment::Environment;
use crate::simulation::agent::AgentSystem;
use crate::simulation::heuristics::{HeuristicStrategy, HeuristicParameters};

const CELL_SIZE: f32 = 8.0;

pub fn run() -> iced::Result {
    EcosystemApp::run(Settings {
        window: window::Settings {
            size: (1280, 720),
            ..window::Settings::default()
        },
        ..Settings::default()
    })
}

#[derive(Debug, Clone)]
pub enum Message {
    Tick,
    ToggleSimulation,
    SpeedChanged(f32),
    AddAgent(HeuristicStrategy),
    ResetSimulation,
    SpawnRandomAgents(usize),
    SpawnEvolvedAgents(usize),
    MutationRateChanged(f32),
    ToggleDayNight,
    HeuristicStrategySelected(HeuristicStrategy),
    ParameterChanged(String, f32),
    ApplyCustomParameters,
    SelectAgent(usize),
}

pub struct EcosystemApp {
    environment: Environment,
    agent_system: AgentSystem,
    running: bool,
    speed: f32, // Ticks per second
    grid_cache: Cache,
    tick_count: usize,
    selected_heuristic: HeuristicStrategy,
    custom_parameters: HeuristicParameters,
    mutation_rate: f32,
    selected_agent_id: Option<usize>,
}

impl Application for EcosystemApp {
    type Message = Message;
    type Theme = Theme;
    type Executor = iced::executor::Default;
    type Flags = ();

    fn new(_flags: ()) -> (Self, Command<Message>) {
        let mut environment = Environment::new();
        let mut agent_system = AgentSystem::new();
        
        // Spawn initial agents
        agent_system.spawn_random_agents(&mut environment, 20);
        
        (
            Self {
                environment,
                agent_system,
                running: false,
                speed: 5.0,
                grid_cache: Cache::new(),
                tick_count: 0,
                selected_heuristic: HeuristicStrategy::Survival,
                custom_parameters: HeuristicParameters::default_survival(),
                mutation_rate: 0.1,
                selected_agent_id: None,
            },
            Command::none(),
        )
    }

    fn title(&self) -> String {
        String::from("Ecosystem Simulation")
    }

    fn update(&mut self, message: Message) -> Command<Message> {
        match message {
            Message::Tick => {
                if self.running {
                    // Update environment
                    self.environment.update_resources();
                    self.environment.update_day_night_cycle();
                    
                    // Update agents
                    self.agent_system.update_all(&mut self.environment);
                    
                    // Reset cache to redraw
                    self.grid_cache.clear();
                    
                    // Count ticks
                    self.tick_count += 1;
                    
                    // If we're low on agents, spawn some more
                    if self.agent_system.agents.len() < 5 && self.tick_count % 50 == 0 {
                        if self.tick_count > 100 {
                            // After some time, use evolution
                            self.agent_system.spawn_evolved_agents(&mut self.environment, 5, self.mutation_rate);
                        } else {
                            // At the beginning, spawn random agents
                            self.agent_system.spawn_random_agents(&mut self.environment, 5);
                        }
                    }
                }
                Command::none()
            }
            Message::ToggleSimulation => {
                self.running = !self.running;
                Command::none()
            }
            Message::SpeedChanged(new_speed) => {
                self.speed = new_speed;
                Command::none()
            }
            Message::AddAgent(strategy) => {
                self.agent_system.spawn_agent(&mut self.environment, strategy);
                self.grid_cache.clear();
                Command::none()
            }
            Message::ResetSimulation => {
                self.environment = Environment::new();
                self.agent_system = AgentSystem::new();
                self.agent_system.spawn_random_agents(&mut self.environment, 20);
                self.tick_count = 0;
                self.grid_cache.clear();
                self.selected_agent_id = None;
                Command::none()
            }
            Message::SpawnRandomAgents(count) => {
                self.agent_system.spawn_random_agents(&mut self.environment, count);
                self.grid_cache.clear();
                Command::none()
            }
            Message::SpawnEvolvedAgents(count) => {
                self.agent_system.spawn_evolved_agents(&mut self.environment, count, self.mutation_rate);
                self.grid_cache.clear();
                Command::none()
            }
            Message::MutationRateChanged(rate) => {
                self.mutation_rate = rate;
                Command::none()
            }
            Message::ToggleDayNight => {
                self.environment.day_night_cycle = !self.environment.day_night_cycle;
                Command::none()
            }
            Message::HeuristicStrategySelected(strategy) => {
                self.selected_heuristic = strategy.clone();
                self.custom_parameters = strategy.get_parameters();
                Command::none()
            }
            Message::ParameterChanged(name, value) => {
                match name.as_str() {
                    "hunger" => self.custom_parameters.hunger_weight = (value * 100.0) as u8,
                    "thirst" => self.custom_parameters.thirst_weight = (value * 100.0) as u8,
                    "energy" => self.custom_parameters.energy_weight = (value * 100.0) as u8,
                    "exploration" => self.custom_parameters.exploration_weight = (value * 100.0) as u8,
                    "obstacle" => self.custom_parameters.obstacle_weight = (value * 100.0) as u8,
                    _ => {}
                }
                Command::none()
            }
            Message::ApplyCustomParameters => {
                self.selected_heuristic = HeuristicStrategy::Custom(self.custom_parameters.clone());
                Command::none()
            }
            Message::SelectAgent(id) => {
                self.selected_agent_id = Some(id);
                Command::none()
            }
        }
    }

    fn view(&self) -> Element<Message> {
        // Create the grid visualization
        let grid = canvas(self)
            .width(Length::Fixed(GRID_SIZE as f32 * CELL_SIZE))
            .height(Length::Fixed(GRID_SIZE as f32 * CELL_SIZE));
        
        // Control panel
        let play_pause_button = button(
            text(if self.running { "Pause" } else { "Play" })
                .horizontal_alignment(alignment::Horizontal::Center),
        )
        .on_press(Message::ToggleSimulation);
        
        let reset_button = button(
            text("Reset")
                .horizontal_alignment(alignment::Horizontal::Center),
        )
        .on_press(Message::ResetSimulation);
        
        let spawn_random_button = button(
            text("Spawn 5 Random")
                .horizontal_alignment(alignment::Horizontal::Center),
        )
        .on_press(Message::SpawnRandomAgents(5));
        
        let spawn_evolved_button = button(
            text("Spawn 5 Evolved")
                .horizontal_alignment(alignment::Horizontal::Center),
        )
        .on_press(Message::SpawnEvolvedAgents(5));
        
        let speed_slider = slider(
            1.0..=20.0,
            self.speed,
            Message::SpeedChanged,
        );
        
        let mutation_slider = slider(
            0.0..=0.5,
            self.mutation_rate,
            Message::MutationRateChanged,
        );
        
        let day_night_toggle = toggler(
            "Day/Night Cycle".to_string(),
            self.environment.day_night_cycle,
            |_| Message::ToggleDayNight,
        );
        
        // Heuristic selection
        let heuristic_picker = pick_list(
            HeuristicStrategy::all_strategies(),
            Some(self.selected_heuristic.clone()),
            Message::HeuristicStrategySelected,
        );
        
        // Custom parameters sliders
        let hunger_slider = slider(
            0..=100,
            self.custom_parameters.hunger_weight,
            |v| Message::ParameterChanged("hunger".to_string(), v as f32 / 100.0),
        );
        
        let thirst_slider = slider(
            0..=100,
            self.custom_parameters.thirst_weight,
            |v| Message::ParameterChanged("thirst".to_string(), v as f32 / 100.0),
        );
        
        let energy_slider = slider(
            0..=100,
            self.custom_parameters.energy_weight,
            |v| Message::ParameterChanged("energy".to_string(), v as f32 / 100.0),
        );
        
        let exploration_slider = slider(
            0..=100,
            self.custom_parameters.exploration_weight,
            |v| Message::ParameterChanged("exploration".to_string(), v as f32 / 100.0),
        );
        
        let obstacle_slider = slider(
            0..=100,
            self.custom_parameters.obstacle_weight,
            |v| Message::ParameterChanged("obstacle".to_string(), v as f32 / 100.0),
        );
        
        let apply_button = button(
            text("Apply Custom Parameters")
                .horizontal_alignment(alignment::Horizontal::Center),
        )
        .on_press(Message::ApplyCustomParameters);
        
        let add_agent_button = button(
            text("Add Agent with Strategy")
                .horizontal_alignment(alignment::Horizontal::Center),
        )
        .on_press(Message::AddAgent(self.selected_heuristic.clone()));
        
        // Statistics panel
        let stats = self.get_statistics_view();
        
        // Agent details panel
        let agent_details = if let Some(id) = self.selected_agent_id {
            self.get_agent_details_view(id)
        } else {
            column![
                text("No agent selected"),
            ]
            .spacing(5)
            .width(Length::Fill)
            .into()
        };
        
        // Layout
        let content = row![
            // Left panel - Grid
            container(grid).width(Length::Fixed(GRID_SIZE as f32 * CELL_SIZE + 20.0)),
            
            // Right panel - Controls and Stats
            scrollable(
                column![
                    // Simulation controls
                    text("Simulation Controls").size(24),
                    row![
                        play_pause_button,
                        reset_button,
                    ].spacing(10),
                    text(format!("Speed: {:.1}", self.speed)),
                    speed_slider,
                    day_night_toggle,
                    
                    // Agent controls
                    text("Agent Controls").size(24),
                    row![
                        spawn_random_button,
                        spawn_evolved_button,
                    ].spacing(10),
                    text(format!("Mutation Rate: {:.2}", self.mutation_rate)),
                    mutation_slider,
                    
                    // Heuristic controls
                    text("Heuristic Settings").size(24),
                    heuristic_picker,
                    text("Hunger Weight:"),
                    hunger_slider,
                    text("Thirst Weight:"),
                    thirst_slider,
                    text("Energy Weight:"),
                    energy_slider,
                    text("Exploration Weight:"),
                    exploration_slider,
                    text("Obstacle Avoidance Weight:"),
                    obstacle_slider,
                    apply_button,
                    add_agent_button,
                    
                    // Statistics
                    text("Statistics").size(24),
                    stats,
                    
                    // Agent details
                    text("Agent Details").size(24),
                    agent_details,
                ]
                .spacing(10)
                .padding(20)
                .width(Length::Fill)
            )
            .height(Length::Fill)
            .width(Length::Fill),
        ]
        .spacing(20)
        .padding(20);
        
        container(content)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .into()
    }
    
    fn subscription(&self) -> Subscription<Message> {
        if self.running {
            iced::subscription::unfold(
                0,
                self.speed,
                |speed| async move {
                    tokio::time::sleep(Duration::from_secs_f32(1.0 / speed)).await;
                    (Message::Tick, speed)
                },
            )
        } else {
            Subscription::none()
        }
    }
}

impl EcosystemApp {
    fn get_statistics_view(&self) -> Element<Message> {
        let population = self.agent_system.agents.len();
        let avg_lifespan = self.agent_system.get_average_lifespan();
        
        let population_stats = self.agent_system.get_population_stats();
        let strategy_stats = column(
            population_stats.iter().map(|(strategy, count)| {
                text(format!("{}: {}", strategy, count)).into()
            }).collect()
        )
        .spacing(5);
        
        column![
            text(format!("Tick: {}", self.tick_count)),
            text(format!("Population: {}", population)),
            text(format!("Average Lifespan: {:.1}", avg_lifespan)),
            text(format!("Environment: {}", if self.environment.is_day { "Day" } else { "Night" })),
            text("Strategy Distribution:"),
            strategy_stats,
        ]
        .spacing(5)
        .into()
    }
    
    fn get_agent_details_view(&self, agent_id: usize) -> Element<Message> {
        let agent = self.agent_system.agents.iter().find(|a| a.id == agent_id);
        
        if let Some(agent) = agent {
            column![
                text(format!("Agent ID: {}", agent.id)),
                text(format!("Position: ({}, {})", agent.position.x, agent.position.y)),
                text(format!("Health: {}", agent.health)),
                text(format!("Hunger: {}", agent.hunger)),
                text(format!("Thirst: {}", agent.thirst)),
                text(format!("Energy: {}", agent.energy)),
                text(format!("Age: {}", agent.age)),
                text(format!("Strategy: {}", agent.heuristic.name())),
            ]
            .spacing(5)
            .into()
        } else {
            text("Agent not found").into()
        }
    }
}

impl canvas::Program<Message> for EcosystemApp {
    type State = ();
    
    fn draw(
        &self,
        _state: &Self::State,
        renderer: &iced::Renderer,
        _theme: &Theme,
        bounds: iced::Rectangle,
        _cursor: iced::mouse::Cursor,
    ) -> Vec<canvas::Geometry> {
        let grid = self.grid_cache.draw(renderer, bounds.size(), |frame| {
            // Draw background
            let background_color = if self.environment.day_night_cycle && !self.environment.is_day {
                Color::from_rgb(0.1, 0.1, 0.3) // Night color
            } else {
                Color::from_rgb(0.9, 0.9, 0.9) // Day color
            };
            
            frame.fill_rectangle(
                iced::Point::ORIGIN,
                frame.size(),
                background_color,
            );
            
            // Draw grid cells
            for y in 0..GRID_SIZE {
                for x in 0..GRID_SIZE {
                    let cell_color = self.get_cell_color(x, y);
                    
                    frame.fill_rectangle(
                        iced::Point::new(x as f32 * CELL_SIZE, y as f32 * CELL_SIZE),
                        iced::Size::new(CELL_SIZE, CELL_SIZE),
                        cell_color,
                    );
                    
                    // Draw cell border
                    let border_path = Path::rectangle(
                        iced::Point::new(x as f32 * CELL_SIZE, y as f32 * CELL_SIZE),
                        iced::Size::new(CELL_SIZE, CELL_SIZE),
                    );
                    
                    frame.stroke(
                        &border_path,
                        Stroke {
                            width: 0.5,
                            style: iced::widget::canvas::stroke::Style::Solid(Color::from_rgba(0.0, 0.0, 0.0, 0.2)),
                            line_cap: iced::widget::canvas::stroke::LineCap::Round,
                            line_join: iced::widget::canvas::stroke::LineJoin::Round,
                            line_dash: iced::widget::canvas::stroke::LineDash::default(),
                        },
                    );
                }
            }
            
            // Highlight the selected agent's sensing radius if one is selected
            if let Some(id) = self.selected_agent_id {
                if let Some(agent) = self.agent_system.agents.iter().find(|a| a.id == id) {
                    let x = agent.position.x as f32 * CELL_SIZE + CELL_SIZE / 2.0;
                    let y = agent.position.y as f32 * CELL_SIZE + CELL_SIZE / 2.0;
                    let radius = SENSING_RADIUS as f32 * CELL_SIZE;
                    
                    // Draw a circle around the agent
                    let sensing_circle = Path::circle(iced::Point::new(x, y), radius);
                    
                    frame.stroke(
                        &sensing_circle,
                        Stroke {
                            width: 2.0,
                            style: iced::widget::canvas::stroke::Style::Solid(Color::from_rgba(1.0, 1.0, 0.0, 0.3)),
                            line_cap: iced::widget::canvas::stroke::LineCap::Round,
                            line_join: iced::widget::canvas::stroke::LineJoin::Round,
                            line_dash: iced::widget::canvas::stroke::LineDash::default(),
                        },
                    );
                }
            }
        });
        
        vec![grid]
    }
}

impl EcosystemApp {
    fn get_cell_color(&self, x: usize, y: usize) -> Color {
        let pos = Position::new(x, y);
        let entity = self.environment.get_entity_at(&pos);
        
        match entity {
            EntityType::Empty => {
                if self.environment.day_night_cycle && !self.environment.is_day {
                    Color::from_rgb(0.1, 0.1, 0.3) // Night empty color
                } else {
                    Color::from_rgb(0.9, 0.9, 0.9) // Day empty color
                }
            },
            EntityType::Food => Color::from_rgb(0.0, 0.8, 0.0),  // Green
            EntityType::Water => Color::from_rgb(0.0, 0.0, 0.8), // Blue
            EntityType::Obstacle => Color::from_rgb(0.5, 0.5, 0.5), // Gray
            EntityType::Agent(id) => {
                // Color agents based on their strategy or health
                let agent = self.agent_system.agents.iter().find(|a| a.id == id);
                
                if let Some(agent) = agent {
                    // If this is the selected agent, highlight it
                    if self.selected_agent_id == Some(id) {
                        return Color::from_rgb(1.0, 1.0, 0.0); // Yellow for selected agent
                    }
                    
                    // Color based on strategy
                    match agent.heuristic {
                        HeuristicStrategy::Survival => Color::from_rgb(1.0, 0.0, 0.0),  // Red
                        HeuristicStrategy::Explorer => Color::from_rgb(1.0, 0.5, 0.0),  // Orange
                        HeuristicStrategy::Efficient => Color::from_rgb(0.8, 0.0, 0.8), // Purple
                        HeuristicStrategy::Custom(_) => Color::from_rgb(0.0, 0.8, 0.8), // Cyan
                    }
                } else {
                    Color::from_rgb(0.5, 0.0, 0.0)  // Dark red for agents not found (should never happen)
                }
            }
        }
    }
}
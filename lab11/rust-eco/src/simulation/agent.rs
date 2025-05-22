use std::collections::HashMap;
use rand::{Rng, thread_rng};

use crate::simulation::{Position, EntityType, GRID_SIZE};
use crate::simulation::environment::Environment;
use crate::simulation::heuristics::{HeuristicStrategy, HeuristicSystem, HeuristicParameters};

#[derive(Debug, Clone)]
pub struct Agent {
    pub id: usize,
    pub position: Position,
    pub health: usize,         // 0-100
    pub hunger: usize,         // 0-100 (0 = full, 100 = starving)
    pub thirst: usize,         // 0-100 (0 = full, 100 = dehydrated)
    pub energy: usize,         // 0-100 (0 = exhausted, 100 = full energy)
    pub age: usize,            // Age in simulation ticks
    pub alive: bool,
    pub heuristic: HeuristicStrategy,
    pub visited_positions: HashMap<Position, usize>, // Tracks where the agent has been
}

impl Agent {
    pub fn new(id: usize, position: Position, heuristic: HeuristicStrategy) -> Self {
        Self {
            id,
            position,
            health: 100,
            hunger: 20,
            thirst: 20,
            energy: 100,
            age: 0,
            alive: true,
            heuristic,
            visited_positions: HashMap::new(),
        }
    }
    
    pub fn update(&mut self, env: &mut Environment) {
        if !self.alive {
            return;
        }
        
        // Track visited positions
        let visit_count = self.visited_positions.entry(self.position).or_insert(0);
        *visit_count += 1;
        
        // Age increases every tick
        self.age += 1;
        
        // Hunger and thirst increase over time
        self.hunger = (self.hunger + 1).min(100);
        self.thirst = (self.thirst + 2).min(100); // Thirst increases faster than hunger
        
        // Check current position for resources
        let current_entity = env.get_entity_at(&self.position);
        match current_entity {
            EntityType::Food => {
                self.hunger = (self.hunger as isize - 30).max(0) as usize;
                env.set_entity_at(&self.position, EntityType::Empty);
                env.set_entity_at(&self.position, EntityType::Agent(self.id));
            },
            EntityType::Water => {
                self.thirst = (self.thirst as isize - 40).max(0) as usize;
                env.set_entity_at(&self.position, EntityType::Empty);
                env.set_entity_at(&self.position, EntityType::Agent(self.id));
            },
            _ => {}
        }
        
        // Calculate health based on hunger and thirst
        let hunger_factor = 1.0 - (self.hunger as f32 / 100.0);
        let thirst_factor = 1.0 - (self.thirst as f32 / 100.0);
        let health_factor = (hunger_factor * 0.4 + thirst_factor * 0.6) * 100.0;
        self.health = health_factor as usize;
        
        // Die if health reaches 0
        if self.health == 0 {
            self.alive = false;
            env.set_entity_at(&self.position, EntityType::Empty);
            return;
        }
        
        // If low on energy, rest
        if self.energy < 10 {
            self.energy = (self.energy + 10).min(100);
            return;
        }
        
        // Choose a direction based on the heuristic
        let direction = HeuristicSystem::choose_direction(self, env);
        
        // Try to move in that direction
        if let Some(new_pos) = self.position.apply_direction(&direction) {
            if env.move_agent(self.id, &self.position, &new_pos) {
                self.position = new_pos;
                self.energy = (self.energy as isize - 5).max(0) as usize; // Moving costs energy
            }
        }
        
        // Energy regenerates based on health
        let energy_regen_rate = (self.health as f32 / 100.0 * 5.0) as usize;
        self.energy = (self.energy + energy_regen_rate).min(100);
    }
    
    pub fn random_position() -> Position {
        let mut rng = thread_rng();
        Position::new(
            rng.gen_range(0..GRID_SIZE),
            rng.gen_range(0..GRID_SIZE),
        )
    }
}

pub struct AgentSystem {
    pub agents: Vec<Agent>,
    pub next_id: usize,
    pub heuristic_success_metrics: HashMap<String, (usize, usize)>, // (total_lifespan, count)
}

impl AgentSystem {
    pub fn new() -> Self {
        Self {
            agents: Vec::new(),
            next_id: 0,
            heuristic_success_metrics: HashMap::new(),
        }
    }
    
    pub fn spawn_agent(&mut self, env: &mut Environment, heuristic: HeuristicStrategy) -> bool {
        let mut attempts = 0;
        const MAX_ATTEMPTS: usize = 100;
        
        while attempts < MAX_ATTEMPTS {
            let pos = Agent::random_position();
            if env.get_entity_at(&pos) == EntityType::Empty {
                let agent = Agent::new(self.next_id, pos, heuristic);
                env.set_entity_at(&pos, EntityType::Agent(self.next_id));
                self.agents.push(agent);
                self.next_id += 1;
                return true;
            }
            attempts += 1;
        }
        
        false // Failed to find an empty spot
    }
    
    pub fn update_all(&mut self, env: &mut Environment) {
        // First, update all agents
        for agent in &mut self.agents {
            agent.update(env);
        }
        
        // Then, handle dead agents
        let mut dead_agents = Vec::new();
        for (i, agent) in self.agents.iter().enumerate() {
            if !agent.alive {
                dead_agents.push(i);
                
                // Track heuristic success
                HeuristicSystem::track_heuristic_success(
                    &agent.heuristic,
                    agent.age,
                    &mut self.heuristic_success_metrics,
                );
            }
        }
        
        // Remove dead agents (in reverse order to maintain indices)
        for i in dead_agents.iter().rev() {
            self.agents.remove(*i);
        }
    }
    
    pub fn spawn_random_agents(&mut self, env: &mut Environment, count: usize) {
        let strategies = HeuristicStrategy::all_strategies();
        let mut rng = thread_rng();
        
        for _ in 0..count {
            let strategy = strategies[rng.gen_range(0..strategies.len())].clone();
            self.spawn_agent(env, strategy);
        }
    }
    
    pub fn spawn_evolved_agents(&mut self, env: &mut Environment, count: usize, mutation_rate: f32) {
        // Get the best performing strategies
        let stats = HeuristicSystem::get_strategy_stats(&self.heuristic_success_metrics);
        if stats.is_empty() {
            // If no stats yet, spawn random agents
            self.spawn_random_agents(env, count);
            return;
        }
        
        // Find the best performing strategies
        let mut best_strategies = Vec::new();
        for agent in &self.agents {
            best_strategies.push(agent.heuristic.clone());
        }
        
        // If we don't have enough living agents, supplement with some random strategies
        let strategies = HeuristicStrategy::all_strategies();
        let mut rng = thread_rng();
        while best_strategies.len() < 2 {
            let strategy = strategies[rng.gen_range(0..strategies.len())].clone();
            best_strategies.push(strategy);
        }
        
        // Spawn evolved agents
        for _ in 0..count {
            // Select two parent strategies randomly from the best ones
            let parent1_idx = rng.gen_range(0..best_strategies.len());
            let parent2_idx = rng.gen_range(0..best_strategies.len());
            let parent1 = &best_strategies[parent1_idx];
            let parent2 = &best_strategies[parent2_idx];
            
            // Breed their parameters to create a new custom strategy
            let parent1_params = parent1.get_parameters();
            let parent2_params = parent2.get_parameters();
            let child_params = HeuristicParameters::breed(&parent1_params, &parent2_params, mutation_rate);
            
            // Create a new agent with the evolved heuristic
            self.spawn_agent(env, HeuristicStrategy::Custom(child_params));
        }
    }
    
    pub fn get_population_stats(&self) -> HashMap<String, usize> {
        let mut stats = HashMap::new();
        
        // Count agents by strategy
        for agent in &self.agents {
            let strategy_name = agent.heuristic.name();
            let count = stats.entry(strategy_name).or_insert(0);
            *count += 1;
        }
        
        stats
    }
    
    pub fn get_average_lifespan(&self) -> f32 {
        let total_age: usize = self.agents.iter().map(|a| a.age).sum();
        let agent_count = self.agents.len();
        
        if agent_count > 0 {
            total_age as f32 / agent_count as f32
        } else {
            0.0
        }
    }
}
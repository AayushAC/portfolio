use iced::widget::{button, canvas, column, container, row, text};
use iced::widget::canvas::{Cursor, Frame, Geometry};
use iced::keyboard;
use iced::keyboard::Event as KeyboardEvent;
use iced::{
    executor, time, Application, Color, Command, Element, Length, Point, Rectangle, Settings,
    Size, Subscription, Theme,
};
use rand::Rng;
use std::collections::VecDeque;
use std::time::{Duration, Instant};

// ----------------------------------------------------------------
// Constants
// ----------------------------------------------------------------

const GRID_SIZE: usize = 20;
const CELL_SIZE: f32 = 30.0;
const WINDOW_SIZE: f32 = GRID_SIZE as f32 * CELL_SIZE;
const TICK_RATE: Duration = Duration::from_millis(150);

// ----------------------------------------------------------------
// Game types
// ----------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GameState {
    Menu,
    SinglePlayer,
    MultiPlayer,
    GameOver,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn is_opposite(self, other: Direction) -> bool {
        matches!(
            (self, other),
            (Direction::Up, Direction::Down)
                | (Direction::Down, Direction::Up)
                | (Direction::Left, Direction::Right)
                | (Direction::Right, Direction::Left)
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Position {
    x: i32,
    y: i32,
}

impl Position {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    fn next(self, dir: Direction) -> Self {
        let (x, y) = match dir {
            Direction::Up => (self.x, self.y - 1),
            Direction::Down => (self.x, self.y + 1),
            Direction::Left => (self.x - 1, self.y),
            Direction::Right => (self.x + 1, self.y),
        };
        Position::new(x, y)
    }

    fn wrap(self) -> Self {
        let x = (self.x + GRID_SIZE as i32) % GRID_SIZE as i32;
        let y = (self.y + GRID_SIZE as i32) % GRID_SIZE as i32;
        Position::new(x, y)
    }
}

#[derive(Debug, Clone)]
struct Snake {
    body: VecDeque<Position>,
    direction: Direction,
    next_direction: Direction,
    alive: bool,
    color: Color,
}

impl Snake {
    fn new(start: Position, dir: Direction, color: Color) -> Self {
        // build initial length-3 snake
        let mut body = VecDeque::new();
        body.push_back(start);
        for i in 1..3 {
            let tail = match dir {
                Direction::Up => Position::new(start.x, start.y + i),
                Direction::Down => Position::new(start.x, start.y - i),
                Direction::Left => Position::new(start.x + i, start.y),
                Direction::Right => Position::new(start.x - i, start.y),
            };
            body.push_back(tail);
        }
        Snake {
            body,
            direction: dir,
            next_direction: dir,
            alive: true,
            color,
        }
    }

    fn head(&self) -> Position {
        *self.body.front().unwrap()
    }

    fn set_direction(&mut self, dir: Direction) {
        if !self.direction.is_opposite(dir) {
            self.next_direction = dir;
        }
    }

    fn move_forward(&mut self, grow: bool) -> Position {
        self.direction = self.next_direction;
        let new_head = self.head().next(self.direction).wrap();
        self.body.push_front(new_head);
        if !grow {
            self.body.pop_back();
        }
        new_head
    }

    fn check_self_collision(&self) -> bool {
        let head = self.head();
        self.body.iter().skip(1).any(|&p| p == head)
    }

    fn check_other_collision(&self, other: &Snake) -> bool {
        let head = self.head();
        other.body.iter().any(|&p| p == head)
    }
}

// ----------------------------------------------------------------
// Messages
// ----------------------------------------------------------------

#[derive(Debug, Clone)]
enum Message {
    Tick(Instant),
    KeyPressed(keyboard::KeyCode),
    StartSingle,
    StartMulti,
    BackToMenu,
    Restart,
}

// ----------------------------------------------------------------
// App
// ----------------------------------------------------------------

struct SnakeGame {
    state: GameState,
    snake1: Snake,
    snake2: Option<Snake>,
    food: Position,
    score1: usize,
    score2: usize,
    high1: usize,
    high2: usize,
    last_update: Instant,
}

impl Application for SnakeGame {
    type Message = Message;
    type Executor = executor::Default;
    type Theme = Theme;
    type Flags = ();

    fn new(_: ()) -> (Self, Command<Message>) {
        let mid = Position::new((GRID_SIZE / 2) as i32, (GRID_SIZE / 2) as i32);
        (
            SnakeGame {
                state: GameState::Menu,
                snake1: Snake::new(
                    Position::new((GRID_SIZE / 4) as i32, (GRID_SIZE / 2) as i32),
                    Direction::Right,
                    Color::from_rgb(0.0, 0.8, 0.0),
                ),
                snake2: None,
                food: mid,
                score1: 0,
                score2: 0,
                high1: 0,
                high2: 0,
                last_update: Instant::now(),
            },
            Command::none(),
        )
    }

    fn title(&self) -> String {
        "Snake Game".into()
    }

    fn update(&mut self, msg: Message) -> Command<Message> {
        match msg {
            Message::Tick(now) => {
                if matches!(self.state, GameState::SinglePlayer | GameState::MultiPlayer)
                    && now.duration_since(self.last_update) >= TICK_RATE
                {
                    self.last_update = now;
                    self.update_game();
                }
            }
            Message::KeyPressed(code) => match self.state {
                GameState::SinglePlayer | GameState::MultiPlayer => {
                    self.handle_key(code)
                }
                GameState::GameOver => match code {
                    keyboard::KeyCode::R => self.restart_game(),
                    keyboard::KeyCode::M => self.state = GameState::Menu,
                    _ => {}
                },
                _ => {}
            },
            Message::StartSingle => self.init_single(),
            Message::StartMulti => self.init_multi(),
            Message::BackToMenu => self.state = GameState::Menu,
            Message::Restart => self.restart_game(),
        }
        Command::none()
    }

    fn subscription(&self) -> Subscription<Message> {
        Subscription::batch(vec![
            time::every(TICK_RATE).map(Message::Tick),
            keyboard::Event::subscription().map(|ev| {
                if let KeyboardEvent::KeyPressed { key_code, .. } = ev {
                    Message::KeyPressed(key_code)
                } else {
                    // ignore key releases
                    Message::Tick(self.last_update) // no-op
                }
            }),
        ])
    }

    fn view(&self) -> Element<Message> {
        match self.state {
            GameState::Menu => self.view_menu(),
            GameState::SinglePlayer | GameState::MultiPlayer => self.view_play(),
            GameState::GameOver => self.view_gameover(),
        }
    }
}

// ----------------------------------------------------------------
// SnakeGame impl
// ----------------------------------------------------------------

impl SnakeGame {
    fn init_single(&mut self) {
        self.state = GameState::SinglePlayer;
        self.snake2 = None;
        self.score1 = 0;
        self.place_food();
        self.last_update = Instant::now();
    }

    fn init_multi(&mut self) {
        self.state = GameState::MultiPlayer;
        self.snake2 = Some(Snake::new(
            Position::new((3 * GRID_SIZE / 4) as i32, (GRID_SIZE / 2) as i32),
            Direction::Left,
            Color::from_rgb(0.0, 0.0, 0.8),
        ));
        self.score1 = 0;
        self.score2 = 0;
        self.place_food();
        self.last_update = Instant::now();
    }

    fn restart_game(&mut self) {
        if self.snake2.is_some() {
            self.init_multi()
        } else {
            self.init_single()
        }
    }

    fn handle_key(&mut self, code: keyboard::KeyCode) {
        use keyboard::KeyCode::*;
        match code {
            W => self.snake1.set_direction(Direction::Up),
            A => self.snake1.set_direction(Direction::Left),
            S => self.snake1.set_direction(Direction::Down),
            D => self.snake1.set_direction(Direction::Right),
            Up => {
                if let Some(s2) = &mut self.snake2 {
                    s2.set_direction(Direction::Up)
                }
            }
            Left => {
                if let Some(s2) = &mut self.snake2 {
                    s2.set_direction(Direction::Left)
                }
            }
            Down => {
                if let Some(s2) = &mut self.snake2 {
                    s2.set_direction(Direction::Down)
                }
            }
            Right => {
                if let Some(s2) = &mut self.snake2 {
                    s2.set_direction(Direction::Right)
                }
            }
            _ => {}
        }
    }

    fn update_game(&mut self) {
        // 1) Move & record
        let ate1 = if self.snake1.alive {
            self.move_snake(&mut self.snake1)
        } else {
            false
        };
        let ate2 = if let Some(s2) = &mut self.snake2 {
            if s2.alive {
                self.move_snake(s2)
            } else {
                false
            }
        } else {
            false
        };

        // 2) Score & food
        if ate1 {
            self.score1 += 1;
        }
        if ate2 {
            self.score2 += 1;
        }
        if ate1 || ate2 {
            self.place_food();
        }

        // 3) Collisions
        if self.snake1.check_self_collision() {
            self.snake1.alive = false;
        }
        if let Some(s2) = &mut self.snake2 {
            if s2.check_self_collision() {
                s2.alive = false;
            }
            if self.snake1.check_other_collision(s2) {
                self.snake1.alive = false;
            }
            if s2.check_other_collision(&self.snake1) {
                s2.alive = false;
            }
        }

        // 4) Game over
        let over = match self.state {
            GameState::SinglePlayer => !self.snake1.alive,
            GameState::MultiPlayer => {
                !self.snake1.alive || !self.snake2.as_ref().unwrap().alive
            }
            _ => false,
        };
        if over {
            self.high1 = self.high1.max(self.score1);
            self.high2 = self.high2.max(self.score2);
            self.state = GameState::GameOver;
        }
    }

    fn move_snake(&mut self, snake: &mut Snake) -> bool {
        let head = snake.move_forward(false);
        let ate = head == self.food;
        if ate {
            let tail = *snake.body.back().unwrap();
            snake.body.push_back(tail);
        }
        ate
    }

    fn place_food(&mut self) {
        let mut rng = rand::thread_rng();
        let mut occupied: Vec<Position> =
            self.snake1.body.iter().copied().collect();
        if let Some(s2) = &self.snake2 {
            occupied.extend(s2.body.iter().copied())
        }
        loop {
            let candidate = Position::new(
                rng.gen_range(0..GRID_SIZE as i32),
                rng.gen_range(0..GRID_SIZE as i32),
            );
            if !occupied.contains(&candidate) {
                self.food = candidate;
                break;
            }
        }
    }

    // --- Views ---

    fn view_menu(&self) -> Element<Message> {
        let title = text("SNAKE GAME")
            .size(50)
            .style(iced::theme::Text::Color(Color::WHITE));

        let single = button("Single Player")
            .padding(20)
            .on_press(Message::StartSingle);
        let multi = button("Multi Player")
            .padding(20)
            .on_press(Message::StartMulti);

        let controls = column![
            text("Controls:").size(24).style(iced::theme::Text::Color(Color::WHITE)),
            text("Player 1: WASD").size(20).style(iced::theme::Text::Color(Color::WHITE)),
            text("Player 2: Arrow Keys").size(20).style(iced::theme::Text::Color(Color::WHITE)),
            text("Restart: R").size(20).style(iced::theme::Text::Color(Color::WHITE)),
            text("Menu: M").size(20).style(iced::theme::Text::Color(Color::WHITE)),
        ]
        .spacing(10)
        .padding(20);

        container(column![title, single, multi, controls].spacing(20).align_items(iced::Alignment::Center))
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .into()
    }

    fn view_play(&self) -> Element<Message> {
        let canvas = canvas::Canvas::new(self)
            .width(Length::Fill)
            .height(Length::Fill);

        let mode = match self.state {
            GameState::SinglePlayer => "Single Player",
            GameState::MultiPlayer => "Multiplayer",
            _ => "",
        };
        let score = if self.state == GameState::MultiPlayer {
            format!("P1: {}  P2: {}", self.score1, self.score2)
        } else {
            format!("Score: {}", self.score1)
        };
        let high = if self.state == GameState::MultiPlayer {
            format!("High P1: {}  High P2: {}", self.high1, self.high2)
        } else {
            format!("High Score: {}", self.high1)
        };

        let header = row![
            text(mode).size(24),
            text(score).size(24),
            text(high).size(24)
        ]
        .spacing(20)
        .padding(10);

        column![header, canvas].into()
    }

    fn view_gameover(&self) -> Element<Message> {
        let title = text("GAME OVER")
            .size(50)
            .style(iced::theme::Text::Color(Color::WHITE));

        let result = if self.state == GameState::SinglePlayer {
            if self.score1 >= self.high1 {
                format!("New High Score: {}", self.score1)
            } else {
                format!("Score: {}", self.score1)
            }
        } else {
            let s1 = self.snake1.alive;
            let s2 = self.snake2.as_ref().unwrap().alive;
            match (s1, s2) {
                (true, false) => "Player 1 Wins!".into(),
                (false, true) => "Player 2 Wins!".into(),
                _ => "Draw!".into(),
            }
        };

        let score = if self.state == GameState::MultiPlayer {
            format!("P1: {}  P2: {}", self.score1, self.score2)
        } else {
            format!("Score: {}", self.score1)
        };

        let restart = button("Restart").padding(20).on_press(Message::Restart);
        let menu = button("Back to Menu").padding(20).on_press(Message::BackToMenu);

        container(
            column![title, text(result).size(30), text(score).size(24), restart, menu]
                .spacing(20)
                .align_items(iced::Alignment::Center),
        )
        .width(Length::Fill)
        .height(Length::Fill)
        .center_x()
        .center_y()
        .into()
    }
}

// ----------------------------------------------------------------
// Canvas::Program impl
// ----------------------------------------------------------------

impl<Message> canvas::Program<Message> for SnakeGame {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &mut canvas::Renderer,
        _theme: &Theme,
        bounds: Rectangle,
        _cursor: Cursor,
    ) -> Vec<Geometry> {
        let mut frame = Frame::new(renderer, bounds.size());

        // Checkerboard
        for y in 0..GRID_SIZE {
            for x in 0..GRID_SIZE {
                let c = if (x + y) % 2 == 0 {
                    Color::from_rgb(0.1, 0.1, 0.1)
                } else {
                    Color::from_rgb(0.15, 0.15, 0.15)
                };
                frame.fill_rectangle(
                    Point::new(x as f32 * CELL_SIZE, y as f32 * CELL_SIZE),
                    Size::new(CELL_SIZE, CELL_SIZE),
                    c,
                );
            }
        }

        // Food
        frame.fill_rectangle(
            Point::new(self.food.x as f32 * CELL_SIZE, self.food.y as f32 * CELL_SIZE),
            Size::new(CELL_SIZE, CELL_SIZE),
            Color::from_rgb(0.8, 0.0, 0.0),
        );

        // Snake1
        for &pos in &self.snake1.body {
            frame.fill_rectangle(
                Point::new(pos.x as f32 * CELL_SIZE, pos.y as f32 * CELL_SIZE),
                Size::new(CELL_SIZE, CELL_SIZE),
                self.snake1.color,
            );
        }

        // Snake2
        if let Some(s2) = &self.snake2 {
            for &pos in &s2.body {
                frame.fill_rectangle(
                    Point::new(pos.x as f32 * CELL_SIZE, pos.y as f32 * CELL_SIZE),
                    Size::new(CELL_SIZE, CELL_SIZE),
                    s2.color,
                );
            }
        }

        vec![frame.into_geometry()]
    }
}

fn main() -> iced::Result {
    SnakeGame::run(Settings {
        window: iced::window::Settings {
            size: (WINDOW_SIZE as u32, WINDOW_SIZE as u32),
            ..Default::default()
        },
        ..Settings::default()
    })
}

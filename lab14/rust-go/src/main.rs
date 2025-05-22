// src/main.rs
mod board;
mod sgf;
mod ai;

use iced::{
    widget::{button, column, row, text, canvas},
    executor, Application, Command, Element, Length, Point, Rectangle, Settings, Size, Theme,
};
use std::path::Path;

use board::{Board, Color as StoneColor, Move, Position};
use sgf::parse_sgf;

pub fn main() -> iced::Result {
    GoGame::run(Settings::default())
}

struct GoGame {
    board: Board,
    current_player: StoneColor,
    message: String,
    last_move: Option<Move>,
    ai_enabled: bool,
}

#[derive(Debug, Clone)]
enum Message {
    PositionSelected(Position),
    Pass,
    LoadSGF,
    ToggleAI,
}

impl Application for GoGame {
    type Executor = executor::Default;
    type Message = Message;
    type Theme = Theme;
    type Flags = ();

    fn new(_flags: ()) -> (Self, Command<Message>) {
        // Default board size is 9x9 for simplicity
        let size = 9;
        (
            Self {
                board: Board::new(size),
                current_player: StoneColor::Black, // Black goes first
                message: String::from("Game started. Black's turn."),
                last_move: None,
                ai_enabled: false,
            },
            Command::none(),
        )
    }

    fn title(&self) -> String {
        String::from("Go Game")
    }

    fn update(&mut self, message: Message) -> Command<Message> {
        match message {
            Message::PositionSelected(pos) => {
                let move_attempt = Move::Place(pos);
                
                // Check if the move is legal
                if self.board.is_legal_move(move_attempt, self.current_player, self.last_move) {
                    // Make the move
                    self.board.make_move(move_attempt, self.current_player);
                    self.last_move = Some(move_attempt);
                    
                    // Switch player
                    self.current_player = self.current_player.opposite();
                    self.message = format!("{}'s turn", self.current_player);
                    
                    // If AI is enabled and it's the AI's turn (white)
                    if self.ai_enabled && self.current_player == StoneColor::White {
                        // Get AI move
                        if let Some(ai_move) = ai::get_move(&self.board, self.current_player, self.last_move) {
                            // Make AI move
                            self.board.make_move(ai_move, self.current_player);
                            self.last_move = Some(ai_move);
                            self.current_player = self.current_player.opposite();
                            self.message = format!("AI played. {}'s turn", self.current_player);
                        } else {
                            // AI passes
                            self.message = String::from("AI passes. Your turn.");
                            self.current_player = self.current_player.opposite();
                        }
                    }
                } else {
                    self.message = String::from("Illegal move! Try again.");
                }
            }
            Message::Pass => {
                // Implement pass functionality
                self.last_move = Some(Move::Pass);
                self.current_player = self.current_player.opposite();
                self.message = format!("{}'s turn", self.current_player);
                
                // If AI is enabled and it's the AI's turn
                if self.ai_enabled && self.current_player == StoneColor::White {
                    // Get AI move
                    if let Some(ai_move) = ai::get_move(&self.board, self.current_player, self.last_move) {
                        // Make AI move
                        self.board.make_move(ai_move, self.current_player);
                        self.last_move = Some(ai_move);
                        self.current_player = self.current_player.opposite();
                        self.message = format!("AI played. {}'s turn", self.current_player);
                    } else {
                        // AI passes
                        self.message = String::from("AI passes. Your turn.");
                        self.current_player = self.current_player.opposite();
                    }
                }
            }
            Message::LoadSGF => {
                // This is simplified - in a real app, you'd want a file picker
                // For simplicity, we'll assume an example.sgf file exists
                if let Ok(game_info) = parse_sgf(Path::new("example.sgf")) {
                    self.board = Board::from_sgf(&game_info);
                    self.current_player = game_info.next_player;
                    self.message = format!("SGF loaded. {}'s turn", self.current_player);
                    self.last_move = None;
                } else {
                    self.message = String::from("Failed to load SGF file.");
                }
            }
            Message::ToggleAI => {
                self.ai_enabled = !self.ai_enabled;
                self.message = if self.ai_enabled {
                    String::from("AI enabled. You play as Black.")
                } else {
                    String::from("AI disabled. Two player mode.")
                };
                
                // If it's the AI's turn after enabling
                if self.ai_enabled && self.current_player == StoneColor::White {
                    // Get AI move
                    if let Some(ai_move) = ai::get_move(&self.board, self.current_player, self.last_move) {
                        // Make AI move
                        self.board.make_move(ai_move, self.current_player);
                        self.last_move = Some(ai_move);
                        self.current_player = self.current_player.opposite();
                        self.message = format!("AI played. {}'s turn", self.current_player);
                    } else {
                        // AI passes
                        self.message = String::from("AI passes. Your turn.");
                        self.current_player = self.current_player.opposite();
                    }
                }
            }
        }
        Command::none()
    }

    fn view(&self) -> Element<Message> {
        let board_canvas = BoardCanvas {
            board: self.board.clone(),
            current_player: self.current_player,
        };
        
        let canvas = canvas(board_canvas)
            .width(Length::Fill)
            .height(Length::Fill);
        
        let status = text(&self.message);
        
        let pass_button = button("Pass")
            .on_press(Message::Pass);
        
        let load_sgf_button = button("Load SGF")
            .on_press(Message::LoadSGF);
        
        let ai_button = button(if self.ai_enabled { "Disable AI" } else { "Enable AI" })
            .on_press(Message::ToggleAI);
        
        let controls = row![pass_button, load_sgf_button, ai_button].spacing(10);
        
        column![
            status,
            canvas,
            controls,
        ]
        .spacing(20)
        .padding(20)
        .into()
    }
}

struct BoardCanvas {
    board: Board,
    current_player: StoneColor,
}

impl canvas::Program<Message> for BoardCanvas {
    type State = ();
    
    fn draw(
        &self,
        _state: &Self::State,
        _theme: &Theme,
        bounds: Rectangle,
        _cursor: canvas::Cursor,
    ) -> Vec<canvas::Geometry> {
        let board_size = self.board.size();
        let cell_size = (bounds.width.min(bounds.height) / board_size as f32).floor();
        let board_width = cell_size * board_size as f32;
        let board_height = cell_size * board_size as f32;
        
        let offset_x = (bounds.width - board_width) / 2.0;
        let offset_y = (bounds.height - board_height) / 2.0;
        
        // Create a frame to draw on
        let mut frame = canvas::Frame::new(bounds.size());
        
        // Draw board background
        frame.fill_rectangle(
            Point::new(offset_x, offset_y),
            Size::new(board_width, board_height),
            iced::Color::from_rgb(0.8, 0.6, 0.2),
        );
        
        // Draw grid lines
        for i in 0..board_size {
            let x = offset_x + i as f32 * cell_size;
            let y = offset_y + i as f32 * cell_size;
            
            // Horizontal line
            frame.stroke(
                &canvas::Path::line(
                    Point::new(offset_x, y),
                    Point::new(offset_x + board_width, y),
                ),
                canvas::Stroke::default(),
            );
            
            // Vertical line
            frame.stroke(
                &canvas::Path::line(
                    Point::new(x, offset_y),
                    Point::new(x, offset_y + board_height),
                ),
                canvas::Stroke::default(),
            );
        }
        
        // Draw stones
        for row in 0..board_size {
            for col in 0..board_size {
                let pos = Position::new(row, col);
                if let Some(stone) = self.board.get(pos) {
                    let x = offset_x + col as f32 * cell_size;
                    let y = offset_y + row as f32 * cell_size;
                    let stone_path = canvas::Path::circle(
                        Point::new(x, y),
                        cell_size / 2.0 * 0.8,
                    );
                    
                    let stone_color = match stone {
                        StoneColor::Black => iced::Color::BLACK,
                        StoneColor::White => iced::Color::WHITE,
                    };
                    
                    frame.fill(&stone_path, stone_color);
                    
                    // Add a stroke around white stones to make them more visible
                    if stone == StoneColor::White {
                        frame.stroke(
                            &stone_path,
                            canvas::Stroke {
                                width: 1.0,
                                ..canvas::Stroke::default()
                            },
                        );
                    }
                }
            }
        }
        
        vec![frame.into_geometry()]
    }
    
    fn mouse_interaction(
        &self,
        _state: &Self::State,
        _bounds: Rectangle,
        _cursor: canvas::Cursor,
    ) -> iced::mouse::Interaction {
        iced::mouse::Interaction::Pointer
    }
    
    fn update(
        &self,
        _state: &mut Self::State,
        event: canvas::Event,
        bounds: Rectangle,
        cursor: canvas::Cursor,
    ) -> (canvas::event::Status, Option<Message>) {
        if let canvas::Event::Mouse(iced::mouse::Event::ButtonPressed(
            iced::mouse::Button::Left,
        )) = event
        {
            if let Some(position) = cursor.position_in(&bounds) {
                let board_size = self.board.size();
                let cell_size = (bounds.width.min(bounds.height) / board_size as f32).floor();
                let board_width = cell_size * board_size as f32;
                let board_height = cell_size * board_size as f32;
                
                let offset_x = (bounds.width - board_width) / 2.0;
                let offset_y = (bounds.height - board_height) / 2.0;
                
                // Calculate the board position
                let board_x = ((position.x - offset_x) / cell_size).floor() as usize;
                let board_y = ((position.y - offset_y) / cell_size).floor() as usize;
                
                // Check if the click is within the board
                if board_x < board_size && board_y < board_size {
                    return (
                        canvas::event::Status::Captured,
                        Some(Message::PositionSelected(Position::new(board_y, board_x))),
                    );
                }
            }
        }
        
        (canvas::event::Status::Ignored, None)
    }
}
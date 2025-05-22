// src/board.rs
use std::collections::{HashSet, VecDeque};
use std::fmt;

use crate::sgf::GameInfo;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Position {
    pub row: usize,
    pub col: usize,
}

impl Position {
    pub fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }
    
    // Get adjacent positions (up, down, left, right)
    pub fn adjacent(&self, board_size: usize) -> Vec<Position> {
        let mut positions = Vec::new();
        
        // Up
        if self.row > 0 {
            positions.push(Position::new(self.row - 1, self.col));
        }
        
        // Down
        if self.row < board_size - 1 {
            positions.push(Position::new(self.row + 1, self.col));
        }
        
        // Left
        if self.col > 0 {
            positions.push(Position::new(self.row, self.col - 1));
        }
        
        // Right
        if self.col < board_size - 1 {
            positions.push(Position::new(self.row, self.col + 1));
        }
        
        positions
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Color {
    Black,
    White,
}

impl Color {
    pub fn opposite(&self) -> Self {
        match self {
            Color::Black => Color::White,
            Color::White => Color::Black,
        }
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Color::Black => write!(f, "Black"),
            Color::White => write!(f, "White"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Move {
    Place(Position),
    Pass,
}

#[derive(Clone, Debug)]
pub struct Board {
    size: usize,
    grid: Vec<Vec<Option<Color>>>,
    captured_black: usize,
    captured_white: usize,
    previous_state: Option<Vec<Vec<Option<Color>>>>, // For KO rule
}

impl Board {
    pub fn new(size: usize) -> Self {
        let grid = vec![vec![None; size]; size];
        
        Self {
            size,
            grid,
            captured_black: 0,
            captured_white: 0,
            previous_state: None,
        }
    }
    
    pub fn from_sgf(game_info: &GameInfo) -> Self {
        let mut board = Self::new(game_info.board_size);
        
        // Apply initial setup if any
        for (pos, color) in &game_info.initial_setup {
            board.grid[pos.row][pos.col] = Some(*color);
        }
        
        // Apply moves if needed
        for &(pos, color) in &game_info.moves {
            match pos {
                Some(position) => board.make_move(Move::Place(position), color),
                None => board.make_move(Move::Pass, color),
            }
        }
        
        board
    }
    
    pub fn size(&self) -> usize {
        self.size
    }
    
    pub fn get(&self, pos: Position) -> Option<Color> {
        if pos.row < self.size && pos.col < self.size {
            self.grid[pos.row][pos.col]
        } else {
            None
        }
    }
    
    // Check if a move is legal
    pub fn is_legal_move(&self, mv: Move, color: Color, _last_move: Option<Move>) -> bool {
        match mv {
            Move::Place(pos) => {
                // Check if position is on the board
                if pos.row >= self.size || pos.col >= self.size {
                    return false;
                }
                
                // Check if position is already occupied
                if self.grid[pos.row][pos.col].is_some() {
                    return false;
                }
                
                // Make a temporary copy of the board to check liberty rule and KO rule
                let mut temp_board = self.clone();
                temp_board.grid[pos.row][pos.col] = Some(color);
                
                // Check if move creates a stone or group with no liberties (suicide move)
                let group = temp_board.find_group(pos);
                if temp_board.count_liberties(&group) == 0 {
                    // Check if this move captures opponent stones
                    let mut captures_opponent = false;
                    for adj_pos in pos.adjacent(self.size) {
                        if let Some(adj_color) = self.get(adj_pos) {
                            if adj_color == color.opposite() {
                                let adj_group = temp_board.find_group(adj_pos);
                                if temp_board.count_liberties(&adj_group) == 0 {
                                    captures_opponent = true;
                                    break;
                                }
                            }
                        }
                    }
                    
                    if !captures_opponent {
                        return false; // Suicide move not allowed
                    }
                }
                
                // Check KO rule
                if let Some(prev_state) = &self.previous_state {
                    // Capture any stones that would be captured by this move
                    let mut new_board = self.clone();
                    new_board.grid[pos.row][pos.col] = Some(color);
                    new_board.capture_stones(pos);
                    
                    // Check if new board state matches the previous state
                    if new_board.grid == *prev_state {
                        return false; // Violates KO rule
                    }
                }
                
                true
            }
            Move::Pass => true, // Passing is always legal
        }
    }
    
    // Make a move and update the board state
    pub fn make_move(&mut self, mv: Move, color: Color) {
        match mv {
            Move::Place(pos) => {
                // Save previous state for KO rule
                self.previous_state = Some(self.grid.clone());
                
                // Place the stone
                self.grid[pos.row][pos.col] = Some(color);
                
                // Capture opponent's stones if necessary
                self.capture_stones(pos);
            }
            Move::Pass => {
                // No changes to the board for a pass
            }
        }
    }
    
    // Find all connected stones of the same color
    fn find_group(&self, start: Position) -> HashSet<Position> {
        let start_color = self.get(start);
        if start_color.is_none() {
            return HashSet::new();
        }
        
        let mut group = HashSet::new();
        let mut queue = VecDeque::new();
        
        queue.push_back(start);
        group.insert(start);
        
        while let Some(pos) = queue.pop_front() {
            for adj_pos in pos.adjacent(self.size) {
                if !group.contains(&adj_pos) && self.get(adj_pos) == start_color {
                    group.insert(adj_pos);
                    queue.push_back(adj_pos);
                }
            }
        }
        
        group
    }
    
    // Count liberties of a group of stones
    fn count_liberties(&self, group: &HashSet<Position>) -> usize {
        let mut liberties = HashSet::new();
        
        for &pos in group {
            for adj_pos in pos.adjacent(self.size) {
                if self.get(adj_pos).is_none() {
                    liberties.insert(adj_pos);
                }
            }
        }
        
        liberties.len()
    }
    
    // Capture stones after a move
    fn capture_stones(&mut self, last_move_pos: Position) {
        let last_move_color = self.get(last_move_pos).unwrap();
        let opponent_color = last_move_color.opposite();
        
        // Check adjacent positions for potential captures
        for adj_pos in last_move_pos.adjacent(self.size) {
            if self.get(adj_pos) == Some(opponent_color) {
                let group = self.find_group(adj_pos);
                if self.count_liberties(&group) == 0 {
                    // Capture the group
                    let group_size = group.len();
                    for pos in &group {
                        self.grid[pos.row][pos.col] = None;
                    }
                    
                    // Update capture count
                    match opponent_color {
                        Color::Black => self.captured_black += group_size,
                        Color::White => self.captured_white += group_size,
                    }
                }
            }
        }
    }
    
    // Get the number of captured stones
    pub fn captured(&self, color: Color) -> usize {
        match color {
            Color::Black => self.captured_black,
            Color::White => self.captured_white,
        }
    }
}
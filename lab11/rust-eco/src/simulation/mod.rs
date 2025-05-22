pub mod environment;
pub mod agent;
pub mod heuristics;

// Shared types and constants
pub const GRID_SIZE: usize = 50; // A smaller grid for better performance
pub const SENSING_RADIUS: usize = 10;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EntityType {
    Empty,
    Food,
    Water,
    Obstacle,
    Agent(usize), // Agent with ID
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    pub fn to_delta(&self) -> (isize, isize) {
        match self {
            Direction::North => (0, -1),
            Direction::East => (1, 0),
            Direction::South => (0, 1),
            Direction::West => (-1, 0),
        }
    }
    
    pub fn all() -> Vec<Direction> {
        vec![Direction::North, Direction::East, Direction::South, Direction::West]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub x: usize,
    pub y: usize,
}

impl Position {
    pub fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }
    
    pub fn distance(&self, other: &Position) -> f32 {
        let dx = self.x as isize - other.x as isize;
        let dy = self.y as isize - other.y as isize;
        ((dx * dx + dy * dy) as f32).sqrt()
    }
    
    pub fn apply_direction(&self, dir: &Direction) -> Option<Position> {
        let (dx, dy) = dir.to_delta();
        
        let new_x = self.x as isize + dx;
        let new_y = self.y as isize + dy;
        
        if new_x >= 0 && new_x < GRID_SIZE as isize && new_y >= 0 && new_y < GRID_SIZE as isize {
            Some(Position::new(new_x as usize, new_y as usize))
        } else {
            None
        }
    }
}
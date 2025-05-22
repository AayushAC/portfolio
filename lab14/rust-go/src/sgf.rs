// src/sgf.rs
use std::fs::File;
use std::io::Read;
use std::path::Path;
use nom::{
    IResult,
    bytes::complete::{tag, take_until},
    character::complete::{char, alpha1},
    branch::alt,
    multi::many0,
    sequence::delimited,
    combinator::value,
};
use crate::board::{Color, Position};

#[derive(Debug)]
pub struct GameInfo {
    pub board_size: usize,
    pub initial_setup: Vec<(Position, Color)>,
    pub moves: Vec<(Option<Position>, Color)>, // None represents a pass
    pub next_player: Color,
}

// Parse an SGF file and extract game information
pub fn parse_sgf(path: &Path) -> Result<GameInfo, std::io::Error> {
    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    
    match parse_sgf_content(&content) {
        Ok((_, game_info)) => Ok(game_info),
        Err(_) => Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Failed to parse SGF file")),
    }
}

// Parse SGF content
fn parse_sgf_content(input: &str) -> IResult<&str, GameInfo> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = tag(";")(input)?;
    
    // Parse properties
    let (input, properties) = many0(parse_property)(input)?;
    
    // Extract board size
    let board_size = properties.iter()
        .find_map(|(key, value)| {
            if *key == "SZ" {
                value.parse::<usize>().ok()
            } else {
                None
            }
        })
        .unwrap_or(19); // Default to 19x19
    
    // Extract initial setup
    let mut initial_setup = Vec::new();
    
    // Add black stones (AB property)
    for (key, value) in &properties {
        if *key == "AB" {
            if let Some(pos) = parse_position(value, board_size) {
                initial_setup.push((pos, Color::Black));
            }
        } else if *key == "AW" {
            if let Some(pos) = parse_position(value, board_size) {
                initial_setup.push((pos, Color::White));
            }
        }
    }
    
    // Parse moves
    let (input, moves) = many0(parse_move)(input)?;
    
    // Convert moves to our format
    let moves: Vec<(Option<Position>, Color)> = moves.into_iter()
        .map(|(color, pos_str)| {
            if pos_str.is_empty() {
                // This is a pass
                (None, color)
            } else {
                // This is a regular move
                (parse_position(pos_str, board_size), color)
            }
        })
        .collect();
    
    // Determine next player based on the last move
    let next_player = if let Some((_, last_color)) = moves.last() {
        last_color.opposite()
    } else {
        // Black starts by default
        Color::Black
    };
    
    Ok((input, GameInfo {
        board_size,
        initial_setup,
        moves,
        next_player,
    }))
}

// Parse a property (key and value)
fn parse_property(input: &str) -> IResult<&str, (&str, &str)> {
    let (input, key) = alpha1(input)?;
    let (input, value) = delimited(
        char('['),
        take_until("]"),
        char(']')
    )(input)?;
    
    Ok((input, (key, value)))
}

// Parse a move
fn parse_move(input: &str) -> IResult<&str, (Color, &str)> {
    let (input, _) = tag(";")(input)?;
    
    let (input, move_type) = alt((
        value(Color::Black, tag("B")),
        value(Color::White, tag("W")),
    ))(input)?;
    
    let (input, move_pos) = delimited(
        char('['),
        take_until("]"),
        char(']')
    )(input)?;
    
    Ok((input, (move_type, move_pos)))
}

// Convert SGF coordinates to Position
fn parse_position(pos_str: &str, board_size: usize) -> Option<Position> {
    if pos_str.len() < 2 {
        return None;
    }
    
    let col = pos_str.chars().nth(0)? as usize - 'a' as usize;
    let row = pos_str.chars().nth(1)? as usize - 'a' as usize;
    
    if col < board_size && row < board_size {
        Some(Position::new(row, col))
    } else {
        None
    }
}

// Basic SGF file generator for testing
pub fn generate_test_sgf() -> String {
    // Create a simple 9x9 game with a few moves
    let sgf = r#"(;GM[1]FF[4]CA[UTF-8]AP[CGoban:3]ST[2]
RU[Japanese]SZ[9]KM[6.50]
PW[White]PB[Black]
;B[ee]
;W[ge]
;B[fg]
;W[dc]
;B[cc]
;W[dd]
;B[cd]
)"#;
    
    sgf.to_string()
}
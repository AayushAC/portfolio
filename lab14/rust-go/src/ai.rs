// src/ai.rs
use rand::Rng;
use std::collections::HashSet;
use crate::board::{Board, Color, Move, Position};

// Get a move for the AI player
pub fn get_move(board: &Board, color: Color, last_move: Option<Move>) -> Option<Move> {
    let board_size = board.size();
    let mut rng = rand::thread_rng();
    
    // 20% chance of random move
    if rng.gen_bool(0.2) {
        return get_random_move(board, color, last_move);
    }
    
    // 80% chance of strategic move
    let mut best_move: Option<Position> = None;
    let mut best_score = -1.0;
    
    // Try all possible moves and evaluate them
    for row in 0..board_size {
        for col in 0..board_size {
            let pos = Position::new(row, col);
            
            if board.is_legal_move(Move::Place(pos), color, last_move) {
                let score = evaluate_move(board, pos, color);
                
                if score > best_score {
                    best_score = score;
                    best_move = Some(pos);
                }
            }
        }
    }
    
    if let Some(pos) = best_move {
        Some(Move::Place(pos))
    } else {
        // No good moves found, try a random move as fallback
        get_random_move(board, color, last_move)
    }
}

// Get a random legal move
fn get_random_move(board: &Board, color: Color, last_move: Option<Move>) -> Option<Move> {
    let board_size = board.size();
    let mut rng = rand::thread_rng();
    
    // Collect all legal moves
    let mut legal_moves = Vec::new();
    
    for row in 0..board_size {
        for col in 0..board_size {
            let pos = Position::new(row, col);
            
            if board.is_legal_move(Move::Place(pos), color, last_move) {
                legal_moves.push(pos);
            }
        }
    }
    
    // If there are legal moves, choose a random one
    if !legal_moves.is_empty() {
        let random_index = rng.gen_range(0..legal_moves.len());
        return Some(Move::Place(legal_moves[random_index]));
    }
    
    // No legal moves, pass
    Some(Move::Pass)
}

// Evaluate a potential move
fn evaluate_move(board: &Board, pos: Position, color: Color) -> f32 {
    let board_size = board.size();
    let mut score = 0.0;
    
    // Simulate making the move
    let mut temp_board = board.clone();
    temp_board.make_move(Move::Place(pos), color);
    
    // Check if this move captures opponent stones
    let opponent_color = color.opposite();
    let mut captured_count = 0;
    
    for adj_pos in pos.adjacent(board_size) {
        if board.get(adj_pos) == Some(opponent_color) {
            let group = find_group(&temp_board, adj_pos);
            if count_liberties(&temp_board, &group) == 0 {
                captured_count += group.len();
            }
        }
    }
    
    // Bonus for capturing stones
    score += captured_count as f32 * 1.5;
    
    // Check liberties of this stone/group
    let group = find_group(&temp_board, pos);
    let liberties = count_liberties(&temp_board, &group);
    
    // Bonus for having more liberties
    score += liberties as f32;
    
    // Bonus for connecting to other groups
    let connected_groups = count_connected_groups(&temp_board, pos, color);
    score += connected_groups as f32 * 0.5;
    
    // Penalty for creating multiple groups
    let group_count = count_player_groups(&temp_board, color);
    score -= group_count as f32 * 0.3;
    
    // Prefer to play near the center in the opening
    let center_row = board_size as f32 / 2.0;
    let center_col = board_size as f32 / 2.0;
    let distance_from_center = ((pos.row as f32 - center_row).powi(2) + 
                              (pos.col as f32 - center_col).powi(2)).sqrt();
    
    // Only apply center preference in the early game
    let total_stones = count_total_stones(board);
    if total_stones < board_size * 3 / 2 {
        score -= distance_from_center * 0.2;
    }
    
    score
}

// Find all connected stones of the same color
fn find_group(board: &Board, start: Position) -> HashSet<Position> {
    let mut group = HashSet::new();
    let mut to_visit = vec![start];
    let start_color = board.get(start);
    
    if start_color.is_none() {
        return group;
    }
    
    while let Some(pos) = to_visit.pop() {
        if group.contains(&pos) {
            continue;
        }
        
        group.insert(pos);
        
        for adj_pos in pos.adjacent(board.size()) {
            if !group.contains(&adj_pos) && board.get(adj_pos) == start_color {
                to_visit.push(adj_pos);
            }
        }
    }
    
    group
}

// Count liberties of a group
fn count_liberties(board: &Board, group: &HashSet<Position>) -> usize {
    let mut liberties = HashSet::new();
    
    for &pos in group {
        for adj_pos in pos.adjacent(board.size()) {
            if board.get(adj_pos).is_none() {
                liberties.insert(adj_pos);
            }
        }
    }
    
    liberties.len()
}

// Count the number of connected groups for a position
fn count_connected_groups(board: &Board, pos: Position, color: Color) -> usize {
    let mut connected_groups = 0;
    let mut checked_positions = HashSet::new();
    
    for adj_pos in pos.adjacent(board.size()) {
        if board.get(adj_pos) == Some(color) && !checked_positions.contains(&adj_pos) {
            let group = find_group(board, adj_pos);
            checked_positions.extend(group);
            connected_groups += 1;
        }
    }
    
    connected_groups
}

// Count the total number of groups for a player
fn count_player_groups(board: &Board, color: Color) -> usize {
    let board_size = board.size();
    let mut visited = HashSet::new();
    let mut group_count = 0;
    
    for row in 0..board_size {
        for col in 0..board_size {
            let pos = Position::new(row, col);
            
            if board.get(pos) == Some(color) && !visited.contains(&pos) {
                let group = find_group(board, pos);
                visited.extend(group);
                group_count += 1;
            }
        }
    }
    
    group_count
}

// Count the total number of stones on the board
fn count_total_stones(board: &Board) -> usize {
    let board_size = board.size();
    let mut count = 0;
    
    for row in 0..board_size {
        for col in 0..board_size {
            let pos = Position::new(row, col);
            if board.get(pos).is_some() {
                count += 1;
            }
        }
    }
    
    count
}
# Go Game Implementation in Rust

## Overview

This project implements the ancient board game of Go (also known as Weiqi in China or Baduk in Korea) in Rust, using the Iced GUI library. The implementation focuses on providing a clean, functional game engine that follows standard Go rules, including the liberty rule and KO rule. It features the ability to read Smart Game Format (SGF) files, maintain internal game state, and play against a basic AI opponent.

Go is one of the oldest board games in the world, originating in China over 2,500 years ago. Despite its simple rules, the game offers profound strategic complexity, making it an interesting challenge for both players and AI developers.

## Table of Contents

- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
- [Features](#features)
- [Project Structure](#project-structure)
- [Requirements Fulfillment](#requirements-fulfillment)
- [Game Rules](#game-rules)
- [Implementation Details](#implementation-details)
- [AI Strategy](#ai-strategy)
- [Limitations](#limitations)
- [Future Improvements](#future-improvements)
- [Development Process](#development-process)
- [Technical Challenges](#technical-challenges)
- [Performance Considerations](#performance-considerations)

## Requirements

- Rust (1.56.0 or newer)
- Cargo (bundled with Rust)
- Dependencies (automatically managed by Cargo):
  - Iced 0.9.0 (with canvas and debug features)
  - nom 7.1.3 (for SGF parsing)
  - rand 0.8.5 (for AI randomization)

## Installation

1. Clone the repository:
   ```
   git clone https://github.com/yourusername/go-game.git
   cd go-game
   ```

2. Build the project:
   ```
   cargo build
   ```

3. Run the game:
   ```
   cargo run
   ```

## Usage

After launching the application, you will be presented with a Go board interface:

- **Placing Stones**: Click on any intersection to place a stone
- **Pass**: Click the "Pass" button to skip your turn
- **Load SGF**: Click the "Load SGF" button to load a game from the example.sgf file
- **Toggle AI**: Click "Enable AI" to play against the computer (you play as Black)

The status message at the top of the window provides feedback about the current player's turn, illegal moves, and game state changes.

Note: The current implementation does not include scoring or game end detection, so games must be concluded by mutual agreement.

## Features

- Interactive game board with an intuitive user interface
- SGF file parsing and loading capability
- Complete implementation of core Go rules:
  - Liberty rule (no suicide moves unless capturing)
  - KO rule (preventing repetition of board state)
  - Capturing mechanics
- Support for various board sizes (9x9, 13x13, 19x19) as defined in loaded SGF files
- Basic AI opponent with strategic move evaluation
- Clean, modular code structure for maintainability and extensibility

## Project Structure

```
go-game/
├── Cargo.toml          # Project configuration and dependencies
├── src/
│   ├── main.rs         # Main application and GUI implementation
│   ├── board.rs        # Game state representation and rules
│   ├── sgf.rs          # SGF file parser
│   └── ai.rs           # AI opponent implementation
└── example.sgf         # Example SGF file for testing
```

### Module Responsibilities

- **main.rs**: Implements the Iced GUI application, handles user input, and orchestrates the game flow
- **board.rs**: Manages the game state, implements game rules, and provides move validation
- **sgf.rs**: Parses SGF files and extracts game information (board size, moves, initial setup)
- **ai.rs**: Implements the computer opponent with move evaluation strategies

## Requirements Fulfillment

This implementation satisfies all requirements specified in the Lab 14 assignment:

### 1. SGF File Reader

The `sgf.rs` module implements a comprehensive parser for Smart Game Format files using the nom parser combinator library. It successfully extracts:

- **Board size**: Default is 19x19, but the parser supports any size specified in the SGF file
- **Initial game state**: The parser identifies and applies AB (Add Black) and AW (Add White) properties to set up the initial board configuration
- **Move sequence**: All moves in the SGF file are parsed and can be replayed on the board

The parser is robust enough to handle common SGF file properties and is extensible for future enhancements.

Implementation highlights:
```rust
// Parse SGF content
fn parse_sgf_content(input: &str) -> IResult {
    // Extract properties like board size, player information
    // Parse initial stone placements (AB, AW properties)
    // Extract the sequence of moves
    // Determine next player based on the last move
}
```

### 2. Internal Game State Representation

The `board.rs` module implements a comprehensive and efficient game state representation:

- **Board grid**: A 2D vector of optional stone colors (`Vec<Vec<Option<Color>>>`)
- **Captured stone tracking**: Counters for stones captured by each player
- **Previous state storage**: Maintains the previous board state for KO rule validation
- **Board size flexibility**: Supports different board sizes

The representation facilitates quick lookup of stone positions, liberty counting, and group detection, which are critical operations for Go game logic.

Key implementation aspects:
```rust
pub struct Board {
    size: usize,
    grid: Vec<Vec<Option>>,
    captured_black: usize,
    captured_white: usize,
    previous_state: Option<Vec<Vec<Option>>>, // For KO rule
}
```

### 3. Move Validation

The `Board::is_legal_move` method in `board.rs` implements thorough rule validation:

- **Boundary validation**: Ensures moves are within the board dimensions
- **Occupation check**: Prevents placing stones on already occupied intersections
- **Liberty rule**: Validates that a stone or group must have at least one liberty (adjacent empty intersection) unless the move captures opponent stones
- **KO rule**: Prevents recreating the board position from the previous move
- **Suicide prevention**: Disallows moves that would result in self-capture without capturing opponent stones

The implementation provides clear feedback when moves are illegal, enhancing the user experience.

The validation process:
```rust
pub fn is_legal_move(&self, mv: Move, color: Color, _last_move: Option) -> bool {
    // Check if position is on the board
    // Check if position is already occupied
    // Check for suicide move (no liberties after placement)
    // Check if the move captures opponent stones (valid even if otherwise suicidal)
    // Check KO rule (prevent board state repetition)
}
```

### 4. Basic AI Player

The `ai.rs` module implements a strategic opponent with a balanced approach:

- **Dual strategy approach**:
  - Random moves (20% of the time) for unpredictability
  - Strategic moves (80% of the time) based on heuristic evaluation

- **Move evaluation criteria**:
  - Liberation maximization: Prioritizes moves that create or maintain more liberties
  - Capture value: Highly values moves that capture opponent stones
  - Group connectivity: Prefers moves that connect separate groups
  - Group count minimization: Aims to reduce the number of isolated groups
  - Center preference: In the opening phase, slightly favors center positions

- **Complete rule compliance**: The AI respects all Go rules, including KO and liberty rules
- **Adaptive strategy**: Evaluates the entire board for each move, adapting to the current game state

Implementation highlight:
```rust
fn evaluate_move(board: &Board, pos: Position, color: Color) -> f32 {
    // Simulate the move on a temporary board
    // Calculate capture value (bonus for capturing opponent stones)
    // Evaluate liberties for the played stone/group
    // Check group connectivity (bonus for connecting existing groups)
    // Apply penalty for creating multiple separate groups
    // Consider distance from center (early game preference)
}
```

### 5. User Interface

The application provides an intuitive and functional interface using the Iced.rs library:

- **Visual board representation**: Clear display of the game board with black and white stones
- **Interactive gameplay**: Click-to-place-stone functionality
- **Control buttons**: Pass, Load SGF, and Enable/Disable AI options
- **Status messages**: Informative feedback about the game state, invalid moves, and current player
- **Responsive design**: Adapts to different window sizes while maintaining proper stone and grid proportions

UI implementation details:
```rust
impl canvas::Program for BoardCanvas {
    // Draw the board background, grid lines, and stones
    // Handle mouse interaction for stone placement
    // Convert screen coordinates to board positions
}
```

## Game Rules

The implementation adheres to standard Go rules:

### Basic Gameplay
- Players (Black and White) take turns placing stones on board intersections
- Black traditionally moves first
- The objective is to surround territory (empty intersections) and capture opponent stones

### Stone Placement
- Stones are placed on grid intersections, not in the squares
- Once placed, stones do not move (unless captured and removed)
- Players can choose to pass instead of placing a stone

### Liberties and Capture
- A liberty is an adjacent empty intersection (orthogonally, not diagonally)
- A stone or connected group of stones is captured and removed when it has no liberties
- Stones of the same color that share an adjacent intersection are connected and share liberties
- A player cannot play a move that would result in their own stones having no liberties (suicide rule), unless that move captures opponent stones

### KO Rule
- A player cannot make a move that would recreate the board position from the previous move
- This prevents infinite cycles of capture and recapture

### Game End
- Traditionally, the game ends when both players pass consecutively
- Final score is determined by counting territory and captured stones
- Note: The current implementation does not automatically detect game end or calculate score

## Implementation Details

### Board Representation

The board is represented as a 2D grid of optional stone colors:

```rust
// None represents an empty intersection
// Some(Color::Black) represents a black stone
// Some(Color::White) represents a white stone
grid: Vec<Vec<Option>>
```

This representation allows for efficient:
- Position checking (`O(1)` time complexity)
- Stone placement and removal
- Board state comparison for KO rule validation

### Move Validation Process

The move validation follows a logical sequence:

1. Boundary check: Verify the move is within the board dimensions
2. Occupation check: Ensure the position is not already occupied
3. Liberty check:
   - Create a temporary board with the proposed move
   - Find the group of connected stones that would result
   - Count the liberties of this group
4. Suicide rule check:
   - If the move would result in zero liberties, check if it captures opponent stones
   - If it captures opponent stones, the move is legal despite being otherwise suicidal
   - If it doesn't capture and has no liberties, the move is illegal
5. KO rule check:
   - Simulate the move including any captures
   - Compare the resulting board state with the stored previous state
   - If they match, the move violates the KO rule and is illegal

### Group Finding Algorithm

The implementation uses a breadth-first search to find connected groups of stones:

```rust
fn find_group(&self, start: Position) -> HashSet {
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
```

This algorithm efficiently:
1. Starts from a given stone position
2. Explores all connected stones of the same color
3. Returns a set of all positions in the group

### Liberty Counting

Liberties are counted by:

```rust
fn count_liberties(&self, group: &HashSet) -> usize {
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
```

This method:
1. Examines each stone in the group
2. Finds all adjacent empty intersections
3. Creates a set of these empty intersections (eliminating duplicates)
4. Returns the size of this set as the number of liberties

### Stone Capture Process

When a stone is placed, the implementation checks for and handles captures:

```rust
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
```

This method:
1. Identifies the color of the placed stone
2. Checks all adjacent opponent stones
3. For each adjacent opponent stone, finds its connected group
4. If the group has no liberties, removes all stones in the group
5. Updates the capture count accordingly

### SGF Parsing

The SGF parser uses the nom library to efficiently parse SGF files:

1. Extracts game properties (board size, player information)
2. Parses initial stone placements (AB, AW properties)
3. Extracts the sequence of moves
4. Builds a `GameInfo` struct containing all necessary information

The parser handles SGF coordinates by converting from SGF notation (a-s letters) to zero-based indices:

```rust
fn parse_position(pos_str: &str, board_size: usize) -> Option {
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
```

## AI Strategy

The AI implementation uses a strategic evaluation approach with two components:

### Random Strategy (20%)
The AI randomly selects from all legal moves 20% of the time. This introduces unpredictability and prevents the AI from being too deterministic, making it more challenging and interesting to play against.

### Heuristic Strategy (80%)
For the majority of moves, the AI evaluates all possible legal moves based on several weighted factors:

1. **Capture Value (Weight: 1.5x)**
   - Moves that capture opponent stones receive a significant bonus
   - The more stones captured, the higher the bonus

2. **Liberty Maximization (Weight: 1.0x)**
   - Moves that result in more liberties for the played stone/group are preferred
   - This promotes safety and flexibility for the AI's stones

3. **Group Connectivity (Weight: 0.5x)**
   - Moves that connect separate groups of the same color receive a bonus
   - This encourages the AI to build stronger, connected formations

4. **Group Minimization (Weight: -0.3x per group)**
   - The AI receives a penalty based on the total number of separate groups it has
   - This discourages fragmented stone placement and encourages cohesive strategy

5. **Center Preference (Weight: -0.2x per distance unit)**
   - In the early game (first ~13 moves on a 9x9 board), moves closer to the center are preferred
   - This implements the strategic principle of prioritizing center influence early

The evaluation function calculates a score for each potential move:

```rust
fn evaluate_move(board: &Board, pos: Position, color: Color) -> f32 {
    let mut score = 0.0;
    
    // Simulate the move
    let mut temp_board = board.clone();
    temp_board.make_move(Move::Place(pos), color);
    
    // Calculate capture value
    score += captured_count as f32 * 1.5;
    
    // Evaluate liberties
    score += liberties as f32;
    
    // Calculate group connectivity bonus
    score += connected_groups as f32 * 0.5;
    
    // Apply penalty for multiple groups
    score -= group_count as f32 * 0.3;
    
    // Apply center preference in early game
    if total_stones < board_size * 3 / 2 {
        score -= distance_from_center * 0.2;
    }
    
    score
}
```

The AI selects the move with the highest overall score, balancing these different strategic considerations.

## Limitations

The current implementation has several limitations that could be addressed in future versions:

1. **No Scoring System**: The game does not implement territory counting or final score calculation
2. **No Game End Detection**: The game does not automatically detect when both players pass consecutively
3. **Limited SGF Support**: The SGF parser handles only basic properties and may not support all SGF features
4. **Fixed File Loading**: SGF files must be named "example.sgf" and placed in the project root
5. **Basic AI Strategy**: The AI uses heuristics but lacks more advanced techniques like Monte Carlo Tree Search
6. **No Handicap System**: The implementation does not support handicap stones for balancing games
7. **Limited UI Features**: No move history, stone markers, or territory visualization
8. **No Resign Option**: There is no way to formally resign a game

## Future Improvements

Several enhancements could make this implementation more complete:

### Gameplay Improvements

1. **Scoring System**:
   - Implement territory counting and final score calculation
   - Display running score during gameplay
   - Support different scoring systems (Chinese, Japanese, etc.)

2. **Game End Detection**:
   - Track consecutive passes
   - Automatically determine game end
   - Display final result with score difference

3. **Handicap and Komi**:
   - Support handicap stone placement for balanced games
   - Implement komi (compensation points) for the second player

### Technical Improvements

1. **Advanced AI**:
   - Implement Monte Carlo Tree Search (MCTS) algorithm
   - Add difficulty levels
   - Improve opening move strategy

2. **Enhanced SGF Support**:
   - Full SGF specification compliance
   - Support for game comments and annotations
   - SGF file export functionality

3. **UI Enhancements**:
   - Move history sidebar
   - Last move highlighting
   - Territory visualization
   - Captured stone display
   - Sound effects

4. **User Experience**:
   - File dialog for SGF loading
   - Game settings configuration
   - Board size selection
   - Customizable colors and themes

### Extended Features

1. **Network Play**:
   - Online multiplayer functionality
   - Game hosting and joining
   - Player rankings

2. **Analysis Tools**:
   - Move analysis and suggestions
   - Heatmap of strong moves
   - Game replay with analysis

3. **Game Database**:
   - Save and load multiple games
   - Search and browse game history
   - Import/export collections of games

## Development Process

This Go game was developed in several stages:

1. **Core Game Logic**: Implementation of the fundamental rules and game state representation
2. **SGF Parser**: Development of file loading capabilities
3. **Basic UI**: Creation of a minimal playable interface
4. **Move Validation**: Implementation of liberty and KO rule checks
5. **AI Strategy**: Development of the computer opponent
6. **Refinement**: Bug fixes, code cleanup, and optimization

The development followed a modular approach, with clear separation of concerns between different components of the codebase. This made it easier to test and debug individual pieces of functionality.

## Technical Challenges

Several challenges were addressed during implementation:

1. **Efficient Group Finding**: Implementing an algorithm to quickly find connected groups of stones
2. **Liberty Counting**: Correctly counting liberties for complex stone formations
3. **KO Rule Validation**: Efficiently checking for board state repetition
4. **SGF Coordinate System**: Converting between SGF coordinates and internal position representation
5. **Rust Ownership**: Managing ownership and borrowing, especially for temporary board states
6. **Canvas Rendering**: Properly drawing the board and stones in the Iced canvas

## Performance Considerations

The implementation balances performance with code clarity:

1. **HashSet for Groups**: Using HashSet for storing stone groups allows for fast insertion and lookup
2. **Board Clone Optimization**: Creating temporary board clones only when necessary for move validation
3. **Lazy Evaluation**: Computing liberties and groups only when needed
4. **Board Size Scaling**: The algorithms are designed to perform well on traditional board sizes (9x9, 13x13, 19x19)
5. **Render Efficiency**: Drawing only what's necessary in the canvas view

---

This project demonstrates the implementation of a complex game with clear rules in Rust, showcasing the language's strengths in creating reliable, maintainable, and efficient software. The modular design makes it easy to extend and enhance in the future.
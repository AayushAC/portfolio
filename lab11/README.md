# Rust Ecosystem Simulation

A comprehensive grid-based multi-agent simulation implementing autonomous entities that navigate to find resources while avoiding obstacles, using customizable heuristics that evolve over time based on survival rates.

![Ecosystem Simulation Screenshot](https://i.imgur.com/example.png)

## Table of Contents

- [Overview](#overview)
- [Requirements Fulfilled](#requirements-fulfilled)
- [Technical Architecture](#technical-architecture)
  - [Project Structure](#project-structure)
  - [Core Components](#core-components)
  - [Data Structures](#data-structures)
  - [Algorithms](#algorithms)
  - [Rendering Pipeline](#rendering-pipeline)
- [Simulation Mechanics](#simulation-mechanics)
  - [Environment System](#environment-system)
  - [Agent System](#agent-system)
  - [Heuristic System](#heuristic-system)
  - [Evolution System](#evolution-system)
- [User Interface Details](#user-interface-details)
  - [Main Components](#main-components)
  - [Control Systems](#control-systems)
  - [Visualization Features](#visualization-features)
  - [User Interactions](#user-interactions)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
  - [Running the Simulation](#running-the-simulation)
- [Usage Guide](#usage-guide)
  - [Basic Operations](#basic-operations)
  - [Advanced Features](#advanced-features)
  - [Experiment Examples](#experiment-examples)
- [Implementation Decisions](#implementation-decisions)
  - [Architecture Choices](#architecture-choices)
  - [Performance Optimizations](#performance-optimizations)
  - [Memory Management](#memory-management)
  - [Concurrency Approach](#concurrency-approach)
- [Future Enhancements](#future-enhancements)
- [Development Notes](#development-notes)
- [Contributing](#contributing)
- [License](#license)
- [Acknowledgments](#acknowledgments)

## Overview

This project implements a sophisticated agent-based ecosystem simulation in Rust using the Iced GUI framework. Multiple autonomous agents navigate a grid environment, seeking food and water resources while managing their hunger, thirst, energy, and health attributes. The simulation features evolving heuristics, allowing agents to improve their survival strategies over generations through a parameterized genetic algorithm approach.

The simulation demonstrates complex emergent behaviors from simple rules, showcasing principles of artificial life, evolutionary algorithms, and agent-based modeling. By adjusting parameters and observing results, users can explore different evolutionary outcomes and agent strategies.

## Requirements Fulfilled

The implementation meets all requirements specified in the lab assignment:

### Environment Requirements
- ✅ Grid-based world (50×50 cells)
- ✅ Randomized placement of food, water, and obstacles
- ✅ Resource regeneration at configurable rates
- ✅ Day/night cycle affecting resource availability

### Agent Requirements
- ✅ Health, hunger, thirst, and energy tracking
- ✅ Age progression over time
- ✅ Movement in cardinal directions
- ✅ Resource consumption mechanics
- ✅ Limited sensing radius (10 cells)
- ✅ Energy expenditure for movement
- ✅ Death conditions based on resource depletion

### Heuristic System Requirements
- ✅ Modular decision-making framework
- ✅ Multiple strategies with different parameter weights
- ✅ User-modifiable heuristic parameters
- ✅ Custom strategy creation and application

### Evolution Requirements
- ✅ Tracking of survival metrics by strategy
- ✅ "Breeding" of successful parameters
- ✅ Mutation of parameters with configurable rates
- ✅ Replacement of unsuccessful strategies with evolved ones
- ✅ Statistical reporting on strategy effectiveness

### User Interface Requirements
- ✅ Grid visualization with entity representation
- ✅ Simulation controls (start/pause/reset/speed)
- ✅ Parameter adjustment interface
- ✅ Agent selection and tracking
- ✅ Real-time statistics display

## Technical Architecture

### Project Structure

```
rust-eco/
├── Cargo.toml                 # Dependency and project configuration
├── src/
│   ├── main.rs                # Entry point
│   ├── simulation/            # Core simulation logic
│   │   ├── mod.rs             # Module exports and shared types
│   │   ├── environment.rs     # Grid and resource management
│   │   ├── agent.rs           # Agent behavior and properties
│   │   └── heuristics.rs      # Decision-making strategies
│   └── ui/                    # User interface components
│       ├── mod.rs             # UI module exports
│       └── app.rs             # Iced application implementation
```

### Core Components

#### 1. Application Core (`main.rs`)
The entry point of the application initializes the Iced application and starts the simulation loop.

```rust
fn main() -> iced::Result {
    ui::app::run()
}
```

#### 2. Environment Management (`environment.rs`)
Handles the simulation grid, entity positioning, resource regeneration, and day/night cycling.

Key components:
- `Environment` struct: Maintains the grid state, resource regeneration rates, and agent positions
- Resource initialization and regeneration algorithms
- Entity placement and collision detection systems
- Nearest resource finding with optimized search algorithms

#### 3. Agent System (`agent.rs`)
Defines the autonomous agents and their behavior systems.

Key components:
- `Agent` struct: Represents individual entities with health, hunger, thirst, and energy attributes
- `AgentSystem` struct: Manages the population, spawning, and updating of agents
- Position tracking for exploration metrics
- Resource consumption and energy management logic
- Death and lifespan tracking mechanisms

#### 4. Heuristic Framework (`heuristics.rs`)
Implements the decision-making system used by agents to navigate the environment.

Key components:
- `HeuristicParameters` struct: Stores weighted parameters for decision-making
- `HeuristicStrategy` enum: Defines different strategic approaches
- `HeuristicSystem` struct: Provides evaluation methods for agent decisions
- Evolution tracking and parameter breeding algorithms
- Strategy comparison and statistical analysis tools

#### 5. User Interface (`app.rs`)
Builds the Iced-based GUI for visualization and interaction.

Key components:
- `EcosystemApp` struct: Main application with Iced integration
- Grid rendering with canvas
- Control panel construction and event handling
- Real-time statistics display and formatting
- Agent selection and detail views

### Data Structures

#### Grid Representation
The environment uses a 2D array for O(1) access to grid cells:
```rust
pub grid: [[EntityType; GRID_SIZE]; GRID_SIZE]
```

#### Entity Type Enumeration
```rust
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EntityType {
    Empty,
    Food,
    Water,
    Obstacle,
    Agent(usize), // Agent with ID
}
```

#### Position Struct
```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub x: usize,
    pub y: usize,
}
```

#### Direction Enumeration
```rust
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Direction {
    North,
    East,
    South,
    West,
}
```

#### Heuristic Parameters
```rust
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct HeuristicParameters {
    pub hunger_weight: u8,       // Importance of hunger (0-100)
    pub thirst_weight: u8,       // Importance of thirst (0-100)
    pub energy_weight: u8,       // Importance of energy conservation (0-100)
    pub exploration_weight: u8,  // Importance of exploration (0-100)
    pub obstacle_weight: u8,     // Penalty for moving toward obstacles (0-100)
}
```

#### Agent State
```rust
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
```

### Algorithms

#### 1. Agent Decision Making
The core algorithm that determines an agent's movement based on its current state and surroundings:

```rust
pub fn choose_direction(agent: &Agent, env: &Environment) -> Direction {
    let params = agent.heuristic.get_parameters();
    
    let mut best_dir = Direction::North;
    let mut best_score = f32::NEG_INFINITY;
    
    for dir in Direction::all() {
        let score = Self::evaluate_direction(agent, env, &dir, &params);
        if score > best_score {
            best_score = score;
            best_dir = dir;
        }
    }
    
    best_dir
}
```

#### 2. Direction Evaluation
Scores each possible move based on multiple weighted factors:

```rust
pub fn evaluate_direction(
    agent: &Agent,
    env: &Environment,
    direction: &Direction,
    params: &HeuristicParameters
) -> f32 {
    // ... [Implementation details] ...
    
    // Food seeking score calculation
    let food_score = (current_dist - new_dist) * hunger_factor * params.hunger_weight_f32() * 10.0;
    
    // Water seeking score calculation
    let water_score = (current_dist - new_dist) * thirst_factor * params.thirst_weight_f32() * 10.0;
    
    // Energy conservation score
    let energy_score = (agent.energy as f32 / 100.0) * params.energy_weight_f32() * 5.0;
    
    // Exploration score
    let exploration_score = params.exploration_weight_f32() * 15.0;
    
    // Combined score determines movement decision
    score
}
```

#### 3. Resource Regeneration
Controlled regeneration with configurable rates and environment conditions:

```rust
pub fn update_resources(&mut self) {
    let mut rng = thread_rng();
    
    // Only regenerate resources during the day
    if self.day_night_cycle && !self.is_day {
        return;
    }
    
    // Regenerate food and water
    for y in 0..GRID_SIZE {
        for x in 0..GRID_SIZE {
            if self.grid[y][x] == EntityType::Empty {
                if rng.gen::<f32>() < self.food_regen_rate {
                    self.grid[y][x] = EntityType::Food;
                } else if rng.gen::<f32>() < self.water_regen_rate {
                    self.grid[y][x] = EntityType::Water;
                }
            }
        }
    }
}
```

#### 4. Agent Evolution
Breeding algorithm that combines parameters from successful agents with mutation:

```rust
pub fn breed(parent1: &Self, parent2: &Self, mutation_rate: f32) -> Self {
    let mut rng = thread_rng();
    let mut child = Self {
        hunger_weight: (parent1.hunger_weight + parent2.hunger_weight) / 2,
        thirst_weight: (parent1.thirst_weight + parent2.thirst_weight) / 2,
        energy_weight: (parent1.energy_weight + parent2.energy_weight) / 2,
        exploration_weight: (parent1.exploration_weight + parent2.exploration_weight) / 2,
        obstacle_weight: (parent1.obstacle_weight + parent2.obstacle_weight) / 2,
    };
    
    // Apply mutations
    if rng.gen::<f32>() < mutation_rate {
        child.hunger_weight = child.hunger_weight.saturating_add_signed(rng.gen_range(-10..=10));
    }
    // ... [Additional mutation logic] ...
    
    child
}
```

#### 5. Nearest Resource Search
Efficient algorithm to find the closest resource within sensing radius:

```rust
pub fn find_nearest_resource(&self, pos: &Position, resource_type: EntityType, radius: usize) -> Option<Position> {
    let nearby = self.get_nearby_entities(pos, radius);
    
    nearby.iter()
        .filter(|(_, &entity)| entity == resource_type)
        .min_by(|(pos_a, _), (pos_b, _)| {
            let dist_a = pos.distance(pos_a);
            let dist_b = pos.distance(pos_b);
            dist_a.partial_cmp(&dist_b).unwrap()
        })
        .map(|(pos, _)| *pos)
}
```

### Rendering Pipeline

The simulation uses Iced's canvas system for efficient grid rendering:

1. **State Preparation**: The simulation state is prepared for rendering
2. **Grid Rendering**: The environment grid is drawn with appropriate colors for each entity
3. **Agent Visualization**: Agents are drawn with colors indicating their strategy
4. **Selection Highlighting**: Selected agents are highlighted with their sensing radius visible
5. **UI Component Rendering**: Control panels, statistics, and agent details are rendered
6. **Composition**: All elements are composed into the final frame

## Simulation Mechanics

### Environment System

#### Grid Initialization
The environment begins with random distribution of:
- Food sources (10% of cells)
- Water sources (5% of cells)
- Obstacles (15% of cells)

#### Resource Management
- **Regeneration**: Empty cells have a chance to spawn resources each tick
- **Consumption**: Resources are removed when agents consume them
- **Day/Night Cycle**: Resources only regenerate during day phase

#### Collision Detection
- Agents cannot move into cells containing obstacles or other agents
- Movement is restricted to the grid boundaries

### Agent System

#### Vital Statistics
All agents maintain the following attributes:
- **Health**: Derived from hunger and thirst levels (0-100)
- **Hunger**: Increases over time, decreased by consuming food (0-100)
- **Thirst**: Increases over time, decreased by consuming water (0-100)
- **Energy**: Depleted by movement, recovered by resting (0-100)
- **Age**: Increments each simulation tick

#### Life Cycle
1. **Birth**: Agent spawns with full health and initial resource levels
2. **Maturation**: Agent develops movement patterns based on its heuristic
3. **Survival**: Agent navigates to find and consume resources
4. **Death**: Agent dies when health reaches zero

#### Movement Mechanics
- Movement costs 5 energy points per step
- Agents can move in four cardinal directions
- Agents can only sense resources within a 10-cell radius
- Movement decisions are based on weighted heuristic parameters

#### Resource Consumption
- **Food**: Reduces hunger by 30 points when consumed
- **Water**: Reduces thirst by 40 points when consumed
- Consumption requires moving onto a resource cell

### Heuristic System

#### Parameter System
Five key parameters influence agent decisions:
1. **Hunger Weight**: Importance of moving toward food (0-100)
2. **Thirst Weight**: Importance of moving toward water (0-100)
3. **Energy Weight**: Importance of conserving energy (0-100)
4. **Exploration Weight**: Importance of visiting new areas (0-100)
5. **Obstacle Weight**: Penalty for moving toward obstacles (0-100)

#### Predefined Strategies
1. **Survival Strategy**:
   - High hunger and thirst weights
   - Moderate energy weight
   - Low exploration weight
   - High obstacle avoidance

2. **Explorer Strategy**:
   - Moderate hunger and thirst weights
   - Low energy weight
   - High exploration weight
   - Moderate obstacle avoidance

3. **Efficient Strategy**:
   - Moderate hunger and thirst weights
   - High energy weight
   - Low exploration weight
   - High obstacle avoidance

#### Custom Strategies
Users can create custom strategies by:
1. Selecting a base strategy
2. Adjusting individual parameter sliders
3. Applying changes to create a new custom strategy
4. Using the custom strategy for new agents

#### Decision Algorithm
The decision-making process follows these steps:
1. Evaluate all possible movement directions
2. Calculate a score for each direction based on:
   - Distance to nearest food (weighted by hunger and hunger_weight)
   - Distance to nearest water (weighted by thirst and thirst_weight)
   - Energy cost (weighted by energy_weight)
   - Exploration value (weighted by exploration_weight)
   - Obstacle proximity (weighted by obstacle_weight)
3. Choose the direction with the highest score

### Evolution System

#### Success Metrics
The system tracks:
- Lifespan of agents using each strategy
- Average survival time by strategy
- Population distribution across strategies

#### Breeding Mechanism
When spawning evolved agents:
1. Select two parent strategies from living agents
2. Average their parameter values
3. Apply random mutations based on mutation rate
4. Create a new agent with the evolved parameters

#### Mutation System
- Each parameter has a chance to mutate based on the mutation rate
- Mutations add or subtract a small random value (-10 to +10 points)
- Parameters are clamped to the valid range (0-100)

#### Selection Pressure
- Strategies with longer-lived agents have more influence on future generations
- The system automatically spawns evolved agents when population is low
- Over time, more successful strategies become predominant

## User Interface Details

### Main Components

#### Grid Display
- Rendered using Iced's canvas system
- Each cell is 8×8 pixels
- Color-coded entities for easy identification
- Selected agent highlighting
- Sensing radius visualization

#### Control Panel
- Simulation controls (play/pause, reset, speed)
- Agent controls (spawn random, spawn evolved)
- Heuristic settings (strategy selection, parameter adjustment)
- Statistics display
- Agent details view

### Control Systems

#### Simulation Controls
- **Play/Pause Button**: Toggles the simulation running state
- **Reset Button**: Reinitializes the environment and agents
- **Speed Slider**: Adjusts simulation tick rate from 1 to 20 ticks per second
- **Day/Night Toggle**: Enables or disables the day/night cycle

#### Agent Controls
- **Spawn Random Button**: Adds 5 agents with random strategies
- **Spawn Evolved Button**: Adds 5 agents with evolved parameters
- **Mutation Rate Slider**: Controls the degree of parameter variation (0.0-0.5)
- **Add Agent Button**: Creates a single agent with the selected strategy

#### Heuristic Controls
- **Strategy Selector**: Drop-down menu for selecting predefined strategies
- **Parameter Sliders**: Five sliders for adjusting individual weights
- **Apply Button**: Creates a custom strategy from current parameters

### Visualization Features

#### Color Coding System
- **Food**: Green (#00CC00)
- **Water**: Blue (#0000CC)
- **Obstacles**: Gray (#808080)
- **Agents by Strategy**:
  - Survival: Red (#FF0000)
  - Explorer: Orange (#FF8000)
  - Efficient: Purple (#CC00CC)
  - Custom: Cyan (#00CCCC)
- **Selected Agent**: Yellow (#FFFF00)

#### Environment Visualization
- Day phase: Light background (#E6E6E6)
- Night phase: Dark blue background (#1A1A4D)

#### Agent Selection
- Clicking an agent highlights it in yellow
- Displays a circle showing the agent's sensing radius
- Shows detailed statistics in the agent details panel

### User Interactions

#### Direct Manipulation
- **Grid Clicking**: Select an agent by clicking on it
- **Button Interactions**: Control simulation flow and agent creation
- **Sliders**: Adjust simulation speed and heuristic parameters
- **Toggles**: Enable/disable features like day/night cycle

#### Real-time Feedback
- Statistics update every tick while simulation is running
- Agent positions update in real-time on the grid
- Selected agent details refresh continuously
- Strategy distribution updates as population changes

## Getting Started

### Prerequisites

- Rust 1.67 or newer
- Cargo package manager
- Operating system: Windows, macOS, or Linux
- Graphics support for Iced (OpenGL 2.1+)

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/rust-eco.git
   cd rust-eco
   ```

2. Build the project:
   ```bash
   cargo build --release
   ```

### Running the Simulation

Launch the application:
```bash
cargo run --release
```

The `--release` flag provides significant performance improvements for grid rendering and agent updates.

## Usage Guide

### Basic Operations

#### Starting Your First Simulation

1. **Launch the application**
2. **Observe the initial state**: The grid is populated with:
   - Green cells (food)
   - Blue cells (water)
   - Gray cells (obstacles)
   - Colored dots (agents with different strategies)
3. **Press Play**: Click the "Play" button to start the simulation
4. **Observe agent behavior**: Watch as agents navigate to find resources
5. **Adjust speed**: Use the speed slider to speed up or slow down the simulation

#### Managing Agents

1. **Adding agents**:
   - Click "Spawn 5 Random" to add agents with random strategies
   - Select a strategy and click "Add Agent with Strategy" for specific types

2. **Replacing the population**:
   - Click "Reset" to clear the grid and start fresh
   - The simulation automatically spawns new agents when population is low

3. **Examining agents**:
   - Click on an agent to select it
   - View its vital statistics in the agent details panel
   - Observe its sensing radius (yellow circle)

#### Controlling Simulation Parameters

1. **Environment settings**:
   - Toggle "Day/Night Cycle" to affect resource regeneration
   - Watch how agents adapt to changing resource availability

2. **Evolution settings**:
   - Adjust the "Mutation Rate" slider to control genetic diversity
   - Higher rates create more variation, lower rates refine successful strategies

### Advanced Features

#### Creating Custom Heuristics

1. **Select a base strategy** from the dropdown menu
2. **Observe the default parameters** loaded into the sliders
3. **Adjust individual parameters**:
   - Increase hunger_weight to prioritize food-seeking
   - Increase thirst_weight to prioritize water-seeking
   - Increase energy_weight to minimize movement
   - Increase exploration_weight to encourage discovery
   - Increase obstacle_weight to enhance obstacle avoidance
4. **Click "Apply Custom Parameters"**
5. **Click "Add Agent with Strategy"** to create an agent with your custom heuristic
6. **Watch your custom agent** and compare its performance

#### Analyzing Strategy Effectiveness

1. **Run the simulation for several hundred ticks**
2. **Observe the Statistics panel**:
   - Note which strategies have the highest average lifespan
   - Check the population distribution to see which strategies dominate
3. **Select agents of different strategies** to compare their vital statistics
4. **Spawn evolved agents** and observe how they compare to predefined strategies

#### Setting Up Experiments

1. **Design experiment parameters**:
   - Reset the simulation
   - Configure day/night cycle and mutation rate
   - Spawn a specific mix of agent strategies
2. **Run the simulation** for a set number of ticks
3. **Record results** from the statistics panel
4. **Modify parameters** and repeat to compare outcomes

### Experiment Examples

#### Survival Strategy Dominance Test

1. Reset the simulation
2. Disable day/night cycle
3. Set mutation rate to 0.1
4. Spawn 5 agents of each strategy type
5. Run for 500 ticks
6. Record which strategy has the highest population and average lifespan

#### Resource Scarcity Adaptation

1. Reset the simulation
2. Enable day/night cycle
3. Set mutation rate to 0.2
4. Spawn 15 random agents
5. Run for 1000 ticks
6. Observe how evolved agents adapt to the cyclical resource availability

#### Custom Strategy Optimization

1. Create a custom strategy with high exploration (80+)
2. Add 10 agents with this strategy
3. Set mutation rate to 0.1
4. Run for 300 ticks
5. Observe how the parameters evolve
6. Create a new custom strategy based on the evolved parameters
7. Compare performance of the original and evolved custom strategies

## Implementation Decisions

### Architecture Choices

#### Component-Based Structure
The simulation uses a component-based architecture to separate concerns:
- Environment system manages the grid and resources
- Agent system handles entity behavior and attributes
- Heuristic system provides decision-making algorithms
- UI system manages visualization and user interaction

This approach facilitates maintenance, testing, and extension of individual components without affecting others.

#### Strong Type System Utilization
The implementation leverages Rust's type system for safety and clarity:
- Enums with variants for different entity types and strategies
- Structs with clear fields for agent and environment properties
- Custom type implementations (PartialEq, Eq, Hash) for Position to enable HashMap usage
- Integer-based parameters (u8) instead of floating-point (f32) to enable Eq implementation

#### Event-Driven UI
The interface uses Iced's event-driven model:
- UI events trigger simulation state changes
- Simulation updates cause UI refreshes
- Subscription system manages timing for simulation ticks

### Performance Optimizations

#### Grid Size Selection
The simulation uses a 50×50 grid rather than 100×100 for several reasons:
- Quadratic performance impact with grid size (2500 vs 10000 cells)
- Better rendering performance with fewer cells
- Sufficient complexity for interesting emergent behaviors
- Faster agent decision-making with fewer cells to evaluate

#### Optimized Resource Finding
The `find_nearest_resource` function is optimized to:
- Only search within the agent's sensing radius
- Return early when ideal resources are found
- Use efficient distance comparisons
- Filter before distance calculation to reduce computation

#### Efficient Agent Updates
- Agents that die are removed from the update loop
- Agents with low energy rest instead of calculating movement
- HashMap tracks agent positions for O(1) lookup

#### Memory Efficiency
- Fixed-size grid avoids dynamic allocations
- Agents use compact representation (usize for attributes)
- Parameter values use u8 (0-100) instead of f32 to reduce memory usage
- Canvas cache reduces rendering overhead

### Memory Management

#### Ownership Model
- Environment owns the grid and manages entity placement
- AgentSystem owns all agents and manages their lifecycle
- Agents borrow environment references for decision-making
- Iced UI owns the rendering canvas and controls

#### Reference Implementation
- Functions take immutable references where possible
- Mutation is isolated to specific update methods
- Clone is implemented for structures that need copying

#### Hash Maps for Spatial Queries
- Agent positions tracked in HashMap for quick lookup
- Visited positions stored in HashMap for exploration tracking
- Success metrics stored in HashMap for statistical analysis

### Concurrency Approach

The simulation uses a single-threaded model for simplicity, with:
- Sequential agent updates to avoid race conditions
- Iced's event loop for UI responsiveness
- Time-based subscription for simulation ticks

A future enhancement could implement parallel agent updates for larger grids.

## Future Enhancements

### Immediate Improvements

1. **Performance Optimization**
   - Parallel agent updates using Rayon
   - Spatial partitioning for more efficient nearest-resource queries
   - Further canvas rendering optimizations

2. **UI Enhancements**
   - Zoom and pan controls for the grid
   - Time-series graphs of population and strategy distribution
   - Heatmap visualization of agent movement patterns

3. **Simulation Features**
   - Agent reproduction based on resource acquisition
   - Agent aging effects on attributes
   - More sophisticated resource distribution patterns

### Medium-Term Extensions

1. **Advanced Heuristics**
   - Neural network-based decision making
   - Reinforcement learning for strategy improvement
   - Adaptive parameter adjustment based on environment

2. **Environmental Complexity**
   - Variable terrain affecting movement cost
   - Weather effects on resource availability
   - Random events (disasters, resource booms)

3. **Agent Specialization**
   - Different agent types with unique attributes
   - Cooperative behaviors between agents
   - Trading or resource sharing mechanics

### Long-Term Vision

1. **Ecosystem Simulation**
   - Full predator-prey dynamics
   - Food chain and energy transfer modeling
   - Plant growth and ecological succession

2. **Advanced Evolution**
   - Genotype/phenotype distinction
   - Sexual reproduction with genetic crossover
   - Speciation and biodiversity metrics

3. **Scientific Analysis Tools**
   - Data export for external analysis
   - Parameter sweeping for automated experimentation
   - Statistical significance testing for evolutionary outcomes

## Development Notes

### Technical Challenges

#### Rust Type System Adaptation
- Implementing `Eq` for `HeuristicParameters` required switching from floating-point (f32) to integer (u8) representation for parameters
- Helper methods were added to convert between integer and float values as needed for calculations

#### Iced Framework Integration
- Canvas rendering required custom implementation of the `Program` trait
- Subscription system needed specific timing configuration for simulation updates
- Custom styling for UI components matched the simulation aesthetic

#### Evolution System Balance
- Mutation rates needed careful tuning to avoid both stagnation and chaos
- Parameter averaging was selected over more complex genetic algorithms for simplicity
- Selection pressure was balanced to allow diversity while rewarding successful strategies

### Known Limitations

1. **Performance with Large Populations**
   - The simulation slows with >100 agents due to O(n²) operations in agent interaction checks
   - Grid rendering becomes less efficient at larger sizes

2. **UI Responsiveness**
   - Heavy simulation load can cause UI lag at high speeds
   - No background threading for simulation updates

3. **Evolution Depth**
   - Limited to parameter evolution rather than structural changes to decision algorithms
   - No explicit speciation or divergent evolution tracking

### Code Quality Considerations

1. **Error Handling**
   - Robust error handling for user interactions
   - Graceful degradation with invalid parameters
   - Panic prevention with boundary checks

2. **Documentation**
   - Comprehensive struct and function documentation
   - Example usage for public APIs
   - Architecture explanation for complex components

3. **Testing**
   - Unit tests for core algorithms
   - Integration tests for component interaction
   - Property-based tests for evolution system

## Contributing

Contributions to this project are welcome! Here's how to get started:

1. **Fork the repository**

2. **Create a feature branch**:
   ```bash
   git checkout -b feature/your-feature-name
   ```

3. **Implement your changes**:
   - Follow existing code style and architecture
   - Add comprehensive documentation
   - Include tests for new functionality

4. **Run the test suite**:
   ```bash
   cargo test
   ```

5. **Ensure your code builds**:
   ```bash
   cargo build --release
   ```

6. **Commit your changes**:
   ```bash
   git commit -m "Add detailed description of your changes"
   ```

7. **Push to your fork**:
   ```bash
   git push origin feature/your-feature-name
   ```

8. **Submit a pull request**:
   - Describe the purpose and scope of your changes
   - Reference any related issues
   - Provide examples of how to test your changes

### Development Environment Setup

To set up an optimal development environment:

1. **Install Rust toolchain**:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. **Install development tools**:
   ```bash
   rustup component add clippy rustfmt
   ```

3. **Configure IDE** (recommended: VS Code with rust-analyzer)

4. **Setup git hooks** for automatic formatting and linting

### Coding Standards

- Follow Rust's official style guide
- Use meaningful variable and function names
- Keep functions focused on a single responsibility
- Add docstrings for all public functions and types
- Use proper error handling instead of unwrap/expect where possible
- Write tests for new functionality

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Built as a lab project for advanced programming course
- Uses the Iced framework (https://github.com/iced-rs/iced) for GUI rendering
- Inspired by classic cellular automata and agent-based modeling systems
- Thanks to the Rust community for excellent documentation and crates

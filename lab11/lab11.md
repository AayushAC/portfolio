# Lab 11: Ecosystem Simulation

* Deadline: no deadline, portfolio work only
* Language: Rust or Haskell
* Score: this is C/B level task
* LLMs: **use them**
* Co-pilots: **use them**


This is an open-ended task, with many aspects subject to your own 
design decisions. Many aspects are of your own choosing and you 
can design them the way YOU want. For example, the size of the 
grid (100x100) and the sensing radius of agents can be changed.
In the portfolio, motivate your choices and design decisions. 

## Overview

Building on your experience from `Lab 10` with the Snake game, you will now implement an agent-based ecosystem simulation. In this project, you will create a grid environment populated with autonomous agents that must navigate to find resources (food and water) while avoiding obstacles. The agents will employ customizable heuristics that should be updated over time based on survival rates.

## Learning Objectives

- Apply and extend your knowledge of Rust or Haskell programming
- Implement agent-based simulation with autonomous behaviors
- Design and implement customizable heuristics for decision-making
- Create a system for evolutionary selection based on fitness criteria
- Develop interactive visualization for a complex simulation


## Choice of Language

You may implement this project in either:
- **Rust** with the Iced framework (or another GUI framework of your choice)
- **Haskell** with Gloss, GTK3 bindings, or another GUI/text-based framework

Build upon the knowledge and frameworks you used in your Snake game implementation.



## Simulation Requirements

### Environment
1. Create a grid-based world (recommended size: 100x100 cells)
2. Populate the grid with:
   - Food sources (randomly placed)
   - Water sources (randomly placed)
   - Obstacles (walls, hazards, etc.)
   - Agents (starting at random positions)
3. Resources should regenerate at a configurable rate
4. Implement a day/night cycle that affects agent behavior and resource availability (optional)

### Agents
1. Each agent must have:
   - Health level. Health is a simple weighted inverse function of hunger and thirst.
   - Hunger level (increases over time, decreased by consuming food)
   - Thirst level (increases over time, decreased by consuming water)
   - Energy level (depleted by movement, replenished by rest). Energy capacity is a function of health.
   - Age (increases over time)
2. Agents should:
   - Move around the grid to find resources
   - Movement consumes energy
   - Consume food and water to survive
   - Die when health (thirst or hunger) reaches zero
3. Additional considerations:
   - Agents move west, north, east or south only. 
   - Agents have limited "sensing radius", that is, they can only see food, 
     water and obstacles in the range of `10` around them. 

### Heuristics System
1. Design a modular system for agent decision-making that considers:
   - Distance to nearest food/water
   - Current hunger/thirst levels
   - Presence of obstacles
   - Energy conservation
   - Exploration vs. exploitation tradeoff
2. Create at least 3 different heuristic strategies
3. Implement a way for the user to:
   - View the current heuristics in use
   - Modify parameters of existing heuristics
   - Create new heuristic rules
   - Save and load different heuristic configurations

### Evolution and Selection
1. Track the survival time and success metrics of agents using different heuristics
2. Implement a mechanism for "breeding" successful heuristics:
   - Combine parameters from successful strategies
   - Add small random mutations
   - Replace unsuccessful strategies with new variations
3. Display statistics about which strategies are most successful
4. Allow for multiple generations of evolution to occur

### User Interface
1. Visualize the grid and all entities (agents, food, water, obstacles)
2. Provide controls to:
   - Start/pause/reset the simulation
   - Adjust simulation speed
   - Modify heuristic parameters
   - Select and track individual agents
3. Display real-time statistics:
   - Population size
   - Average agent lifespan
   - Resource distribution
   - Success rates of different heuristics


## Technical Requirements

### For Rust Implementation
1. Use appropriate crates for your chosen frameworks. The only constraint is that you must use `Iced.rs` for GUI if you use Rust.
2. Implement proper error handling and memory management
3. Structure your code using appropriate Rust patterns and practices
4. Use traits to structure your code better.

### For Haskell Implementation
1. Implement pure functional representations of the simulation state
2. Use appropriate data structures for efficient grid operations
3. Leverage Haskell's type system for modeling simulation entities
4. Employ higher-order functions for implementing heuristics

### Common Requirements
1. Organize your code with clear module separation:
   - Simulation logic
   - Agent behavior and heuristics
   - Visualization and rendering
   - User interface and controls
   - Statistics and heuristic update systems
2. Include appropriate tests for core functionality
3. Implement efficient algorithms for:
   - Resource discovery
   - Agent actions and plans
   - Collision detection



Good luck, and enjoy creating your ecosystem simulation!
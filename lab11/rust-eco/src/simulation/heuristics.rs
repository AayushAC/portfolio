impl std::fmt::Display for HeuristicStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}use std::collections::HashMap;
use rand::{Rng, thread_rng};
use serde::{Serialize, Deserialize};

use crate::simulation::{Direction, EntityType, SENSING_RADIUS};
use crate::simulation::environment::Environment;
use crate::simulation::agent::Agent;

// We'll convert the float values to fixed-point integers for comparison
// This ensures we can implement Eq for HeuristicParameters
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct HeuristicParameters {
    // Store weights as integers (0-100) for Eq compatibility
    pub hunger_weight: u8,       // Importance of hunger (0-100)
    pub thirst_weight: u8,       // Importance of thirst (0-100)
    pub energy_weight: u8,       // Importance of energy conservation (0-100)
    pub exploration_weight: u8,  // Importance of exploration (0-100)
    pub obstacle_weight: u8,     // Penalty for moving toward obstacles (0-100)
}

impl HeuristicParameters {
    // Helper methods to convert between integer and float representations
    fn to_float(value: u8) -> f32 {
        value as f32 / 100.0
    }
    
    fn from_float(value: f32) -> u8 {
        (value * 100.0).clamp(0.0, 100.0) as u8
    }
    
    // Get the float value for calculations
    pub fn hunger_weight_f32(&self) -> f32 {
        Self::to_float(self.hunger_weight)
    }
    
    pub fn thirst_weight_f32(&self) -> f32 {
        Self::to_float(self.thirst_weight)
    }
    
    pub fn energy_weight_f32(&self) -> f32 {
        Self::to_float(self.energy_weight)
    }
    
    pub fn exploration_weight_f32(&self) -> f32 {
        Self::to_float(self.exploration_weight)
    }
    
    pub fn obstacle_weight_f32(&self) -> f32 {
        Self::to_float(self.obstacle_weight)
    }
    
    pub fn random() -> Self {
        let mut rng = thread_rng();
        Self {
            hunger_weight: Self::from_float(rng.gen_range(0.3..0.8)),
            thirst_weight: Self::from_float(rng.gen_range(0.3..0.8)),
            energy_weight: Self::from_float(rng.gen_range(0.1..0.5)),
            exploration_weight: Self::from_float(rng.gen_range(0.1..0.4)),
            obstacle_weight: Self::from_float(rng.gen_range(0.5..0.9)),
        }
    }
    
    pub fn default_survival() -> Self {
        Self {
            hunger_weight: Self::from_float(0.7),
            thirst_weight: Self::from_float(0.8),
            energy_weight: Self::from_float(0.3),
            exploration_weight: Self::from_float(0.2),
            obstacle_weight: Self::from_float(0.8),
        }
    }
    
    pub fn default_explorer() -> Self {
        Self {
            hunger_weight: Self::from_float(0.5),
            thirst_weight: Self::from_float(0.5),
            energy_weight: Self::from_float(0.2),
            exploration_weight: Self::from_float(0.7),
            obstacle_weight: Self::from_float(0.6),
        }
    }
    
    pub fn default_efficient() -> Self {
        Self {
            hunger_weight: Self::from_float(0.6),
            thirst_weight: Self::from_float(0.6),
            energy_weight: Self::from_float(0.8),
            exploration_weight: Self::from_float(0.1),
            obstacle_weight: Self::from_float(0.7),
        }
    }
    
    // Combine parameters from two parents with possible mutation
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
        if rng.gen::<f32>() < mutation_rate {
            child.thirst_weight = child.thirst_weight.saturating_add_signed(rng.gen_range(-10..=10));
        }
        if rng.gen::<f32>() < mutation_rate {
            child.energy_weight = child.energy_weight.saturating_add_signed(rng.gen_range(-10..=10));
        }
        if rng.gen::<f32>() < mutation_rate {
            child.exploration_weight = child.exploration_weight.saturating_add_signed(rng.gen_range(-10..=10));
        }
        if rng.gen::<f32>() < mutation_rate {
            child.obstacle_weight = child.obstacle_weight.saturating_add_signed(rng.gen_range(-10..=10));
        }
        
        child
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HeuristicStrategy {
    Survival,
    Explorer,
    Efficient,
    Custom(HeuristicParameters),
}

impl HeuristicStrategy {
    pub fn get_parameters(&self) -> HeuristicParameters {
        match self {
            HeuristicStrategy::Survival => HeuristicParameters::default_survival(),
            HeuristicStrategy::Explorer => HeuristicParameters::default_explorer(),
            HeuristicStrategy::Efficient => HeuristicParameters::default_efficient(),
            HeuristicStrategy::Custom(params) => params.clone(),
        }
    }
    
    pub fn name(&self) -> String {
        match self {
            HeuristicStrategy::Survival => "Survival".to_string(),
            HeuristicStrategy::Explorer => "Explorer".to_string(),
            HeuristicStrategy::Efficient => "Efficient".to_string(),
            HeuristicStrategy::Custom(_) => "Custom".to_string(),
        }
    }
    
    pub fn all_strategies() -> Vec<HeuristicStrategy> {
        vec![
            HeuristicStrategy::Survival,
            HeuristicStrategy::Explorer,
            HeuristicStrategy::Efficient,
        ]
    }
}

pub struct HeuristicSystem;

impl HeuristicSystem {
    // Evaluate the value of moving in a particular direction
    pub fn evaluate_direction(
        agent: &Agent,
        env: &Environment,
        direction: &Direction,
        params: &HeuristicParameters
    ) -> f32 {
        let current_pos = agent.position;
        let new_pos = match current_pos.apply_direction(direction) {
            Some(pos) => pos,
            None => return -1000.0, // Invalid move, heavily penalize
        };
        
        // Check if the new position is an obstacle or another agent
        match env.get_entity_at(&new_pos) {
            EntityType::Obstacle => return -500.0, // Cannot move into obstacles
            EntityType::Agent(_) => return -500.0, // Cannot move into other agents
            _ => {},
        }
        
        let mut score = 0.0;
        
        // Food seeking score (based on hunger and proximity to food)
        let nearest_food = env.find_nearest_resource(&current_pos, EntityType::Food, SENSING_RADIUS);
        if let Some(food_pos) = nearest_food {
            let current_dist = current_pos.distance(&food_pos);
            let new_dist = new_pos.distance(&food_pos);
            
            // Reward moving toward food if hungry
            let hunger_factor = agent.hunger as f32 / 100.0;
            let food_score = (current_dist - new_dist) * hunger_factor * params.hunger_weight_f32() * 10.0;
            score += food_score;
        }
        
        // Water seeking score (based on thirst and proximity to water)
        let nearest_water = env.find_nearest_resource(&current_pos, EntityType::Water, SENSING_RADIUS);
        if let Some(water_pos) = nearest_water {
            let current_dist = current_pos.distance(&water_pos);
            let new_dist = new_pos.distance(&water_pos);
            
            // Reward moving toward water if thirsty
            let thirst_factor = agent.thirst as f32 / 100.0;
            let water_score = (current_dist - new_dist) * thirst_factor * params.thirst_weight_f32() * 10.0;
            score += water_score;
        }
        
        // Energy conservation score
        let energy_score = (agent.energy as f32 / 100.0) * params.energy_weight_f32() * 5.0;
        score -= energy_score; // Moving costs energy, so this is a penalty
        
        // Exploration score (prefer unexplored areas)
        if agent.visited_positions.get(&new_pos).is_none() {
            score += params.exploration_weight_f32() * 15.0;
        } else {
            // Penalize revisiting recently visited positions
            let visits = agent.visited_positions.get(&new_pos).unwrap_or(&0);
            score -= (*visits as f32) * params.exploration_weight_f32() * 0.5;
        }
        
        score
    }
    
    // Choose the best direction for the agent to move based on the heuristic
    pub fn choose_direction(
        agent: &Agent,
        env: &Environment
    ) -> Direction {
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
    
    // Track success metrics for evolution
    pub fn track_heuristic_success(
        strategy: &HeuristicStrategy,
        lifespan: usize,
        success_metrics: &mut HashMap<String, (usize, usize)>, // (total_lifespan, count)
    ) {
        let strategy_name = strategy.name();
        let entry = success_metrics.entry(strategy_name).or_insert((0, 0));
        entry.0 += lifespan;
        entry.1 += 1;
    }
    
    // Get average lifespan for each strategy
    pub fn get_strategy_stats(
        success_metrics: &HashMap<String, (usize, usize)>,
    ) -> HashMap<String, f32> {
        let mut stats = HashMap::new();
        
        for (strategy, (total_lifespan, count)) in success_metrics {
            if *count > 0 {
                let avg_lifespan = *total_lifespan as f32 / *count as f32;
                stats.insert(strategy.clone(), avg_lifespan);
            }
        }
        
        stats
    }
}
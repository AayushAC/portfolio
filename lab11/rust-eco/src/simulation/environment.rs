use std::collections::HashMap;
use rand::{Rng, thread_rng};
use crate::simulation::{EntityType, Position, GRID_SIZE};

pub struct Environment {
    pub grid: [[EntityType; GRID_SIZE]; GRID_SIZE],
    pub day_night_cycle: bool,
    pub is_day: bool,
    pub cycle_counter: usize,
    pub cycle_length: usize, // Number of ticks per day/night cycle
    pub food_regen_rate: f32,
    pub water_regen_rate: f32,
    pub agent_positions: HashMap<usize, Position>, // Map agent IDs to positions
}

impl Environment {
    pub fn new() -> Self {
        let mut env = Self {
            grid: [[EntityType::Empty; GRID_SIZE]; GRID_SIZE],
            day_night_cycle: true,
            is_day: true,
            cycle_counter: 0,
            cycle_length: 100, // 100 ticks per day/night cycle
            food_regen_rate: 0.02, // 2% chance per cell per cycle
            water_regen_rate: 0.01, // 1% chance per cell per cycle
            agent_positions: HashMap::new(),
        };
        
        env.initialize_resources();
        env
    }
    
    pub fn initialize_resources(&mut self) {
        let mut rng = thread_rng();
        
        // Add food (10% of cells)
        for _ in 0..(GRID_SIZE * GRID_SIZE / 10) {
            let x = rng.gen_range(0..GRID_SIZE);
            let y = rng.gen_range(0..GRID_SIZE);
            if self.grid[y][x] == EntityType::Empty {
                self.grid[y][x] = EntityType::Food;
            }
        }
        
        // Add water (5% of cells)
        for _ in 0..(GRID_SIZE * GRID_SIZE / 20) {
            let x = rng.gen_range(0..GRID_SIZE);
            let y = rng.gen_range(0..GRID_SIZE);
            if self.grid[y][x] == EntityType::Empty {
                self.grid[y][x] = EntityType::Water;
            }
        }
        
        // Add obstacles (15% of cells)
        for _ in 0..(GRID_SIZE * GRID_SIZE * 3 / 20) {
            let x = rng.gen_range(0..GRID_SIZE);
            let y = rng.gen_range(0..GRID_SIZE);
            if self.grid[y][x] == EntityType::Empty {
                self.grid[y][x] = EntityType::Obstacle;
            }
        }
    }
    
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
    
    pub fn update_day_night_cycle(&mut self) {
        if self.day_night_cycle {
            self.cycle_counter += 1;
            if self.cycle_counter >= self.cycle_length {
                self.is_day = !self.is_day;
                self.cycle_counter = 0;
            }
        }
    }
    
    pub fn get_entity_at(&self, pos: &Position) -> EntityType {
        self.grid[pos.y][pos.x]
    }
    
    pub fn set_entity_at(&mut self, pos: &Position, entity: EntityType) {
        self.grid[pos.y][pos.x] = entity;
        
        // Update agent positions map if needed
        if let EntityType::Agent(id) = entity {
            self.agent_positions.insert(id, *pos);
        } else if let EntityType::Agent(id) = self.grid[pos.y][pos.x] {
            self.agent_positions.remove(&id);
        }
    }
    
    pub fn move_agent(&mut self, agent_id: usize, from: &Position, to: &Position) -> bool {
        // Check if the destination is valid
        if to.x >= GRID_SIZE || to.y >= GRID_SIZE {
            return false;
        }
        
        // Check if the destination is empty or has resources
        let dest_entity = self.get_entity_at(to);
        match dest_entity {
            EntityType::Empty | EntityType::Food | EntityType::Water => {
                // Update the grid
                self.set_entity_at(from, EntityType::Empty);
                self.set_entity_at(to, EntityType::Agent(agent_id));
                true
            },
            _ => false,
        }
    }
    
    pub fn get_nearby_entities(&self, pos: &Position, radius: usize) -> HashMap<Position, EntityType> {
        let mut nearby = HashMap::new();
        
        let min_x = pos.x.saturating_sub(radius);
        let max_x = (pos.x + radius).min(GRID_SIZE - 1);
        let min_y = pos.y.saturating_sub(radius);
        let max_y = (pos.y + radius).min(GRID_SIZE - 1);
        
        for y in min_y..=max_y {
            for x in min_x..=max_x {
                let entity_pos = Position::new(x, y);
                let entity = self.get_entity_at(&entity_pos);
                if entity != EntityType::Empty {
                    nearby.insert(entity_pos, entity);
                }
            }
        }
        
        nearby
    }
    
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
}
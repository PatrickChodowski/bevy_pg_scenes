use bevy::prelude::*;
use bevy::platform::collections::HashMap;

#[derive(Resource)]
pub struct MSSettings {
    pub markers_mapping: fn(name: String) -> Marker,
    pub spawners_mapping: fn(name: String, maybe_data: &Option<HashMap<String, String>>) -> Spawner
}

#[derive(Component, Reflect)]
pub struct Spawnee;


#[derive(Component, Reflect, Clone)]
pub struct Spawner {
    pub id:     usize,
    pub data:   HashMap<String, String>, // contain any key value data that might be useful to pass from editor to game
    pub active: bool
}

impl Spawner {
    pub fn new(id: usize) -> Self {
        Spawner{id, active: true, data: HashMap::new()}
    }
    pub fn add(&mut self, k: String, v: String) -> Self {
        self.data.insert(k, v);
        self.clone()
    }
}


#[derive(Component, Reflect)]
pub struct Marker {
    pub id: usize,
    pub processed: bool
}
#[derive(Component, Reflect)]
pub struct Markee; // Result of Markers processing

impl Marker {
    pub fn new(id: usize) -> Self {
        Marker{id, processed: false}
    }
}


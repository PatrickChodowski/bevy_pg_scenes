use bevy::prelude::*;

#[derive(Event, Message)]
pub struct LoadScene {
    pub scene_path: String,
    pub maybe_loc:  Option<Vec3>
}

#[derive(Event, Message)]
pub struct LoadMap {
    pub map_path: String,
    pub maybe_loc: Option<Vec3>,
    pub load_terrains: bool,
    pub load_scenes: bool
}


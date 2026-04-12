use bevy::prelude::*;
use bevy::platform::collections::{HashMap, HashSet};
use bevy_pg_core::prelude::{GameState, GameStateTransition, Tile, TerrainChunk};

use crate::plane_scenes::PGPlaneScenesPlugin;
use crate::terrain_planes::PGScenesTerrainPlanesPlugin;

use crate::spawners_markers::{MSSettings, Marker, Spawner};

pub struct PGScenesPlugin {
    pub map_resolution: usize,
    pub markers_mapping: fn(name: String) -> Marker,
    pub spawners_mapping: fn(name: String, maybe_data: &Option<HashMap<String, String>>) -> Spawner
}

impl Plugin for PGScenesPlugin {
    fn build(&self, app: &mut App) {
        app
        .insert_resource(MSSettings{
            spawners_mapping: self.spawners_mapping,
            markers_mapping: self.markers_mapping
        })
        .add_plugins(PGPlaneScenesPlugin)
        .add_plugins(PGScenesTerrainPlanesPlugin)
        .insert_resource(CurrentChunk::new())
        ;
    }
}

#[derive(Resource)]
pub struct LoadStartScene {
    pub player_ready: bool
}

impl LoadStartScene {
    pub fn ready(&self) -> bool {
        self.player_ready
    }
}


#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub enum ScenesSet {
    MapsPlayer,
    Maps,
    Markers,
    Spawners
}


#[derive(Resource)]
pub struct SceneTransition {
    timer:         Timer,
    next_state:    GameState,
    set_scene:     SetScene
}

impl SceneTransition {
    pub fn new(
        next_state: GameState,
        set_scene: SetScene, 
        delay: f32
    ) -> Self {

        let st = SceneTransition {
            timer: Timer::from_seconds(delay, TimerMode::Once),
            next_state,
            set_scene: set_scene.clone()
        };
        return st;
    }
}


#[derive(Resource, Debug, Clone, Reflect)]
#[reflect(Resource)]
pub enum SetScene  {
    Start,
    Save(String),
}




#[derive(Resource, Reflect, Clone)]
#[reflect(Resource)]
pub struct CurrentChunk {
    pub x:                  usize,
    pub y:                  usize,
    pub visible:            Vec<Tile>,
    pub to_spawn:           HashSet<Tile>,
    pub to_despawn:         HashSet<Tile>,
    pub visited:            HashSet<Tile>,
    pub chunk_id:           String,
    pub map_name:           String
}

impl CurrentChunk {
    pub fn new() -> Self {
        CurrentChunk{
            x: 0, 
            y: 0, 
            to_spawn: HashSet::default(),
            to_despawn: HashSet::default(),
            visited: HashSet::default(),
            visible: vec![Tile::new(0, 0)],
            chunk_id: "000".into(), 
            map_name: "hedeby".into()
        }
    }

    pub fn reset(&mut self) {
        *self = CurrentChunk::new();
    }

    pub fn get_visible(
        &mut self, 
        map_resolution: usize,
        n_chunks: u8
    ){

        self.visible.clear();

        let mut start: i32 = -1;
        let mut end: i32 = 1;

        if map_resolution >= 1 && map_resolution % 2 == 1 {
            start = (map_resolution as i32 -1)/2 * -1;
            end = (map_resolution as i32 -1)/2;
        }

        for i in  start..=end {
            for j in start..=end {
                let new_x: i32 = self.x as i32 + i;
                let new_y: i32 = self.y as i32 + j;
                if new_x >= 0 && new_x < n_chunks as i32 && new_y >= 0 && new_y < n_chunks as i32{
                    self.visible.push(Tile::new(new_x as usize, new_y as usize));
                } 
            }
        }

    }

    // pub fn reset_tiles(&mut self, mapchunks: &Query<(Entity, &TerrainChunk)>) {
    //     let mut new_map_tiles: HashSet<Tile> = HashSet::default();
    //     let mut old_map_tiles: HashSet<Tile> = HashSet::default();
    //     let mut to_despawn: HashSet<Tile> = HashSet::default();
    //     for tile in self.visible.iter(){
    //         new_map_tiles.insert(*tile);
    //     }
    //     for (_entity, terrain_chunk) in mapchunks.iter(){
    //         old_map_tiles.insert(terrain_chunk.tile);
    //         if !new_map_tiles.contains(&terrain_chunk.tile){
    //             to_despawn.insert(terrain_chunk.tile);
    //         }
    //     }
    //     self.to_spawn = new_map_tiles.difference(&old_map_tiles).cloned().collect::<HashSet<Tile>>();
    //     self.to_despawn = to_despawn;

    //     for tile in self.to_spawn.iter(){
    //         self.visited.insert(*tile);
    //     }
    // }
}

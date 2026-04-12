use bevy::prelude::*;
use bevy::platform::collections::HashMap;
use bevy_pg_core::prelude::{GameState, GameStateTransition, Tile, TerrainChunk};

use crate::follower::PGSceneFollowerPlugin;
use crate::maps::{PGMapsPlugin, MapSettings};
use crate::plane_scenes::PGPlaneScenesPlugin;
use crate::terrain_planes::PGScenesTerrainPlanesPlugin;
use crate::spawners_markers::{MSSettings, Marker, Spawner};

pub struct PGScenesPlugin {
    pub map_resolution: u8,
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
        .insert_resource(MapSettings::new(self.map_resolution))
        .add_plugins(PGMapsPlugin)
        .add_plugins(PGPlaneScenesPlugin)
        .add_plugins(PGScenesTerrainPlanesPlugin)
        .add_plugins(PGSceneFollowerPlugin)
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

use bevy::prelude::*;
use bevy_common_assets::json::JsonAssetPlugin;
use bevy_pg_core::prelude::{GameState, GameStateTransition,AABB};

use crate::follower::PGSceneFollowerPlugin;
use crate::types::{PGMap, PGSerializedMesh, SceneData, CurrentChunk};
use crate::events::{LoadMap, LoadScene, LoadTerrain};
use crate::spawning::{spawn_terrain_plane, spawn_from_map, TrackLoadingMap, UpdateMesh, track_meshes, track_maps};

pub struct PGScenesPlugin;

impl Plugin for PGScenesPlugin {
    fn build(&self, app: &mut App) {
        app
        .add_message::<LoadMap>()
        .add_message::<LoadScene>()
        .add_message::<LoadTerrain>()
        .add_plugins(
            (
                PGSceneFollowerPlugin, 
                JsonAssetPlugin::<PGMap>::new(&["map.json"]),
                JsonAssetPlugin::<SceneData>::new(&["scene.json"]),
                JsonAssetPlugin::<PGSerializedMesh>::new(&["mesh.json"])
            )
        )
        .add_systems(Update, 
            (
                msg_load_map.run_if(on_message::<LoadMap>),
                msg_load_terrain_plane.run_if(on_message::<LoadMap>),
                track_maps.run_if(any_with_component::<TrackLoadingMap>),
                track_meshes.run_if(any_with_component::<UpdateMesh>),
                update_map_aabbs.run_if(resource_added::<PGMap>)

            )
        )

        .add_systems(OnEnter(GameStateTransition::UnloadScene),   unload_scene)
        .add_systems(OnEnter(GameStateTransition::LoadSceneData), load_scene_data)

        .add_observer(on_load_map)
        .add_observer(on_load_terrain_plane)

        ;
    }
}

#[derive(Resource)]
pub struct MapSettings {
    pub render_tiles: u8
}



#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub enum ScenesSet {
    MapsPlayer,
    Maps,
    Markers,
    Spawners
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


fn unload_scene(
    mut current_chunk:  ResMut<CurrentChunk>,
    mut next_gst:       ResMut<NextState<GameStateTransition>>
){
    // Clear all the resources
    info!(" [SCENES] Transition: Unload Scene");
    current_chunk.reset();
    next_gst.set(GameStateTransition::LoadSceneData)
}


fn load_scene_data(
    mut commands:   Commands,
    ass:            Res<AssetServer>,
    transition:     Res<SceneTransition>
) {

    info!(" [SCENES] Transition Loading new scene {:?}", transition.set_scene);
    match transition.set_scene {
        SetScene::Start => {
            commands.insert_resource(
                LoadStartScene{player_ready: false}
            );
        }
        SetScene::Save(ref save_name) => {
            // commands.insert_resource(
            //     LoadSaveResources {
            //         loaded_entities: false,
            //         loaded_resources: false,
            //         handle_save_entities:  ass.load(format!("saves/{}.scn.ron", save_name))
            // });
        }
    }
}


// Observers and systems


fn on_load_terrain_plane(
    trigger: On<LoadTerrain>,
    mut commands: Commands,
    ass:    Res<AssetServer>,
    mut materials: ResMut<Assets<StandardMaterial>>,
){
    info!("On Load Terrain Plane");
    spawn_terrain_plane(
        trigger.mesh_path.clone(), 
        trigger.maybe_loc, 
        &mut commands,
        &mut materials,
        &ass
    );
}


fn msg_load_terrain_plane(
    mut msgs: MessageReader<LoadTerrain>,
    mut commands: Commands,
    ass:    Res<AssetServer>,
    mut materials: ResMut<Assets<StandardMaterial>>,
){
    for msg in msgs.read(){
        spawn_terrain_plane(
            msg.mesh_path.clone(), 
            msg.maybe_loc, 
            &mut commands,
            &mut materials,
            &ass
        );
    }
}


fn update_map_aabbs(
     mut scenemap: ResMut<PGMap>
){  
    let dims = Vec2::splat(scenemap.tile_dim);
    for (_tile, entry) in scenemap.data.iter_mut(){
        entry.aabb = AABB::from_loc_dims(entry.loc.xz(), dims);
    }
}




fn on_load_map(
    trigger: On<LoadMap>,
    mut commands: Commands,
    ass:    Res<AssetServer>,
){
    spawn_from_map(
        trigger.map_path.clone(), 
        trigger.maybe_loc, 
        &mut commands,
        &ass,
        trigger.load_terrains,
        trigger.load_scenes
    );
}

fn msg_load_map(
    mut msgs: MessageReader<LoadMap>,
    mut commands: Commands,
    ass:    Res<AssetServer>
){
    for msg in msgs.read(){
        spawn_from_map(
            msg.map_path.clone(), 
            msg.maybe_loc, 
            &mut commands,
            &ass,
            msg.load_terrains,
            msg.load_scenes
        );
    }
}

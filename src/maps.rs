use bevy::prelude::*;
use bevy::platform::collections::HashMap;
use serde::{Serialize, Deserialize};
use bevy::asset::LoadState;
use bevy_pg_core::prelude::{Tile, AABB};
use bevy_common_assets::json::JsonAssetPlugin;

use crate::prelude::{LoadPlaneScene, LoadTerrainPlane};


pub struct PGMapsPlugin;

impl Plugin for PGMapsPlugin {
    fn build(&self, app: &mut App) {
        app
        .add_message::<LoadMap>()
        .add_plugins(JsonAssetPlugin::<SceneMap>::new(&["map.json"]))
        .add_systems(Update, msg_load_map.run_if(on_message::<LoadMap>))
        .add_observer(on_load_map)
        .add_systems(Update, track_pg_maps.run_if(any_with_component::<TrackLoadingMap>))
        .add_systems(Update, update_map_aabbs.run_if(resource_added::<SceneMap>))
        ;
    }
}

#[derive(Resource)]
pub(crate) struct MapSettings {
    pub(crate) render_tiles: u8
}

impl MapSettings {
    pub(crate) fn new(render_tiles: u8) -> Self {
        Self{render_tiles}
    }
}

impl Default for MapSettings {
    fn default() -> Self {
        MapSettings{render_tiles: 1}
    }
}


#[derive(Component)]
struct TrackLoadingMap {
    handle: Handle<SceneMap>,
    maybe_loc: Option<Vec3>,  // Origin of the scene. All objects transforms will be relative to it. 
    load_terrains: bool,
    load_scenes: bool,
    for_editor: bool
}

// Meant to be used with editor scenario only. Loads the whole map and puts it to the resources
fn track_pg_maps(
    mut commands:      Commands,
    ass:               Res<AssetServer>,
    pgmaps:            Res<Assets<SceneMap>>,
    query:             Query<(Entity, &TrackLoadingMap)>,
    mut spawn_terrain: MessageWriter<LoadTerrainPlane>,
    mut spawn_scene:   MessageWriter<LoadPlaneScene>
){
    for (entity, track_loading_map) in query.iter(){
        if let Some(load_state) = ass.get_load_state(&track_loading_map.handle){
            match load_state {
                LoadState::Failed(_) => {
                    warn!("Failed to Load pg map for entity: {}", entity);
                    commands.entity(entity).despawn();
                }
                LoadState::Loaded => {
                    info!("Loaded pg map for entity: {}", entity);
                    if let Some(map_data) = pgmaps.get(&track_loading_map.handle){

                        for (_tile, entry) in map_data.data.iter(){
                            if track_loading_map.load_terrains {
                                let mesh_path = format!("scenes/terrains/{}.mesh.json", entry.name);
                                spawn_terrain.write(LoadTerrainPlane{mesh_path, maybe_loc: None, for_editor: track_loading_map.for_editor});
                            }

                            if track_loading_map.load_scenes {
                                let scene_path = format!("scenes/scenes/{}.scene.json", entry.name);
                                spawn_scene.write(LoadPlaneScene{scene_path, maybe_loc: None, for_editor: track_loading_map.for_editor});
                            }
                        }
                        commands.entity(entity).despawn();
                        commands.insert_resource(map_data.clone());
                    }
                }
                _ => {}
            }
        }
    }
}


fn update_map_aabbs(
     mut scenemap: ResMut<SceneMap>
){  
    let dims = Vec2::splat(scenemap.tile_dim);
    for (_tile, entry) in scenemap.data.iter_mut(){
        entry.aabb = AABB::from_loc_dims(entry.loc.xz(), dims);
    }
}



fn spawn_from_map(
    path: String,
    maybe_loc: Option<Vec3>,
    commands: &mut Commands,
    ass: &Res<AssetServer>,
    load_terrains: bool,
    load_scenes: bool,
    for_editor: bool
){
    let scene_map_handle: Handle<SceneMap> = ass.load(path);
    commands.spawn(
        TrackLoadingMap{
            handle: scene_map_handle, 
            maybe_loc, 
            for_editor,
            load_terrains,
            load_scenes
        }
    );
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
        trigger.load_scenes,
        trigger.for_editor
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
            msg.load_scenes,
            msg.for_editor
        );
    }
}




#[derive(Event, Message)]
pub struct LoadMap {
    pub map_path: String,
    pub maybe_loc: Option<Vec3>,
    pub load_terrains: bool,
    pub load_scenes: bool,
    pub for_editor: bool
}



// Contains Scene names with positions
#[derive(Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath, Resource)]
pub struct SceneMap {
    pub tile_dim: f32, // These are always squares
    pub tiles_side: u8, // Number of tiles in one dimension
    pub data: HashMap<Tile, SceneMapEntry>,
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SceneMapEntry {
    pub loc: Vec3,
    // pub tile: Tile,
    pub name: String,
    #[serde(skip)]
    pub aabb:       AABB
}
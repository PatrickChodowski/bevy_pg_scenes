use bevy::prelude::*;
use bevy_common_assets::json::JsonAssetPlugin;
use bevy_pg_core::prelude::{GameState, GameStateTransition,AABB};

use crate::prelude::WaterData;
use crate::follower::{PGSceneFollowerPlugin};
use crate::types::{GameMap, MapMeta, SceneData, MapsData};
use crate::events::{LoadMap, LoadScene};
use crate::spawning::spawn_from_map;
use crate::water::WaterPlugin;
use crate::terrain::TerrainMaterialPlugin;
use crate::load_assets::collect_map_data;

pub struct PGScenesPlugin;

impl Plugin for PGScenesPlugin {
    fn build(&self, app: &mut App) {
        app
        .add_message::<LoadMap>()
        .add_message::<LoadScene>()
        .insert_resource(MapSettings{render_tiles: 3})
        .add_plugins(
            (
                PGSceneFollowerPlugin, 
                JsonAssetPlugin::<GameMap>::new(&["map.json"]),
                JsonAssetPlugin::<MapMeta>::new(&["meta.json"]),
                JsonAssetPlugin::<SceneData>::new(&["scene.json"]),
                WaterPlugin,
                TerrainMaterialPlugin::default(),
            )
        )
        .add_systems(Update, 
                (
                    update_map_aabbs
                ).run_if(resource_added::<GameMap>)
        )
        .add_systems(
            Update,
            collect_map_data.run_if(
                resource_exists::<GameMap>.and_then(not(resource_exists::<MapsData>)),
            ),
        )

        .add_systems(OnEnter(GameStateTransition::UnloadScene),   transition_unload_scene)
        .add_systems(OnEnter(GameStateTransition::LoadSceneData), transition_load_scene_data)
        .add_systems(OnEnter(GameStateTransition::LoadStatic), transition_load_static)
        .add_systems(OnEnter(GameStateTransition::LoadRest), transition_load_rest)
        .add_systems(Update, close_transition.run_if(in_state(GameStateTransition::Ready)))
        .add_systems(OnEnter(GameStateTransition::Done),  switch_state)
        .add_observer(on_load_map)

        ;
    }
}


fn transition_load_static(
    mut commands:           Commands,
    ass:                    Res<AssetServer>,
    mut meshes:             ResMut<Assets<Mesh>>,
    mapdata:                Res<GameMap>,
    // mapchunks:              Query<(Entity, &TerrainChunk)>,
    // scenechunks:            Query<(Entity, &SceneChunk)>,
    // waterchunks:            Query<(Entity, &WaterChunk)>,
    mut next_gst:           ResMut<NextState<GameStateTransition>>,
    // water_data:             Res<WaterData>
){
    info!("[SCENES] [LOAD_STATIC]");

    // info!("CurrentTile: {:?}", current_tile);

    // for (entity, terrainchunk) in mapchunks.iter(){
    //     if current_tile.to_despawn.contains(&terrainchunk.tile){
    //         info!("[Terrain] Despawning terrain {}: {}", terrainchunk.map_name, terrainchunk.chunk_id);
    //         commands.entity(entity).despawn();
    //     }
    // }
    // for (entity, scenechunk) in scenechunks.iter(){
    //     if current_tile.to_despawn.contains(&scenechunk.tile){
    //         info!("[Terrain] Despawning scene element");
    //         commands.entity(entity).despawn();
    //     }
    // }
    // for (entity, waterchunk) in waterchunks.iter(){
    //     if current_tile.to_despawn.contains(&waterchunk.tile){
    //         info!("[Terrain] Despawning water chunk");
    //         commands.entity(entity).despawn();
    //     }
    // }

    // for tile in current_tile.to_spawn.iter(){
    //     let Some(chunk) = mapdata.data.get(tile) else {continue;};
    //     let sd = scenes.data.get(&chunk.chunk_id).unwrap();
    //     // spawn_terrain_chunk(&mut commands, &ass, &chunk.map_name, tile, chunk, mapdata.chunk_size);
    //     let mut bundles = Vec::with_capacity(sd.objects.len());
    //     for (name, sods) in sd.objects.iter(){

    //         let asset_path = format!("objects/{}.glb", name);
    //         let mesh: Mesh3d = Mesh3d(ass.load(
    //             GltfAssetLabel::Primitive{primitive:0, mesh:0}.from_asset(asset_path.clone()),
    //         ));
    //         let material: MeshMaterial3d::<StandardMaterial> = MeshMaterial3d(ass.load(
    //             GltfAssetLabel::Material { index: 0, is_scale_inverted: false}.from_asset(asset_path)
    //         ));

    //         for sod in sods.iter(){
    //             bundles.push((
    //                 mesh.clone(), 
    //                 material.clone(),
    //                 sod.transform(), 
    //                 SceneChunk{tile: *tile}, 
    //                 name.clone(),
    //                 AssignComponents,
    //                 DespawnOnExit(GameState::Play)
    //             ));
    //         }
    //     }
    //     commands.spawn_batch(bundles);
    //     // commands.spawn(water_bundle(&mut meshes, &tile, &chunk, water_data.material.clone(), mapdata.chunk_size));
        
    // }
    next_gst.set(GameStateTransition::LoadRest);

}

fn transition_load_rest(
    mut next_gst: ResMut<NextState<GameStateTransition>>,
){
    next_gst.set(GameStateTransition::Ready);
}



fn close_transition(
    mut next_gst:       ResMut<NextState<GameStateTransition>>,
    mut transition:     ResMut<SceneTransition>,
    time:               Res<Time>
) {
    transition.timer.tick(time.delta());
    if transition.timer.is_finished() {
        next_gst.set(GameStateTransition::Done);
    }
}

fn switch_state(
    mut commands:    Commands,
    mut next_gs:     ResMut<NextState<GameState>>,
    transition:      Res<SceneTransition>
){
    info!(" [SCENES] Closing transition, setting game state to {:?}", transition.next_state);
    next_gs.set(transition.next_state);
    commands.remove_resource::<SceneTransition>();
}


#[derive(Resource)]
pub struct MapSettings {
    pub render_tiles: u8 // How many tiles to render around the player's tile
}



#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub enum ScenesSet {
    MapsPlayer,
    Maps,
    Markers,
    Spawners
}

#[derive(Resource)]
pub struct LoadPlayerScene {
    pub player_ready: bool
}

impl LoadPlayerScene {
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


fn transition_unload_scene(
    mut next_gst:      ResMut<NextState<GameStateTransition>>
){
    // Clear all the resources
    info!(" [SCENES] Transition: Unload Scene");
    next_gst.set(GameStateTransition::LoadSceneData)
}


fn transition_load_scene_data(
    mut commands:   Commands,
    ass:            Res<AssetServer>,
    transition:     Res<SceneTransition>
) {

    info!(" [SCENES] Transition Loading new scene {:?}", transition.set_scene);
    match transition.set_scene {
        SetScene::Start => {
            commands.insert_resource(
                LoadPlayerScene{player_ready: false}
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


fn update_map_aabbs(
     mut game_map: ResMut<GameMap>
){  
    for entry in game_map.data.iter_mut(){
        let dims = Vec2::splat(entry.tiles_size);
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

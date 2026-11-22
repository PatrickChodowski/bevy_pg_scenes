use bevy::prelude::*;

use crate::types::GameMap;



// // Meant to be used with editor scenario only. Loads the whole map and puts it to the resources
// pub(crate) fn track_maps(
//     mut commands:      Commands,
//     ass:               Res<AssetServer>,
//     pgmaps:            Res<Assets<GameMap>>,
//     query:             Query<(Entity, &TrackLoadingMap)>,
//     mut spawn_terrain: MessageWriter<LoadTerrain>,
//     mut spawn_scene:   MessageWriter<LoadScene>
// ){
//     info!("Track Maps");
//     for (entity, track_loading_map) in query.iter(){
//         if let Some(load_state) = ass.get_load_state(&track_loading_map.handle){
//             match load_state {
//                 LoadState::Failed(_) => {
//                     warn!("Failed to Load pg map for entity: {}", entity);
//                     commands.entity(entity).despawn();
//                 }
//                 LoadState::Loaded => {
//                     info!("Loaded pg map for entity: {}", entity);
//                     if let Some(map_data) = pgmaps.get(&track_loading_map.handle){

//                         for (_tile, entry) in map_data.data.iter(){
//                             if track_loading_map.load_terrains {
//                                 let mesh_path = format!("scenes/terrains/{}.mesh.json", entry.name);
//                                 spawn_terrain.write(LoadTerrain{mesh_path, maybe_loc: None});
//                             }

//                             if track_loading_map.load_scenes {
//                                 let scene_path = format!("scenes/scenes/{}.scene.json", entry.name);
//                                 spawn_scene.write(LoadScene{scene_path, maybe_loc: None});
//                             }
//                         }
//                         commands.entity(entity).despawn();
//                         commands.insert_resource(map_data.clone());
//                     }
//                 }
//                 _ => {}
//             }
//         }
//     }
// }



pub(crate) fn spawn_from_map(
    path: String,
    maybe_loc: Option<Vec3>,
    commands: &mut Commands,
    ass: &Res<AssetServer>,
    load_terrains: bool,
    load_scenes: bool
){
    let scene_map_handle: Handle<GameMap> = ass.load(path);
    commands.spawn(
        TrackLoadingMap{
            handle: scene_map_handle, 
            maybe_loc,
            load_terrains,
            load_scenes
        }
    );
}


#[derive(Component)]
pub(crate) struct TrackLoadingMap {
    handle: Handle<GameMap>,
    maybe_loc: Option<Vec3>,  // Origin of the scene. All objects transforms will be relative to it. 
    load_terrains: bool,
    load_scenes: bool
}
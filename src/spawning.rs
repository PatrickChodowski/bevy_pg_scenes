use bevy::prelude::*;
use bevy_pg_core::prelude::{GameState, TerrainChunk};
use bevy::asset::LoadState;

use crate::types::{PGMap, PGSerializedMesh};
use crate::events::{LoadTerrain, LoadMap, LoadScene};

use crate::water::depth::render_to_depth;



// Meant to be used with editor scenario only. Loads the whole map and puts it to the resources
pub(crate) fn track_maps(
    mut commands:      Commands,
    ass:               Res<AssetServer>,
    pgmaps:            Res<Assets<PGMap>>,
    query:             Query<(Entity, &TrackLoadingMap)>,
    mut spawn_terrain: MessageWriter<LoadTerrain>,
    mut spawn_scene:   MessageWriter<LoadScene>
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
                                spawn_terrain.write(LoadTerrain{mesh_path, maybe_loc: None});
                            }

                            if track_loading_map.load_scenes {
                                let scene_path = format!("scenes/scenes/{}.scene.json", entry.name);
                                spawn_scene.write(LoadScene{scene_path, maybe_loc: None});
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



pub (crate) fn track_meshes(
    mut commands:   Commands,
    ass:            Res<AssetServer>,
    pgmeshes:       Res<Assets<PGSerializedMesh>>,
    mut meshes:     ResMut<Assets<Mesh>>,
    query:          Query<(Entity, &UpdateMesh)>,
){
    for (entity, update_mesh) in query.iter(){
        if let Some(load_state) = ass.get_load_state(&update_mesh.handle){
            match load_state {
                LoadState::Failed(_) => {
                    warn!("Failed to Load pgserialized mesh for entity: {}", entity);
                    commands.entity(entity).despawn();
                }
                LoadState::Loaded => {
                    info!("Loaded pgserialized mesh for entity: {}", entity);
                    if let Some(pgmesh) = pgmeshes.get(&update_mesh.handle){
                        let terrain_mesh: Mesh = pgmesh.data.clone().into_mesh();
                        commands.entity(entity).insert(Mesh3d(meshes.add(terrain_mesh)));


                        if let Some(loc) = update_mesh.maybe_loc {
                            commands.entity(entity).insert(Transform::from_translation(loc));
                        } else {
                            commands.entity(entity).insert(Transform::from_translation(pgmesh.saved_loc));
                        }

                        commands.entity(entity).insert(Name::from(pgmesh.name.clone()));
                        commands.entity(entity).insert(TerrainChunk{dims: Vec2::new(pgmesh.width, pgmesh.height)});
                        commands.entity(entity).remove::<UpdateMesh>();
                    }
                }
                _ => {}
            }
        }
    }
}





pub(crate) fn spawn_from_map(
    path: String,
    maybe_loc: Option<Vec3>,
    commands: &mut Commands,
    ass: &Res<AssetServer>,
    load_terrains: bool,
    load_scenes: bool
){
    let scene_map_handle: Handle<PGMap> = ass.load(path);
    commands.spawn(
        TrackLoadingMap{
            handle: scene_map_handle, 
            maybe_loc,
            load_terrains,
            load_scenes
        }
    );
}


pub(crate) fn spawn_terrain_plane(
    path: String,
    maybe_loc: Option<Vec3>,
    commands: &mut Commands,
    materials: &mut ResMut<Assets<StandardMaterial>>,
    ass: &Res<AssetServer>
){

    let terrain_plane_entity = commands.spawn((
        // Mesh3d(terrain_scene_mesh),
        // MeshMaterial3d(terrain_scene_mat),
        MeshMaterial3d(materials.add(StandardMaterial::from(Color::WHITE))),
        DespawnOnExit(GameState::Play),
        render_to_depth(),
        Pickable::IGNORE
    )).id();


    let handle_serialized_mesh: Handle<PGSerializedMesh> = ass.load(path);
    commands.entity(terrain_plane_entity).insert(UpdateMesh{
        maybe_loc,
        handle: handle_serialized_mesh
    });

}




#[derive(Component)]
pub(crate) struct UpdateMesh {
    handle: Handle<PGSerializedMesh>,
    maybe_loc: Option<Vec3>
}

#[derive(Component)]
pub(crate) struct TrackLoadingMap {
    handle: Handle<PGMap>,
    maybe_loc: Option<Vec3>,  // Origin of the scene. All objects transforms will be relative to it. 
    load_terrains: bool,
    load_scenes: bool
}
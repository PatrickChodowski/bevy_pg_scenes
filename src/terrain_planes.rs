use bevy::prelude::*;
use bevy::mesh::Mesh;
use serde::{Serialize, Deserialize};
use bevy_pg_core::prelude::{GameStateTransition, GameState, AABB, Player, TerrainChunk, Tile};
use bevy::mesh::SerializedMesh;
use bevy::gltf::GltfLoaderSettings;
use bevy::asset::{LoadState, RenderAssetUsages};
use bevy_pg_nav::prelude::{PGNavmesh, GenerateNavMesh};
use bevy::platform::collections::{HashMap, HashSet};
use bevy::math::Vec3;
use bevy_common_assets::json::JsonAssetPlugin;

use crate::water::depth::render_to_depth;



#[derive(Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct PGSerializedMesh {
    pub name: String,
    pub width: f32,
    pub height: f32,
    pub subdivisions: u32,
    pub data: SerializedMesh
}

#[derive(Component)]
struct UpdateMesh {
    handle: Handle<PGSerializedMesh>,
    for_editor: bool
}

#[derive(Event, Message)]
pub struct LoadTerrainPlane {
    pub mesh_path: String,
    pub loc: Vec3,
    pub for_editor: bool
}


pub struct PGScenesTerrainPlanesPlugin;

impl Plugin for PGScenesTerrainPlanesPlugin {
    fn build(&self, app: &mut App) {
        app
        .add_message::<LoadTerrainPlane>()
        .add_plugins(JsonAssetPlugin::<PGSerializedMesh>::new(&["mesh.json"]))
        .add_systems(Update, msg_load_terrain_plane.run_if(on_message::<LoadTerrainPlane>))
        .add_observer(on_load_terrain_plane)
        .add_systems(Update, track_pg_meshes.run_if(any_with_component::<UpdateMesh>))
        ;
    }
}

fn spawn_terrain_plane(
    path: String,
    loc: Vec3,
    commands: &mut Commands,
    ass: &Res<AssetServer>,
    for_editor: bool
){

    let terrain_plane_entity = commands.spawn((
        // Mesh3d(terrain_scene_mesh),
        // MeshMaterial3d(terrain_scene_mat),
        Transform::from_translation(loc),
        DespawnOnExit(GameState::Play),
        render_to_depth()
    )).id();

    if for_editor {
        commands.entity(terrain_plane_entity).insert(Pickable{should_block_lower: true, ..default()});
    } else {
        commands.entity(terrain_plane_entity).insert(Pickable::IGNORE);
    }

    let handle_serialized_mesh: Handle<PGSerializedMesh> = ass.load(path);
    commands.entity(terrain_plane_entity).insert(UpdateMesh{
        handle: handle_serialized_mesh,
        for_editor: for_editor
    });

}



fn track_pg_meshes(
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
                        if update_mesh.for_editor {
                            commands.entity(entity).insert(PlaneToEdit::new(pgmesh.width, pgmesh.height, pgmesh.subdivisions));
                        }
                        commands.entity(entity).insert(Name::from(pgmesh.name.clone()));
                        commands.entity(entity).remove::<UpdateMesh>();
                    }
                }
                _ => {}
            }
        }
    }
}


fn on_load_terrain_plane(
    trigger: On<LoadTerrainPlane>,
    mut commands: Commands,
    ass:    Res<AssetServer>
){
    spawn_terrain_plane(
        trigger.mesh_path.clone(), 
        trigger.loc, 
        &mut commands,
        &ass,
        trigger.for_editor
    );
}


fn msg_load_terrain_plane(
    mut msgs: MessageReader<LoadTerrainPlane>,
    mut commands: Commands,
    ass:    Res<AssetServer>
){
    for msg in msgs.read(){
        spawn_terrain_plane(
            msg.mesh_path.clone(), 
            msg.loc, 
            &mut commands,
            &ass,
            msg.for_editor
        );
    }
}




#[derive(Component)]
pub struct PlaneToEdit{
    pub width: f32,
    pub height: f32,
    pub subdivisions: u32,
    pub changes: u32 
}

impl PlaneToEdit {
    pub fn new(width: f32, height: f32, subdivisions: u32) -> Self {
        PlaneToEdit {
            width, height, subdivisions, changes: 0
        }
    }

    pub fn dims(&self) -> Vec2 {
        return Vec2::new(self.width, self.height);
    }

    pub fn calculate_optimal_vertex_radius(&self, percentage: f32) -> f32 {
        let spacing_x = self.height / (self.subdivisions+1).max(1) as f32;
        let spacing_y = self.width / (self.subdivisions+1).max(1) as f32;
        let min_spacing = spacing_x.min(spacing_y);
        let max_radius = min_spacing / 2.0;
        let safe_fill = percentage.clamp(0.01, 0.99);
        max_radius * safe_fill
    }
}

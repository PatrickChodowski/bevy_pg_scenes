use bevy::prelude::*;
use bevy::platform::collections::HashMap;
use serde::{Serialize, Deserialize};
use bevy::asset::LoadState;
use bevy_pg_core::prelude::GameState;
use bevy_common_assets::json::JsonAssetPlugin;

use crate::spawners_markers::MSSettings;

pub struct PGPlaneScenesPlugin;

impl Plugin for PGPlaneScenesPlugin {
    fn build(&self, app: &mut App) {
        app
        .add_message::<LoadPlaneScene>()
        .add_plugins(JsonAssetPlugin::<SceneData>::new(&["scene.json"]))
        .add_systems(Update, msg_load_plane_scene.run_if(on_message::<LoadPlaneScene>))
        .add_observer(on_load_plane_scene)
        .add_systems(Update, track_pg_scenes.run_if(any_with_component::<TrackLoadingScene>))
        ;
    }
}




fn track_pg_scenes(
    mut commands:   Commands,
    ass:            Res<AssetServer>,
    pgscenes:       Res<Assets<SceneData>>,
    query:          Query<(Entity, &TrackLoadingScene)>,
    sm_settings:    Res<MSSettings>
){
    for (entity, track_loading_scene) in query.iter(){
        if let Some(load_state) = ass.get_load_state(&track_loading_scene.handle){
            match load_state {
                LoadState::Failed(_) => {
                    warn!("Failed to Load pg scene for entity: {}", entity);
                    commands.entity(entity).despawn();
                }
                LoadState::Loaded => {
                    info!("Loaded pg scene for entity: {}", entity);
                    if let Some(scene_data) = pgscenes.get(&track_loading_scene.handle){
                        scene_data.spawn(track_loading_scene.maybe_loc, &mut commands, &ass, &sm_settings);
                        commands.entity(entity).remove::<TrackLoadingScene>();
                    }
                }
                _ => {}
            }
        }
    }
}


#[derive(Event, Message)]
pub struct LoadPlaneScene {
    pub scene_path: String,
    pub maybe_loc: Option<Vec3>,
    pub for_editor: bool
}

#[derive(Component)]
struct TrackLoadingScene {
    handle: Handle<SceneData>,
    maybe_loc: Option<Vec3>,  // Origin of the scene. All objects transforms will be relative to it. 
    for_editor: bool
}

#[derive(Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct SceneData {
    pub saved_loc:   Vec3,  // Origin point of scene
    pub objects:  HashMap<Name, Vec<SceneObjectData>>
}

#[derive(Deserialize, Serialize, Debug, Clone, Copy, Component)]
pub struct AssignComponents;

#[derive(Reflect, Debug)]
pub enum AssetSourceType{
    MeshMaterial,
    Scene
}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
pub struct AssetSource {
    pub path: String,
    pub typ:  AssetSourceType
}

impl AssetSource {
    pub fn new_mm(path: String) -> Self {
        AssetSource{path, typ: AssetSourceType::MeshMaterial}
    }
    pub fn new_scene(path: String) -> Self {
        AssetSource{path, typ: AssetSourceType::Scene}
    }
}

// All non moving/non spawnees entities: Buildings, Props
#[derive(Component, Reflect)]
pub struct Static;



impl SceneData {
    fn spawn(
        &self, 
        maybe_origin:    Option<Vec3>,
        commands:        &mut Commands, 
        ass:             &Res<AssetServer>, 
        scenes_settings: &Res<MSSettings>
    ) {

        let scene_origin: Vec3;
        if let Some(origin) = maybe_origin {
            scene_origin = origin;
        } else {
            scene_origin = self.saved_loc;
        };


        let mut object_bundles = Vec::with_capacity(self.objects.len());
        let mut spawner_bundles = Vec::with_capacity(self.objects.len());
        let mut marker_bundles = Vec::with_capacity(self.objects.len());

        for (name, sods) in self.objects.iter(){
            if !(name.contains("Spawner_") | name.contains("Marker_")){
                let asset_path = format!("objects/{}.glb", name);
                let mesh: Handle<Mesh> = ass.load(
                    GltfAssetLabel::Primitive{primitive:0, mesh:0}.from_asset(asset_path.clone()),
                );
                let material: Handle<StandardMaterial> = ass.load(
                    GltfAssetLabel::Material { index: 0, is_scale_inverted: false}.from_asset(asset_path.clone())
                );
                for sod in sods.iter(){
                    object_bundles.push((
                        Mesh3d(mesh.clone()), 
                        MeshMaterial3d(material.clone()), 
                        sod.transform(&scene_origin), 
                        AssignComponents,
                        DespawnOnExit(GameState::Play),
                        name.clone(),
                        AssetSource::new_mm(asset_path.clone())
                    ));
                }
            } else if name.contains("Spawner_") {
                for sod in sods.iter(){
                    spawner_bundles.push(
                        (
                            sod.transform(&scene_origin),
                            (scenes_settings.spawners_mapping)(name.to_string(), &sod.data),
                            DespawnOnExit(GameState::Play),
                            name.clone()
                        )
                    );
                }
            } else if name.contains("Marker_") {
                for sod in sods.iter(){
                    marker_bundles.push(
                        (
                            sod.transform(&scene_origin),
                            (scenes_settings.markers_mapping)(name.to_string()),
                            DespawnOnExit(GameState::Play),
                            name.clone()
                        )
                    );
                }
            }
        }

        commands.spawn_batch(object_bundles);
        commands.spawn_batch(spawner_bundles);
        commands.spawn_batch(marker_bundles);
    }
}


#[derive(Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct SceneObjectData {
    pub location:   Vec3,
    pub rotation:   Vec3,
    pub scale:      Vec3,
    pub data:       Option<HashMap<String,String>>  // Hold NPC Type etc.
} 
impl SceneObjectData {
    pub fn transform(&self, origin: &Vec3) -> Transform {
        Transform::from_translation(self.location + origin)
                  .with_rotation(Quat::from_euler(EulerRot::XYZ, self.rotation.x, self.rotation.y, self.rotation.z))
                  .with_scale(self.scale)
    }
}

fn spawn_plane_scene(
    path: String,
    maybe_loc: Option<Vec3>,
    commands: &mut Commands,
    ass: &Res<AssetServer>,
    for_editor: bool
){
    let scene_data_handle: Handle<SceneData> = ass.load(path);
    commands.spawn(TrackLoadingScene{handle: scene_data_handle, maybe_loc, for_editor});
}

fn on_load_plane_scene(
    trigger: On<LoadPlaneScene>,
    mut commands: Commands,
    ass:    Res<AssetServer>,
){
    spawn_plane_scene(
        trigger.scene_path.clone(), 
        trigger.maybe_loc, 
        &mut commands,
        &ass,
        trigger.for_editor
    );
}

fn msg_load_plane_scene(
    mut msgs: MessageReader<LoadPlaneScene>,
    mut commands: Commands,
    ass:    Res<AssetServer>
){
    for msg in msgs.read(){
        spawn_plane_scene(
            msg.scene_path.clone(), 
            msg.maybe_loc, 
            &mut commands,
            &ass,
            msg.for_editor
        );
    }
}

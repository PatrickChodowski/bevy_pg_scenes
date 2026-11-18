use bevy::prelude::*;
use bevy::platform::collections::HashMap;
use serde::{Serialize, Deserialize};
use bevy_pg_core::prelude::EditorAsset;
use bevy_common_assets::json::JsonAssetPlugin;

pub struct PGPlaneScenesPlugin;

impl Plugin for PGPlaneScenesPlugin {
    fn build(&self, app: &mut App) {
        app
        .add_plugins(JsonAssetPlugin::<SceneData>::new(&["scene.json"]))
        ;
    }
}


#[derive(Event, Message)]
pub struct LoadPlaneScene {
    pub scene_path: String,
    pub maybe_loc: Option<Vec3>,
    pub for_editor: bool
}


#[derive(Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct SceneData {
    pub saved_loc:   Vec3,  // Origin point of scene
    pub objects:  HashMap<Name, Vec<SceneObjectData>>
}

#[derive(Deserialize, Serialize, Debug, Clone, Copy, Component, Default)]
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

// All non moving/non spawnees entities: Buildings, Props, Markers, Water
#[derive(Component, Reflect, Default, Clone)]
pub struct Static;


#[derive(Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct SceneObjectData {
    pub editor_asset: EditorAsset,
    pub location:     Vec3,
    pub rotation:     Vec3,
    pub scale:        Vec3,
    pub data:         Option<HashMap<String,String>>  // Hold NPC Type etc.
} 
impl SceneObjectData {
    pub fn transform(&self, origin: &Vec3) -> Transform {
        Transform::from_translation(self.location + origin)
                  .with_rotation(Quat::from_euler(EulerRot::XYZ, self.rotation.x, self.rotation.y, self.rotation.z))
                  .with_scale(self.scale)
    }
}

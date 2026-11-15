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

// #[derive(Deserialize, Serialize, Debug, Clone, Copy, Component, Default)]
// pub struct AssignComponents;

// #[derive(Reflect, Debug)]
// pub enum AssetSourceType{
//     MeshMaterial,
//     Scene
// }

// #[derive(Component, Reflect, Debug)]
// #[reflect(Component)]
// pub struct AssetSource {
//     pub path: String,
//     pub typ:  AssetSourceType
// }

// impl AssetSource {
//     pub fn new_mm(path: String) -> Self {
//         AssetSource{path, typ: AssetSourceType::MeshMaterial}
//     }
//     pub fn new_scene(path: String) -> Self {
//         AssetSource{path, typ: AssetSourceType::Scene}
//     }
// }

// All non moving/non spawnees entities: Buildings, Props, Markers, Water
#[derive(Component, Reflect, Default, Clone)]
pub struct Static;



// impl SceneData {

//     // Spawning scene from the scene file. All elements will be spawned from here 
//     pub fn spawn(
//         &self, 
//         maybe_origin:    Option<Vec3>,
//         commands:        &mut Commands, 
//         meshes:          &mut ResMut<Assets<Mesh>>,
//         materials:       &mut ResMut<Assets<StandardMaterial>>,
//         water_material:  &Handle<WaterMaterial>,
//         ass:             &Res<AssetServer>, 
//         scenes_settings: &Res<MSSettings>,
//         for_editor: bool
//     ) {

//         let scene_origin: Vec3;
//         if let Some(origin) = maybe_origin {
//             scene_origin = origin;
//         } else {
//             scene_origin = self.saved_loc;
//         };


//         let mut object_bundles = Vec::with_capacity(self.objects.len());
//         let mut spawner_bundles = Vec::with_capacity(100);
//         let mut marker_bundles = Vec::with_capacity(100);

//         for (name, sods) in self.objects.iter(){
//             if !(name.contains("Spawner_") | name.contains("Marker_") | (name.as_str() == "water")){
//                 let asset_path = format!("objects/{}.glb", name);
//                 let mesh: Handle<Mesh> = ass.load(
//                     GltfAssetLabel::Primitive{primitive:0, mesh:0}.from_asset(asset_path.clone()),
//                 );
//                 let material: Handle<StandardMaterial> = ass.load(
//                     GltfAssetLabel::Material { index: 0, is_scale_inverted: false}.from_asset(asset_path.clone())
//                 );
//                 for sod in sods.iter(){
//                     object_bundles.push((
//                         Mesh3d(mesh.clone()), 
//                         MeshMaterial3d(material.clone()), 
//                         sod.transform(&scene_origin), 
//                         AssignComponents,
//                         DespawnOnExit(GameState::Play),
//                         name.clone(),
//                         AssetSource::new_mm(asset_path.clone()),
//                         EditorAsset::Scene(name.to_string()),
//                         Pickable{should_block_lower: true, ..default()},
//                         Static
//                     ));
//                 }
//             } else if name.contains("Spawner_") {
//                 for sod in sods.iter(){
//                     spawner_bundles.push(
//                         (
//                             sod.transform(&scene_origin),
//                             (scenes_settings.spawners_mapping)(name.to_string(), &sod.data),
//                             DespawnOnExit(GameState::Play),
//                             EditorAsset::Spawner(name.to_string()),
//                             name.clone(),
//                             Pickable{should_block_lower: true, ..default()},
//                             Static
//                         )
//                     );
//                 }
//             } else if name.contains("Marker_") {
//                 for sod in sods.iter(){
//                     marker_bundles.push(
//                         (
//                             sod.transform(&scene_origin),
//                             (scenes_settings.markers_mapping)(name.to_string()),
//                             DespawnOnExit(GameState::Play),
//                             EditorAsset::Marker(name.to_string()),
//                             name.clone(),
//                             Pickable{should_block_lower: true, ..default()},
//                             Static
//                         )
//                     );
//                 }
//             } else if name.as_str() == "water" {
//                 for sod in sods.iter(){
//                     let dims = sod.scale.xz();
//                     spawn_water(commands, meshes, materials, water_material.clone(), &sod.location, &dims, for_editor);
//                 }
//             }
//         }

//         commands.spawn_batch(object_bundles);
//         commands.spawn_batch(spawner_bundles);
//         commands.spawn_batch(marker_bundles);
//     }
// }


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

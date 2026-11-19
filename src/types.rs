use bevy::prelude::*;
use bevy::platform::collections::HashMap;
use serde::{Serialize, Deserialize};
use bevy::mesh::SerializedMesh;
use bevy_pg_core::prelude::{EditorAsset, Tile, AABB};



// Contains Scene names with positions
#[derive(Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath, Resource)]
pub struct PGMap {
    pub tile_dim: f32, // These are always squares
    pub tiles_side: u8, // Number of tiles in one dimension
    pub data: HashMap<Tile, PGMapEntry>,
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PGMapEntry {
    pub loc: Vec3,
    // pub tile: Tile,
    pub name: String,
    #[serde(skip)]
    pub aabb:       AABB
}


#[derive(Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct PGSerializedMesh {
    pub saved_loc:  Vec3,
    pub name: String,
    pub width: f32,
    pub height: f32,
    pub subdivisions: u32,
    pub data: SerializedMesh
}

impl PGSerializedMesh {
    pub fn load(path: &str) -> std::io::Result<(Mesh, f32, f32, u32, String)> {
        let json = std::fs::read_to_string(path)?;
        let serialized: PGSerializedMesh = serde_json::from_str(&json)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
        return Ok((
            serialized.data.into_mesh(), 
            serialized.width, 
            serialized.height, 
            serialized.subdivisions, 
            serialized.name
        ));
    }
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


#[derive(Component, Reflect)]
pub struct Spawnee;


#[derive(Component, Reflect, Clone)]
pub struct Spawner {
    pub id:     usize,
    pub data:   HashMap<String, String>, // contain any key value data that might be useful to pass from editor to game
    pub active: bool
}

impl Spawner {
    pub fn new(id: usize) -> Self {
        Spawner{id, active: true, data: HashMap::new()}
    }
    pub fn add(&mut self, k: String, v: String) -> Self {
        self.data.insert(k, v);
        self.clone()
    }
}

impl Default for Spawner {
    fn default() -> Self {
        Spawner { id: 0, data: HashMap::new(), active: true}
    }
}



#[derive(Component, Reflect, Clone)]
pub struct Marker {
    pub id: usize,
    pub data:   HashMap<String, String>, // contain any key value data that might be useful to pass from editor to game
    pub processed: bool
}
#[derive(Component, Reflect)]
pub struct Markee; // Result of Markers processing

impl Marker {
    pub fn new(id: usize) -> Self {
        Marker{id, processed: false, data: HashMap::new()}
    }
    pub fn add(&mut self, k: String, v: String) -> Self {
        self.data.insert(k, v);
        self.clone()
    }

}

impl Default for Marker {
    fn default() -> Self {
        Marker { id: 0, processed: false, data: HashMap::new()}
    }
}

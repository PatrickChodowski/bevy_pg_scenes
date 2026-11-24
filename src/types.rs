use bevy::prelude::*;
use bevy::platform::collections::{HashMap, HashSet};
use serde::{Serialize, Deserialize};
use bevy::mesh::SerializedMesh;
use bevy_pg_core::prelude::{EditorAsset, Tile, AABB, TerrainChunk};



#[derive(Resource)]
pub struct LoadSaveResources {
    pub loaded_entities:        bool,
    pub loaded_resources:       bool,
    pub handle_save_entities:   Handle<DynamicWorld>
}

impl LoadSaveResources {
    pub fn ready(&self) -> bool {
        self.loaded_entities & self.loaded_resources
    }
}

#[derive(Component, Default, Clone)]
pub struct StreamingFocus;


#[derive(Serialize, Deserialize, Clone, Debug, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct MapChunk {
    pub name: String,
    pub terrain_file: String,
    pub scene_file: String,
    pub location: [f32; 3],
    pub local_x_index: u32,
    pub local_y_index: u32,
}

#[derive(Serialize, Deserialize, Clone, Debug, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct MapMeta {
    pub name: String,
    pub chunks_count_x: u32,
    pub chunks_count_y: u32,
    pub chunk_width: f32,
    pub chunk_height: f32,
    pub chunks: Vec<MapChunk>,
    /// Terrain texture weights for the whole map (next to the chunk meshes in terrain/).
    /// None = unpainted. Chunk meshes keep the full plane's UVs, so they all sample this one image.
    #[serde(default)]
    pub splat_file: Option<String>,
    /// Weights of layers 4..7 (second splat texture). None = none of those layers is painted.
    #[serde(default)]
    pub splat_file_2: Option<String>,
    /// World position of the plane the map was exported from. Chunk `location`s and scene objects
    /// are relative to it, so several maps (e.g. heightmap tiles) can sit side by side.
    /// Navmeshes are already in world space. Missing in older exports = the origin.
    #[serde(default)]
    pub origin: [f32; 3],
    /// Terrain as one heightmap; chunk meshes are built from it at load time. None = older
    /// export with one mesh file per chunk (`MapChunk::terrain_file`).
    #[serde(default)]
    pub heightmap: Option<crate::terrain::heightmap::MapHeightmap>,
    /// Navmesh kinds this map has files for ("terrain", "water"), kept up to date by the editor.
    /// None = older export: the game tries every kind.
    #[serde(default)]
    pub navmeshes: Option<Vec<String>>,
}

#[derive(Resource, Default)]
pub struct MapsData {
    pub data: HashMap<String, MapMeta>
}

// Contains Scene names with positions
// GameMap holds information in what position what map/scene should load
//
#[derive(Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath, Resource)]
pub struct GameMap {
    pub data: Vec<GameMapEntry>,
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GameMapEntry {
    pub loc: Vec3,
    pub tiles_count: u8, // Number of tiles in one axis (total_tiles_count = tiles_count^2). Its Square
    pub tiles_size: f32, // Single tile size (width=height)
    // pub tile: Tile,
    pub name: String,
    #[serde(skip)]
    pub aabb:       AABB
}


/// DEPRECATED: the old whole-plane `{name}.mesh.json` export. Maps are exported as heightmaps
/// now (`MapMeta::heightmap`); this only remains so the editor can open old exports to
/// re-export them.
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

// Marker kinds, shared by the editor and the game. An `EditorAsset::Marker(name)` becomes a
// `Marker` with the id for its name.
pub const MARKER_LOCATION: usize = 0;
pub const MARKER_ENTRYPORT: usize = 1;
pub const MARKER_EXITPORT: usize = 2;

/// `Marker::id` for an `EditorAsset::Marker` name; None for an unknown name.
pub fn marker_id(name: &str) -> Option<usize> {
    match name {
        "Marker_Locations" => Some(MARKER_LOCATION),
        "Marker_EntryPort" => Some(MARKER_ENTRYPORT),
        "Marker_ExitPort" => Some(MARKER_EXITPORT),
        _ => None,
    }
}

/// A marker's radius in world units: its X scale. The editor draws it as a cylinder of
/// radius 1 scaled by its transform, so what it shows is what the game uses.
pub fn marker_radius(scale: Vec3) -> f32 {
    scale.x
}

// Keys of `Marker::data` (and of the marker's `SceneObjectData::data`).
/// The location's name.
pub const MARKER_NAME: &str = "Name";
/// Settlement tier (1 = major centre).
pub const MARKER_TIER: &str = "tier";
/// Who placed it: "settlements" for markers placed from the settlement list (the editor's bulk
/// import replaces those), absent for hand-placed ones.
pub const MARKER_SOURCE: &str = "source";

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

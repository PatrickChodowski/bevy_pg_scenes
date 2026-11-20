use bevy::prelude::*;
use bevy::platform::collections::{HashMap, HashSet};
use serde::{Serialize, Deserialize};
use bevy::mesh::SerializedMesh;
use bevy_pg_core::prelude::{EditorAsset, Tile, AABB, TerrainChunk};


#[derive(Resource, Reflect, Clone)]
#[reflect(Resource)]
pub struct CurrentChunk {
    pub x:                  usize,
    pub y:                  usize,
    pub visible:            Vec<Tile>,
    pub to_spawn:           HashSet<Tile>,
    pub to_despawn:         HashSet<Tile>,
    pub visited:            HashSet<Tile>,
    pub chunk_id:           String,
    pub map_name:           String
}

impl CurrentChunk {
    pub fn new() -> Self {
        CurrentChunk{
            x: 0, 
            y: 0, 
            to_spawn: HashSet::default(),
            to_despawn: HashSet::default(),
            visited: HashSet::default(),
            visible: vec![Tile::new(0, 0)],
            chunk_id: "000".into(), 
            map_name: "hedeby".into()
        }
    }

    pub fn reset(&mut self) {
        *self = CurrentChunk::new();
    }

    pub fn get_visible(
        &mut self, 
        map_resolution: usize,
        n_chunks: u8
    ){

        self.visible.clear();

        let mut start: i32 = -1;
        let mut end: i32 = 1;

        if map_resolution >= 1 && map_resolution % 2 == 1 {
            start = (map_resolution as i32 -1)/2 * -1;
            end = (map_resolution as i32 -1)/2;
        }

        for i in  start..=end {
            for j in start..=end {
                let new_x: i32 = self.x as i32 + i;
                let new_y: i32 = self.y as i32 + j;
                if new_x >= 0 && new_x < n_chunks as i32 && new_y >= 0 && new_y < n_chunks as i32{
                    self.visible.push(Tile::new(new_x as usize, new_y as usize));
                } 
            }
        }

    }

    pub fn reset_tiles(&mut self, mapchunks: &Query<(Entity, &TerrainChunk)>) {
        let mut new_map_tiles: HashSet<Tile> = HashSet::default();
        let mut old_map_tiles: HashSet<Tile> = HashSet::default();
        let mut to_despawn: HashSet<Tile> = HashSet::default();
        for tile in self.visible.iter(){
            new_map_tiles.insert(*tile);
        }
        for (_entity, terrain_chunk) in mapchunks.iter(){
            old_map_tiles.insert(terrain_chunk.tile);
            if !new_map_tiles.contains(&terrain_chunk.tile){
                to_despawn.insert(terrain_chunk.tile);
            }
        }
        self.to_spawn = new_map_tiles.difference(&old_map_tiles).cloned().collect::<HashSet<Tile>>();
        self.to_despawn = to_despawn;

        for tile in self.to_spawn.iter(){
            self.visited.insert(*tile);
        }
    }
}





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

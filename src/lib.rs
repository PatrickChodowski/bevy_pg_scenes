pub mod follower;
mod water;

pub mod spawning;
pub mod plugin;
pub mod events;
pub mod types;
pub mod load_assets;


pub mod prelude {
    pub use crate::types::{SceneData, SceneObjectData, Static, AssetSource, AssetSourceType, AssignComponents, PGSerializedMesh, 
        Markee, Marker, Spawnee, Spawner, MapMeta, MapsData, MapChunk, GameMap, GameMapEntry
    };
    pub use crate::events::{LoadMap, LoadTerrain, LoadScene};
    pub use crate::plugin::{PGScenesPlugin, ScenesSet, LoadPlayerScene, SceneTransition, SetScene, MapSettings};
    pub use crate::water::depth::{Wakes, WakeHitter, render_to_depth};
    pub use crate::water::{WaterPlugin, WaveUpdate, WaterData, WaterMaterial, WaterMaterialExtension, nice_water, simple_water};
}

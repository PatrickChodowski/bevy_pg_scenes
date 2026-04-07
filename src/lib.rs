pub mod scenes;
pub mod terrain_planes;
mod water;

pub mod prelude {
    pub use crate::scenes::{PGScenesPlugin, PGScenesSettings, LoadStartScene, 
        AssignComponents, Static, Spawnee, 
        SceneTransition, MapsData, SceneData, Scenes, SetScene,
        CurrentChunk, Marker, Spawner, Markee, ScenesSet, 
        SceneObjectData, AssetSource
    };
    pub use crate::terrain_planes::{PGSerializedMesh, LoadTerrainPlane, PlaneToEdit};
    pub use crate::water::depth::{Wakes, WakeHitter, render_to_depth};
    pub use crate::water::{WaterPlugin, WaveUpdate, WaterChunk};
}

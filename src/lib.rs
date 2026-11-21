pub mod follower;
mod water;

pub mod spawning;
pub mod plugin;
pub mod events;
pub mod types;




pub mod prelude {
    pub use crate::follower::CurrentTile;
    pub use crate::types::{SceneData, SceneObjectData, Static, AssetSource, AssetSourceType, AssignComponents, PGSerializedMesh, Markee, Marker, Spawnee, Spawner};
    pub use crate::events::{LoadMap, LoadTerrain, LoadScene};
    pub use crate::plugin::{PGScenesPlugin, ScenesSet, LoadStartScene, SceneTransition, SetScene, MapSettings};
    // pub use crate::maps::LoadMap;

    


    // pub use crate::scenes::{PGScenesPlugin, SetScene, SceneTransition, ScenesSet, LoadStartScene};

    // pub use crate::scenes::{PGScenesPlugin, PGScenesSettings, 
    //     LoadStartScene, Static, SceneTransition, MapsData, SetScene, ScenesSet
    // };
    pub use crate::water::depth::{Wakes, WakeHitter, render_to_depth};
    pub use crate::water::{WaterPlugin, WaveUpdate, WaterData, WaterMaterial, WaterMaterialExtension, nice_water, simple_water};
}

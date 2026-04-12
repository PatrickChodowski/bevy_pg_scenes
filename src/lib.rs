pub mod plane_scenes;
pub mod scenes;
pub mod spawners_markers;
pub mod terrain_planes;
mod water;

pub mod prelude {
    pub use crate::spawners_markers::{Markee, Marker, Spawnee, Spawner};
    
    pub use crate::plane_scenes::{AssetSource, AssetSourceType, 
        AssignComponents, SceneData, SceneObjectData, LoadPlaneScene, Static
    };

    pub use crate::scenes::{PGScenesPlugin, SetScene, SceneTransition, ScenesSet, LoadStartScene, CurrentChunk};

    // pub use crate::scenes::{PGScenesPlugin, PGScenesSettings, 
    //     LoadStartScene, Static, SceneTransition, MapsData, SetScene, CurrentChunk, ScenesSet
    // };
    pub use crate::terrain_planes::{PGSerializedMesh, LoadTerrainPlane, PlaneToEdit};
    pub use crate::water::depth::{Wakes, WakeHitter, render_to_depth};
    pub use crate::water::{WaterPlugin, WaveUpdate, WaterChunk};
}

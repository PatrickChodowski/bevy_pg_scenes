pub mod follower;
mod water;
pub mod weather;
pub mod terrain;

pub mod spawning;
pub mod plugin;
pub mod events;
pub mod types;
pub mod load_assets;


pub mod prelude {
    pub use crate::types::{SceneData, SceneObjectData, Static, AssetSource, AssetSourceType, AssignComponents, PGSerializedMesh, 
        Markee, Marker, Spawnee, Spawner, MapMeta, MapsData, MapChunk, GameMap, GameMapEntry,
        MARKER_LOCATION, MARKER_ENTRYPORT, MARKER_EXITPORT, MARKER_NAME, MARKER_CITY, MARKER_TIER, MARKER_SOURCE,
        marker_id, marker_radius,
    };
    pub use crate::events::{LoadMap, LoadScene};
    pub use crate::plugin::{PGScenesPlugin, ScenesSet, LoadPlayerScene, SceneTransition, SetScene, MapSettings};
    pub use crate::water::depth::{Wakes, WakeHitter, render_to_depth};
    pub use crate::terrain::{
        TerrainMaterialPlugin, TerrainMaterial, TerrainLayersExtension, TerrainLayers, TerrainLayersConfig,
        TerrainLayerConfig, TerrainParams, TERRAIN_LAYERS, SPLAT_TEXTURES, terrain_material, terrain_base_material,
        new_splat_image, splat_sampler, splat_loader_settings, rgba8_pixels, NoisePattern,
    };
    pub use crate::terrain::heightmap::{
        MapHeightmap, grid_normal, encode_heights, decode_heights, decode_colors, chunk_mesh,
    };
    pub use crate::water::{WaterPlugin, WaveUpdate, WaterData, WaterMaterial, WaterMaterialExtension, nice_water, simple_water, water_plane, SeaUniform, WaterSplashes};
    pub use crate::water::open_sea::{OpenSeaField, WATER_LEVEL};
    pub use crate::water::waves::Sea;
    pub use crate::weather::{WeatherPlugin, Wind, WindDriver, Rain};
}

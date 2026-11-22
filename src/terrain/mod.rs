//! Splat-map terrain material, shared by the game and the editor.
//!
//! Each terrain has a splat (weight) texture: its RGBA channels say how much of terrain layers
//! 0..3 shows at every point. The shader blends each layer's tiling albedo and normal map (and
//! roughness) by those weights. Where nothing is painted (all weights 0) the terrain looks
//! exactly like a plain white StandardMaterial tinted by vertex colors, as before.
//!
//! Layers come from a JSON config (`TerrainMaterialPlugin::config_path`, e.g.
//! `terrain/terrain.layers.json` in the game's assets):
//!
//! ```json
//! { "layers": [
//!     { "name": "Grass", "color": [0.30, 0.45, 0.18], "tile_size": 4.0, "roughness": 0.9,
//!       "albedo": "textures/terrain/grass_albedo.png", "normal": "textures/terrain/grass_normal.png" },
//!     ...
//! ] }
//! ```
//!
//! Up to 4 layers. `albedo` / `normal` are optional asset paths. Layers without them get generated,
//! seamlessly tiling detail instead: `color` varied by `variation` (0..1), a normal map of strength
//! `bump`, shaped by `pattern` ("smooth", "ridged" for rock, "speckled" for dirt/sand). Normal maps
//! use the OpenGL convention (green = up). Textures of different sizes are resampled to the largest
//! one. Missing config file -> 4 default layers (grass, dirt, rock, sand) with generated detail.

use bevy::asset::{embedded_asset, LoadState, RenderAssetUsages};
use bevy::image::{ImageAddressMode, ImageFilterMode, ImageLoaderSettings, ImageSampler, ImageSamplerDescriptor};
use bevy::pbr::{ExtendedMaterial, MaterialExtension};
use bevy::prelude::*;
use bevy::render::render_resource::{
    AsBindGroup, Extent3d, ShaderType, TextureDataOrder, TextureDimension, TextureFormat,
    TextureViewDescriptor, TextureViewDimension,
};
use bevy::shader::ShaderRef;
use bevy_common_assets::json::JsonAssetPlugin;
use serde::{Deserialize, Serialize};

mod noise;
pub mod heightmap;
pub use noise::NoisePattern;
use noise::generate_layer;

/// Maximum number of terrain layers: two RGBA splat textures (layers 0..3 and 4..7).
pub const TERRAIN_LAYERS: usize = 8;
/// Splat textures per terrain; texture `k` holds the weights of layers 4k..4k+3.
pub const SPLAT_TEXTURES: usize = 2;

pub type TerrainMaterial = ExtendedMaterial<StandardMaterial, TerrainLayersExtension>;

pub struct TerrainMaterialPlugin {
    /// Asset path of the layer config.
    pub config_path: String,
    /// Prepended to the texture paths in the config, e.g. an asset source like "game assets://".
    pub asset_prefix: String,
}

impl Default for TerrainMaterialPlugin {
    fn default() -> Self {
        Self { config_path: "terrain/terrain.layers.json".into(), asset_prefix: String::new() }
    }
}

impl Plugin for TerrainMaterialPlugin {
    fn build(&self, app: &mut App) {
        // WESL: Bevy 0.20 only preprocesses imports/conditionals in .wesl shaders.
        embedded_asset!(app, "terrain.wesl");
        app.add_plugins((
            MaterialPlugin::<TerrainMaterial>::default(),
            JsonAssetPlugin::<TerrainLayersConfig>::new(&["layers.json"]),
        ))
        .insert_resource(TerrainLayersSource {
            config_path: self.config_path.clone(),
            asset_prefix: self.asset_prefix.clone(),
        })
        .add_systems(PreStartup, setup_terrain_layers)
        .add_systems(Update, (apply_layer_config, build_layer_textures).chain());
    }
}

// ───────────────────────── material ─────────────────────────

/// Per-layer shader settings; each field holds 8 layers as two Vec4s (layer i = [i / 4][i % 4]).
#[derive(ShaderType, Clone, Copy, Debug, Reflect)]
pub struct TerrainParams {
    /// World units per texture repeat.
    pub tile_size: [Vec4; 2],
    pub roughness: [Vec4; 2],
    pub normal_strength: [Vec4; 2],
    /// 1 = hex tiling (random offset per cell) against visible repetition, 0 = plain tiling.
    pub anti_tiling: [Vec4; 2],
    /// 1 = hex cells also get a random rotation (off for directional textures).
    pub random_rotation: [Vec4; 2],
    /// Large-scale world-space brightness variation (0..1).
    pub macro_variation: [Vec4; 2],
}

#[derive(Asset, AsBindGroup, Reflect, Debug, Clone)]
pub struct TerrainLayersExtension {
    #[uniform(100)]
    pub params: TerrainParams,
    /// Layer weights, RGBA = layers 0..3, sampled with the mesh's UVs.
    #[texture(101)]
    #[sampler(102)]
    pub splat: Handle<Image>,
    /// Layer weights, RGBA = layers 4..7.
    #[texture(103)]
    #[sampler(104)]
    pub splat2: Handle<Image>,
    #[texture(105, dimension = "2d_array")]
    #[sampler(106)]
    pub albedo: Handle<Image>,
    #[texture(107, dimension = "2d_array")]
    #[sampler(108)]
    pub normal: Handle<Image>,
}

impl MaterialExtension for TerrainLayersExtension {
    fn fragment_shader() -> ShaderRef {
        "embedded://bevy_pg_scenes/terrain/terrain.wesl".into()
    }
}

/// The base every terrain uses: white (vertex colours tint it), fully rough.
pub fn terrain_base_material() -> StandardMaterial {
    StandardMaterial { base_color: Color::WHITE, perceptual_roughness: 1.0, ..default() }
}

/// A terrain material using the two splat textures (layers 0..3, 4..7) and the shared layer textures.
pub fn terrain_material(layers: &TerrainLayers, splats: [Handle<Image>; SPLAT_TEXTURES]) -> TerrainMaterial {
    let [splat, splat2] = splats;
    TerrainMaterial {
        base: terrain_base_material(),
        extension: TerrainLayersExtension {
            params: layers.params,
            splat,
            splat2,
            albedo: layers.albedo.clone(),
            normal: layers.normal.clone(),
        },
    }
}

/// Sampler for splat textures: smooth, clamped at the edges.
pub fn splat_sampler() -> ImageSampler {
    ImageSampler::Descriptor(ImageSamplerDescriptor {
        address_mode_u: ImageAddressMode::ClampToEdge,
        address_mode_v: ImageAddressMode::ClampToEdge,
        mag_filter: ImageFilterMode::Linear,
        min_filter: ImageFilterMode::Linear,
        ..default()
    })
}

/// An empty (unpainted) splat texture, editable on the CPU.
pub fn new_splat_image(width: u32, height: u32) -> Image {
    let mut image = Image::new_fill(
        Extent3d { width, height, depth_or_array_layers: 1 },
        TextureDimension::D2,
        &[0, 0, 0, 0],
        TextureFormat::Rgba8Unorm,
        RenderAssetUsages::MAIN_WORLD | RenderAssetUsages::RENDER_WORLD,
    );
    image.sampler = splat_sampler();
    image
}

/// The image's pixels as 8-bit RGBA bytes (values unchanged: no colour-space conversion).
/// `Image::convert` can't produce linear `Rgba8Unorm`, hence this helper.
pub fn rgba8_pixels(image: &Image) -> Option<Vec<u8>> {
    match image.texture_descriptor.format {
        TextureFormat::Rgba8Unorm | TextureFormat::Rgba8UnormSrgb => image.data.clone(),
        // into_rgba8 only reshapes channels; the srgb flag doesn't change the bytes.
        _ => image.convert(TextureFormat::Rgba8UnormSrgb)?.data,
    }
}

/// Loader settings for splat PNGs: weights are linear data, not colours.
pub fn splat_loader_settings(settings: &mut ImageLoaderSettings) {
    settings.is_srgb = false;
    settings.sampler = splat_sampler();
}

// ───────────────────────── layers ─────────────────────────

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TerrainLayerConfig {
    pub name: String,
    /// Linear-ish sRGB colour used when there is no albedo texture.
    #[serde(default = "default_color")]
    pub color: [f32; 3],
    #[serde(default)]
    pub albedo: Option<String>,
    #[serde(default)]
    pub normal: Option<String>,
    #[serde(default = "default_tile_size")]
    pub tile_size: f32,
    #[serde(default = "default_roughness")]
    pub roughness: f32,
    #[serde(default = "default_normal_strength")]
    pub normal_strength: f32,
    /// Generated detail for layers without an albedo texture: how much brightness/hue varies (0..1).
    #[serde(default)]
    pub variation: f32,
    /// Generated normal map strength for layers without a normal texture (0 = flat).
    #[serde(default)]
    pub bump: f32,
    /// Shape of the generated detail: "smooth", "ridged" or "speckled".
    #[serde(default)]
    pub pattern: NoisePattern,
    /// Hide the repeating tile pattern (hex tiling: each small cell gets a random offset).
    #[serde(default = "default_true")]
    pub anti_tiling: bool,
    /// With anti_tiling, also rotate each cell randomly. Turn off for directional textures
    /// (ripples, planks, strata).
    #[serde(default = "default_true")]
    pub random_rotation: bool,
    /// Slight brightness variation over tens of metres, so large areas aren't uniform (0..1).
    #[serde(default = "default_macro_variation")]
    pub macro_variation: f32,
    /// Suggested brush hardness (0 = all soft fade, 1 = hard edge) when this layer is picked in
    /// the editor, e.g. high for roads and paths. None = keep the current brush setting.
    #[serde(default)]
    pub hardness: Option<f32>,
}

fn default_true() -> bool { true }
fn default_macro_variation() -> f32 { 0.15 }

fn default_color() -> [f32; 3] { [1.0, 1.0, 1.0] }
fn default_tile_size() -> f32 { 4.0 }
fn default_roughness() -> f32 { 0.9 }
fn default_normal_strength() -> f32 { 1.0 }

#[derive(Serialize, Deserialize, Clone, Debug, Asset, TypePath)]
pub struct TerrainLayersConfig {
    pub layers: Vec<TerrainLayerConfig>,
}

impl Default for TerrainLayersConfig {
    fn default() -> Self {
        let layer = |name: &str, color: [f32; 3], roughness: f32, tile_size: f32, variation: f32, bump: f32, pattern| {
            TerrainLayerConfig {
                name: name.into(),
                color,
                albedo: None,
                normal: None,
                tile_size,
                roughness,
                normal_strength: default_normal_strength(),
                variation,
                bump,
                pattern,
                anti_tiling: true,
                random_rotation: true,
                macro_variation: default_macro_variation(),
                hardness: None,
            }
        };
        let mut road = layer("Dirt road", [0.52, 0.43, 0.32], 0.85, 4.0, 0.12, 0.3, NoisePattern::Packed);
        road.macro_variation = 0.08;
        road.hardness = Some(0.85);
        Self {
            layers: vec![
                layer("Grass", [0.30, 0.45, 0.18], 0.95, 6.0, 0.35, 0.25, NoisePattern::Smooth),
                layer("Dirt", [0.42, 0.31, 0.20], 0.95, 5.0, 0.30, 0.45, NoisePattern::Speckled),
                layer("Rock", [0.45, 0.45, 0.47], 0.75, 6.0, 0.45, 1.6, NoisePattern::Ridged),
                layer("Sand", [0.76, 0.70, 0.50], 0.9, 8.0, 0.12, 0.2, NoisePattern::Speckled),
                road,
                layer("Snow", [0.92, 0.94, 0.97], 0.6, 7.0, 0.08, 0.15, NoisePattern::Smooth),
            ],
        }
    }
}

impl TerrainLayersConfig {
    /// At most TERRAIN_LAYERS layers (extra ones dropped with a warning), at least one.
    fn checked(mut self) -> Self {
        if self.layers.len() > TERRAIN_LAYERS {
            warn!("Terrain layer config has {} layers; only the first {TERRAIN_LAYERS} are used", self.layers.len());
            self.layers.truncate(TERRAIN_LAYERS);
        }
        if self.layers.is_empty() {
            warn!("Terrain layer config has no layers; using the defaults");
            return Self::default();
        }
        self
    }

    /// Layer `i`, or a blank (white, flat) filler for unused slots of the texture arrays.
    fn layer(&self, i: usize) -> TerrainLayerConfig {
        self.layers.get(i).cloned().unwrap_or_else(|| TerrainLayerConfig {
            name: String::new(),
            color: default_color(),
            albedo: None,
            normal: None,
            tile_size: default_tile_size(),
            roughness: default_roughness(),
            normal_strength: default_normal_strength(),
            variation: 0.0,
            bump: 0.0,
            pattern: NoisePattern::Smooth,
            anti_tiling: false,
            random_rotation: false,
            macro_variation: 0.0,
            hardness: None,
        })
    }

    fn params(&self) -> TerrainParams {
        let v = |f: fn(&TerrainLayerConfig) -> f32| {
            let all: [f32; TERRAIN_LAYERS] = std::array::from_fn(|i| f(&self.layer(i)));
            [Vec4::from_slice(&all[0..4]), Vec4::from_slice(&all[4..8])]
        };
        TerrainParams {
            tile_size: v(|l| l.tile_size.max(0.01)),
            roughness: v(|l| l.roughness.clamp(0.0, 1.0)),
            normal_strength: v(|l| l.normal_strength),
            anti_tiling: v(|l| if l.anti_tiling { 1.0 } else { 0.0 }),
            random_rotation: v(|l| if l.random_rotation { 1.0 } else { 0.0 }),
            macro_variation: v(|l| l.macro_variation.clamp(0.0, 1.0)),
        }
    }
}

#[derive(Resource)]
struct TerrainLayersSource {
    config_path: String,
    asset_prefix: String,
}

/// The shared layer textures (texture arrays) and settings every terrain material uses.
/// The handles never change: textures are swapped in place once they've loaded.
#[derive(Resource)]
pub struct TerrainLayers {
    pub albedo: Handle<Image>,
    pub normal: Handle<Image>,
    pub params: TerrainParams,
    /// Display names of the configured layers (up to TERRAIN_LAYERS), for UIs.
    pub names: Vec<String>,
    /// Per configured layer: the brush hardness it suggests (see TerrainLayerConfig::hardness).
    pub hardness: Vec<Option<f32>>,
    state: LayersState,
}

enum LayersState {
    LoadingConfig(Handle<TerrainLayersConfig>),
    LoadingTextures {
        config: TerrainLayersConfig,
        albedo: [Option<Handle<Image>>; TERRAIN_LAYERS],
        normal: [Option<Handle<Image>>; TERRAIN_LAYERS],
    },
    Ready,
}

fn setup_terrain_layers(
    mut commands: Commands,
    ass: Res<AssetServer>,
    source: Res<TerrainLayersSource>,
    mut images: ResMut<Assets<Image>>,
) {
    let config = TerrainLayersConfig::default();
    let (albedo, normal) = placeholder_arrays(&config);
    commands.insert_resource(TerrainLayers {
        albedo: images.add(albedo),
        normal: images.add(normal),
        params: config.params(),
        names: config.layers.iter().map(|l| l.name.clone()).collect(),
        hardness: config.layers.iter().map(|l| l.hardness).collect(),
        state: LayersState::LoadingConfig(ass.load(source.config_path.clone())),
    });
}

/// Once the config has loaded (or failed to): apply names/colours/params and start loading textures.
fn apply_layer_config(
    mut layers: ResMut<TerrainLayers>,
    ass: Res<AssetServer>,
    source: Res<TerrainLayersSource>,
    configs: Res<Assets<TerrainLayersConfig>>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<TerrainMaterial>>,
) {
    let LayersState::LoadingConfig(handle) = &layers.state else { return };
    let config = match ass.load_state(handle) {
        LoadState::Loaded => match configs.get(handle) {
            Some(config) => config.clone().checked(),
            None => return,
        },
        LoadState::Failed(err) => {
            warn!("No terrain layer config at '{}' ({err}); using the default layers", source.config_path);
            TerrainLayersConfig::default()
        }
        _ => return,
    };

    layers.params = config.params();
    layers.names = config.layers.iter().map(|l| l.name.clone()).collect();
    layers.hardness = config.layers.iter().map(|l| l.hardness).collect();
    let (albedo, normal) = placeholder_arrays(&config);
    let (albedo_id, normal_id) = (layers.albedo.id(), layers.normal.id());
    let _ = images.insert(albedo_id, albedo);
    let _ = images.insert(normal_id, normal);
    refresh_materials(&layers, &mut materials);

    let load = |path: &Option<String>, srgb: bool| {
        path.as_ref().map(|p| {
            ass.load_builder()
                .with_settings(move |s: &mut ImageLoaderSettings| s.is_srgb = srgb)
                .load::<Image>(format!("{}{p}", source.asset_prefix))
        })
    };
    let albedo = std::array::from_fn(|i| load(&config.layer(i).albedo, true));
    let normal = std::array::from_fn(|i| load(&config.layer(i).normal, false));
    layers.state = LayersState::LoadingTextures { config, albedo, normal };
}

/// Once every layer texture has loaded (or failed to): build the texture arrays.
fn build_layer_textures(
    mut layers: ResMut<TerrainLayers>,
    ass: Res<AssetServer>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<TerrainMaterial>>,
) {
    let LayersState::LoadingTextures { config, albedo, normal } = &layers.state else { return };
    let pending = albedo.iter().chain(normal.iter()).flatten().any(|h| {
        matches!(ass.load_state(h), LoadState::NotLoaded | LoadState::Loading)
    });
    if pending {
        return;
    }

    let pixels = |handles: &[Option<Handle<Image>>; TERRAIN_LAYERS]| {
        std::array::from_fn::<_, TERRAIN_LAYERS, _>(|i| {
            let handle = handles[i].as_ref()?;
            let image = images.get(handle).or_else(|| {
                warn!("Terrain layer texture {:?} failed to load", handle.path());
                None
            })?;
            Some((image.width(), image.height(), rgba8_pixels(image)?))
        })
    };
    let mut albedo_px = pixels(albedo);
    let mut normal_px = pixels(normal);
    let loaded = albedo_px.iter().chain(normal_px.iter()).flatten().count();

    // Layers without textures get generated, tiling noise detail at the arrays' resolution.
    let size = albedo_px
        .iter()
        .chain(normal_px.iter())
        .flatten()
        .map(|(w, h, _)| (*w).max(*h))
        .max()
        .unwrap_or(GENERATED_SIZE);
    for i in 0..TERRAIN_LAYERS {
        let layer = config.layer(i);
        if albedo_px[i].is_some() && normal_px[i].is_some() {
            continue;
        }
        let (gen_albedo, gen_normal) =
            generate_layer(size, color_bytes(layer.color), layer.variation, layer.bump, layer.pattern, i as u32 * 7919 + 1);
        albedo_px[i].get_or_insert((size, size, gen_albedo));
        normal_px[i].get_or_insert((size, size, gen_normal));
    }

    let colors: [[u8; 4]; TERRAIN_LAYERS] = std::array::from_fn(|i| color_bytes(config.layer(i).color));
    let albedo_array = layer_array(&albedo_px, &colors, TextureFormat::Rgba8UnormSrgb);
    let normal_array = layer_array(&normal_px, &[FLAT_NORMAL; TERRAIN_LAYERS], TextureFormat::Rgba8Unorm);

    info!("Terrain layers ready: {} ({loaded} texture file(s), rest generated)", layers.names.join(", "));

    let (albedo_id, normal_id) = (layers.albedo.id(), layers.normal.id());
    let _ = images.insert(albedo_id, albedo_array);
    let _ = images.insert(normal_id, normal_array);
    refresh_materials(&layers, &mut materials);
    layers.state = LayersState::Ready;
}

/// Materials only rebind their textures when the material asset itself changes, so after
/// replacing layer textures (or params) every terrain material has to be touched.
fn refresh_materials(layers: &TerrainLayers, materials: &mut Assets<TerrainMaterial>) {
    for (_, material) in materials.iter_mut() {
        material.extension.params = layers.params;
    }
}

// ───────────────────────── texture arrays ─────────────────────────

const FLAT_NORMAL: [u8; 4] = [128, 128, 255, 255];
/// Resolution of generated layer textures when no texture files set one.
const GENERATED_SIZE: u32 = 512;
const PLACEHOLDER_SIZE: u32 = 4;

fn color_bytes(c: [f32; 3]) -> [u8; 4] {
    let srgb = Color::srgb(c[0], c[1], c[2]).to_srgba();
    [
        (srgb.red.clamp(0.0, 1.0) * 255.0).round() as u8,
        (srgb.green.clamp(0.0, 1.0) * 255.0).round() as u8,
        (srgb.blue.clamp(0.0, 1.0) * 255.0).round() as u8,
        255,
    ]
}

fn placeholder_arrays(config: &TerrainLayersConfig) -> (Image, Image) {
    let none: [Option<(u32, u32, Vec<u8>)>; TERRAIN_LAYERS] = Default::default();
    let colors: [[u8; 4]; TERRAIN_LAYERS] = std::array::from_fn(|i| color_bytes(config.layer(i).color));
    (
        layer_array(&none, &colors, TextureFormat::Rgba8UnormSrgb),
        layer_array(&none, &[FLAT_NORMAL; TERRAIN_LAYERS], TextureFormat::Rgba8Unorm),
    )
}

/// A TERRAIN_LAYERS-layer texture array with mipmaps. Layers without pixels get `fill`;
/// layers of a different size are resampled to the largest one.
fn layer_array(
    layers: &[Option<(u32, u32, Vec<u8>)>; TERRAIN_LAYERS],
    fill: &[[u8; 4]; TERRAIN_LAYERS],
    format: TextureFormat,
) -> Image {
    let (width, height) = layers
        .iter()
        .flatten()
        .map(|(w, h, _)| (*w, *h))
        .max_by_key(|(w, h)| w * h)
        .unwrap_or((PLACEHOLDER_SIZE, PLACEHOLDER_SIZE));
    let mip_count = mip_count(width, height);

    let mut data = Vec::new();
    for (i, layer) in layers.iter().enumerate() {
        let base = match layer {
            Some((w, h, px)) if (*w, *h) == (width, height) => px.clone(),
            Some((w, h, px)) => resize_nearest(px, *w, *h, width, height),
            None => fill[i].repeat((width * height) as usize),
        };
        // Layer-major: all mips of layer 0, then all mips of layer 1, ...
        for mip in mip_chain(base, width, height) {
            data.extend_from_slice(&mip);
        }
    }

    let mut image = Image::new_fill(
        Extent3d { width, height, depth_or_array_layers: TERRAIN_LAYERS as u32 },
        TextureDimension::D2,
        &[0, 0, 0, 0],
        format,
        RenderAssetUsages::RENDER_WORLD,
    );
    image.data = Some(data);
    image.data_order = TextureDataOrder::LayerMajor;
    image.texture_descriptor.mip_level_count = mip_count;
    image.texture_view_descriptor = Some(TextureViewDescriptor {
        dimension: Some(TextureViewDimension::D2Array),
        ..default()
    });
    image.sampler = ImageSampler::Descriptor(ImageSamplerDescriptor {
        address_mode_u: ImageAddressMode::Repeat,
        address_mode_v: ImageAddressMode::Repeat,
        mag_filter: ImageFilterMode::Linear,
        min_filter: ImageFilterMode::Linear,
        mipmap_filter: ImageFilterMode::Linear,
        anisotropy_clamp: 8,
        ..default()
    });
    image
}

fn mip_count(width: u32, height: u32) -> u32 {
    32 - width.max(height).max(1).leading_zeros()
}

/// Mip 0 is `base`; each next level is a 2x2 box filter of the previous one, down to 1x1.
fn mip_chain(base: Vec<u8>, width: u32, height: u32) -> Vec<Vec<u8>> {
    let mut mips = vec![base];
    let (mut w, mut h) = (width, height);
    while w > 1 || h > 1 {
        let (nw, nh) = ((w / 2).max(1), (h / 2).max(1));
        let prev = mips.last().unwrap();
        let mut next = vec![0u8; (nw * nh * 4) as usize];
        for y in 0..nh {
            for x in 0..nw {
                for c in 0..4 {
                    let mut sum = 0u32;
                    for (dx, dy) in [(0, 0), (1, 0), (0, 1), (1, 1)] {
                        let sx = (x * 2 + dx).min(w - 1);
                        let sy = (y * 2 + dy).min(h - 1);
                        sum += prev[((sy * w + sx) * 4 + c) as usize] as u32;
                    }
                    next[((y * nw + x) * 4 + c) as usize] = ((sum + 2) / 4) as u8;
                }
            }
        }
        mips.push(next);
        (w, h) = (nw, nh);
    }
    mips
}

fn resize_nearest(px: &[u8], w: u32, h: u32, nw: u32, nh: u32) -> Vec<u8> {
    let mut out = vec![0u8; (nw * nh * 4) as usize];
    for y in 0..nh {
        for x in 0..nw {
            let sx = (x * w / nw).min(w - 1);
            let sy = (y * h / nh).min(h - 1);
            let src = ((sy * w + sx) * 4) as usize;
            let dst = ((y * nw + x) * 4) as usize;
            out[dst..dst + 4].copy_from_slice(&px[src..src + 4]);
        }
    }
    out
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mip_chain_goes_down_to_one_pixel() {
        let base = vec![255u8; 8 * 4 * 4];
        let mips = mip_chain(base, 8, 4);
        assert_eq!(mips.len() as u32, mip_count(8, 4));
        assert_eq!(mips.last().unwrap().len(), 4);
        assert!(mips.iter().all(|m| m.iter().all(|&b| b == 255)));
    }

    #[test]
    fn layer_array_data_size_matches_all_layers_and_mips() {
        let layers: [Option<(u32, u32, Vec<u8>)>; TERRAIN_LAYERS] =
            std::array::from_fn(|i| match i {
                0 => Some((8, 8, vec![10; 8 * 8 * 4])),
                2 => Some((4, 4, vec![20; 4 * 4 * 4])),
                _ => None,
            });
        let image = layer_array(&layers, &[[1, 2, 3, 255]; TERRAIN_LAYERS], TextureFormat::Rgba8Unorm);
        let per_layer: usize = mip_chain(vec![0; 8 * 8 * 4], 8, 8).iter().map(|m| m.len()).sum();
        assert_eq!(image.data.as_ref().unwrap().len(), per_layer * TERRAIN_LAYERS);
        assert_eq!(image.texture_descriptor.mip_level_count, 4);
        assert_eq!(image.texture_descriptor.size.depth_or_array_layers, TERRAIN_LAYERS as u32);
    }

    #[test]
    fn config_is_limited_to_max_layers_and_padded_for_the_arrays() {
        let mut cfg = TerrainLayersConfig::default();
        cfg.layers.extend(TerrainLayersConfig::default().layers);
        assert!(cfg.layers.len() > TERRAIN_LAYERS);
        let cfg = cfg.checked();
        assert_eq!(cfg.layers.len(), TERRAIN_LAYERS);

        let cfg = TerrainLayersConfig::default();
        assert_eq!(cfg.layers.len(), 6);
        assert_eq!(cfg.layers[5].name, "Snow");
        assert_eq!(cfg.layer(7).name, "", "unused slots are blank fillers");
        let params = cfg.params();
        assert_eq!(params.tile_size[1].x, 4.0, "layer 4 (road) is in the second Vec4");
    }
}

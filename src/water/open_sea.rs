//! Open sea: how far every point of the world's water is from land, and how deep it is, so the
//! water shader can make the waves bigger the further out to sea a boat is (and keep them off
//! shallow banks and low islands, which they would wash over).
//!
//! Once the maps are known, their heightmaps are loaded (the same files and settings as the
//! terrain chunks, so it's one load), every heightmap sample above the water level marks its
//! texel as land, and an exact distance transform gives each texel its distance to the nearest
//! land. The depth per texel is that of its shallowest sample. That runs once, on the async compute pool. The result goes to the water material as a
//! texture ([`WaterMaterialExtension::open_sea`]) and stays on the CPU as [`OpenSeaField`] (for
//! gameplay: boats riding the waves, rough seas).
//!
//! Until the field is ready (or for maps without heightmaps) the shader treats all water as
//! coastal: flat, as before.

use bevy::asset::{LoadState, RenderAssetUsages};
use bevy::image::{ImageLoaderSettings, ImageSampler};
use bevy::prelude::*;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy::tasks::{block_on, futures_lite::future, AsyncComputeTaskPool, Task};

use crate::terrain::heightmap::{decode_heights, MapHeightmap};
use crate::types::MapsData;
use crate::water::WaterData;

/// World Y of the sea surface: heightmap samples above it are land.
pub const WATER_LEVEL: f32 = 0.0;
/// World units per texel of the distance field (at least; larger for a very big world).
const TEXEL: f32 = 8.0;
const MAX_TEXELS: f32 = 4096.0;
/// The texture stores distance / DIST_UNIT as u8, so it covers 0..255 * DIST_UNIT world units.
pub const DIST_UNIT: f32 = 4.0;
/// The texture's second channel stores water depth / DEPTH_UNIT as u8 (0..5.1 world units; the
/// sea floor is mostly at -3.2).
pub const DEPTH_UNIT: f32 = 0.02;

pub struct OpenSeaPlugin;

impl Plugin for OpenSeaPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            bake_open_sea.run_if(resource_exists::<MapsData>).run_if(resource_exists::<WaterData>),
        );
    }
}

/// Distance to land over the whole world, on the CPU.
#[derive(Resource, Clone)]
pub struct OpenSeaField {
    /// World XZ of texel (0, 0)'s corner.
    pub min: Vec2,
    /// World units per texel.
    pub texel: f32,
    pub width: u32,
    pub height: u32,
    /// Distance to the nearest land per texel (world units / DIST_UNIT, capped at 255), row-major.
    pub dist: Vec<u8>,
    /// Water depth per texel (world units / DEPTH_UNIT, capped at 255; 0 on land), row-major:
    /// of its shallowest sample (the waves stay off shallow banks).
    pub depth: Vec<u8>,
    /// Mean water depth per texel (same units; land counts as 0): smooth, for how the water
    /// looks (the shallowest sample per texel would show the texels as squares).
    pub mean_depth: Vec<u8>,
}

impl OpenSeaField {
    /// Distance (world units) from a world XZ point to the nearest land, bilinearly filtered like
    /// the shader's sample. Outside the field: as far as the field goes.
    pub fn distance(&self, p: Vec2) -> f32 {
        self.sample(&self.dist, p) * DIST_UNIT
    }

    /// Water depth (world units) at a world XZ point, filtered like the shader's sample.
    pub fn depth(&self, p: Vec2) -> f32 {
        self.sample(&self.depth, p) * DEPTH_UNIT
    }

    fn sample(&self, values: &[u8], p: Vec2) -> f32 {
        let g = (p - self.min) / self.texel - 0.5;
        let (w, h) = (self.width as i32, self.height as i32);
        let at = |x: i32, y: i32| values[(y.clamp(0, h - 1) * w + x.clamp(0, w - 1)) as usize] as f32;
        let (x0, y0) = (g.x.floor() as i32, g.y.floor() as i32);
        let f = g - g.floor();
        let top = at(x0, y0) * (1.0 - f.x) + at(x0 + 1, y0) * f.x;
        let bottom = at(x0, y0 + 1) * (1.0 - f.x) + at(x0 + 1, y0 + 1) * f.x;
        top * (1.0 - f.y) + bottom * f.y
    }
}

/// One map's heightmap, decoded, for the bake.
struct MapHeights {
    origin: Vec3,
    hm: MapHeightmap,
    heights: Vec<f32>,
}

#[derive(Default)]
enum BakeState {
    #[default]
    Start,
    Loading(Vec<(Vec3, MapHeightmap, Handle<Image>)>),
    Baking(Task<Option<OpenSeaField>>),
    Done,
}

fn bake_open_sea(
    mut commands: Commands,
    maps_data: Res<MapsData>,
    water: Res<WaterData>,
    ass: Res<AssetServer>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<crate::water::WaterMaterial>>,
    mut state: Local<BakeState>,
) {
    if maps_data.is_changed() && !maps_data.is_added() {
        *state = BakeState::Start; // different maps: bake again
    }
    match &mut *state {
        BakeState::Start => {
            let cpu_only = |s: &mut ImageLoaderSettings| {
                s.is_srgb = false;
                s.asset_usage = RenderAssetUsages::MAIN_WORLD;
            };
            let pending = maps_data
                .data
                .values()
                .filter_map(|meta| {
                    let hm = meta.heightmap.as_ref()?;
                    let path = format!("game_map/{}/terrain/{}", meta.name, hm.file);
                    Some((Vec3::from(meta.origin), hm.clone(), ass.load_builder().with_settings(cpu_only).load(path)))
                })
                .collect();
            *state = BakeState::Loading(pending);
        }
        BakeState::Loading(pending) => {
            let loading = pending
                .iter()
                .any(|(.., h)| images.get(h).is_none() && !matches!(ass.load_state(h), LoadState::Failed(_)));
            if loading {
                return;
            }
            let maps: Vec<MapHeights> = pending
                .iter()
                .filter_map(|(origin, hm, h)| Some(MapHeights { origin: *origin, hm: hm.clone(), heights: decode_heights(images.get(h)?, hm)? }))
                .collect();
            let bounds = world_bounds(&maps_data);
            *state = match (maps.is_empty(), bounds) {
                (false, Some((min, max))) => BakeState::Baking(AsyncComputeTaskPool::get().spawn(async move { Some(bake(&maps, min, max)) })),
                _ => BakeState::Done, // no heightmaps: the water stays flat
            };
        }
        BakeState::Baking(task) => {
            let Some(result) = block_on(future::poll_once(task)) else { return };
            *state = BakeState::Done;
            let Some(field) = result else { return };
            let mut image = Image::new(
                Extent3d { width: field.width, height: field.height, depth_or_array_layers: 1 },
                TextureDimension::D2,
                field.dist.iter().zip(&field.depth).zip(&field.mean_depth).flat_map(|((&d, &z), &m)| [d, z, m, 255]).collect(),
                TextureFormat::Rgba8Unorm,
                RenderAssetUsages::RENDER_WORLD,
            );
            image.sampler = ImageSampler::linear();
            let handle = images.add(image);
            if let Some(mut material) = materials.get_mut(&water.material) {
                let size = Vec2::new(field.width as f32, field.height as f32) * field.texel;
                material.extension.open_sea = Some(handle);
                material.extension.sea.rect = Vec4::new(field.min.x, field.min.y, 1.0 / size.x, 1.0 / size.y);
                material.extension.sea.enabled = 1.0;
            }
            info!("[Water] Open sea field baked: {} x {} texels of {} units", field.width, field.height, field.texel);
            commands.insert_resource(field);
        }
        BakeState::Done => {}
    }
}

/// XZ bounds (min, max) of every map's chunks.
fn world_bounds(maps_data: &MapsData) -> Option<(Vec2, Vec2)> {
    let (mut min, mut max) = (Vec2::splat(f32::MAX), Vec2::splat(f32::MIN));
    for meta in maps_data.data.values() {
        let half = Vec2::new(meta.chunk_width, meta.chunk_height) / 2.0;
        for chunk in &meta.chunks {
            let c = Vec2::new(meta.origin[0] + chunk.location[0], meta.origin[2] + chunk.location[2]);
            min = min.min(c - half);
            max = max.max(c + half);
        }
    }
    (min.x < max.x && min.y < max.y).then_some((min, max))
}

fn bake(maps: &[MapHeights], min: Vec2, max: Vec2) -> OpenSeaField {
    let size = max - min;
    let texel = TEXEL.max(size.max_element() / MAX_TEXELS);
    let (w, h) = ((size.x / texel).ceil().max(1.0) as usize, (size.y / texel).ceil().max(1.0) as usize);

    // Land: every texel holding a heightmap sample above the water (so small islands count too).
    let mut land = vec![false; w * h];
    // Highest ground per texel (none = no map here: deep).
    let mut ground = vec![f32::MIN; w * h];
    // Sum of water depths per texel and how many samples (for the mean).
    let mut depth_sum = vec![0.0f32; w * h];
    let mut depth_count = vec![0u32; w * h];
    for map in maps {
        let (hm, stride, apron) = (&map.hm, map.hm.stride(), map.hm.apron as usize);
        for j in 0..hm.samples {
            for i in 0..hm.samples {
                let y = map.heights[(j as usize + apron) * stride + i as usize + apron] + map.origin.y;
                let p = (map.origin.xz() + hm.local_xz(i, j) - min) / texel;
                let (x, z) = ((p.x as usize).min(w - 1), (p.y as usize).min(h - 1));
                ground[z * w + x] = ground[z * w + x].max(y);
                depth_sum[z * w + x] += (WATER_LEVEL - y).max(0.0);
                depth_count[z * w + x] += 1;
                if y > WATER_LEVEL {
                    land[z * w + x] = true;
                }
            }
        }
    }

    // Exact squared distance (texels) to the nearest land: Felzenszwalb & Huttenlocher, columns
    // then rows.
    const INF: f32 = 1e20;
    let mut d2: Vec<f32> = land.iter().map(|&l| if l { 0.0 } else { INF }).collect();
    let mut f = vec![0.0; w.max(h)];
    let mut out = vec![0.0; w.max(h)];
    let mut v = vec![0usize; w.max(h)];
    let mut zs = vec![0.0; w.max(h) + 1];
    for x in 0..w {
        for z in 0..h {
            f[z] = d2[z * w + x];
        }
        edt_1d(&f[..h], &mut out[..h], &mut v, &mut zs);
        for z in 0..h {
            d2[z * w + x] = out[z];
        }
    }
    for z in 0..h {
        f[..w].copy_from_slice(&d2[z * w..(z + 1) * w]);
        edt_1d(&f[..w], &mut out[..w], &mut v, &mut zs);
        d2[z * w..(z + 1) * w].copy_from_slice(&out[..w]);
    }

    let dist = d2.iter().map(|&d| (d.sqrt() * texel / DIST_UNIT).min(255.0) as u8).collect();
    let depth = ground.iter().map(|&g| ((WATER_LEVEL - g) / DEPTH_UNIT).clamp(0.0, 255.0) as u8).collect();
    let mean_depth = depth_sum
        .iter()
        .zip(&depth_count)
        .map(|(&s, &n)| if n == 0 { 255 } else { (s / n as f32 / DEPTH_UNIT).clamp(0.0, 255.0) as u8 })
        .collect();
    OpenSeaField { min, texel, width: w as u32, height: h as u32, dist, depth, mean_depth }
}

/// 1D squared distance transform of `f` (0 at features, huge elsewhere) into `d`.
fn edt_1d(f: &[f32], d: &mut [f32], v: &mut [usize], z: &mut [f32]) {
    let n = f.len();
    // No features on this line at all: everything stays "far" (the other pass may fix it).
    if f.iter().all(|&x| x >= 1e19) {
        d.copy_from_slice(f);
        return;
    }
    let mut k = 0usize;
    // Skip leading infinite samples so the lower envelope starts at a real parabola.
    let first = f.iter().position(|&x| x < 1e19).unwrap();
    v[0] = first;
    z[0] = f32::NEG_INFINITY;
    z[1] = f32::INFINITY;
    for q in first + 1..n {
        if f[q] >= 1e19 {
            continue;
        }
        loop {
            let p = v[k];
            let s = ((f[q] + (q * q) as f32) - (f[p] + (p * p) as f32)) / (2.0 * (q as f32 - p as f32));
            if s <= z[k] && k > 0 {
                k -= 1;
                continue;
            }
            if s <= z[k] {
                // k == 0: q replaces the only parabola.
                v[0] = q;
                z[1] = f32::INFINITY;
                break;
            }
            k += 1;
            v[k] = q;
            z[k] = s;
            z[k + 1] = f32::INFINITY;
            break;
        }
    }
    let mut k = 0usize;
    for (q, out) in d.iter_mut().enumerate().take(n) {
        while z[k + 1] < q as f32 {
            k += 1;
        }
        let p = v[k];
        let dq = q as f32 - p as f32;
        *out = dq * dq + f[p];
    }
}

#[cfg(test)]
mod tests {
    use super::edt_1d;

    #[test]
    fn edt_matches_brute_force() {
        let inf = 1e20;
        let cases: Vec<Vec<f32>> = vec![
            vec![inf, inf, 0.0, inf, inf, inf, 0.0, inf],
            vec![0.0, inf, inf, inf, inf],
            vec![inf, inf, inf, inf, 0.0],
            vec![4.0, inf, 1.0, 0.0, inf, 9.0, inf],
            vec![inf, 0.0],
        ];
        for f in cases {
            let n = f.len();
            let mut d = vec![0.0; n];
            let (mut v, mut z) = (vec![0; n], vec![0.0; n + 1]);
            edt_1d(&f, &mut d, &mut v, &mut z);
            for q in 0..n {
                let brute = (0..n).map(|p| (q as f32 - p as f32).powi(2) + f[p]).fold(f32::MAX, f32::min);
                assert!((d[q] - brute).abs() < 1e-3, "{f:?} at {q}: {} vs {brute}", d[q]);
            }
        }
    }
}

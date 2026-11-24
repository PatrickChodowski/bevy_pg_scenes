//! River and lake water: how it looks differs from the sea. The editor writes two RGBA8 images
//! per map (`MapHeightmap::water_look_file`, `water_flow_file`; one texel per heightmap sample),
//! premultiplied by how much each texel is river or lake:
//! - look: sRGB colour x weight, weight;
//! - flow: 0.5 + flow direction (x east, z south) x weight / 2, speed / RIVER_MAX_FLOW x weight,
//!   turbidity x weight.
//!
//! Once the maps are known, they're loaded and laid into two world-wide textures at the
//! heightmap's sample spacing, which go to the water material (`river_look`, `river_flow`,
//! `SeaUniform::rivers`). The shader blends per pixel, so rivers fade into the sea at their
//! mouths. Maps without the images (all sea) leave their part of the textures empty.

use bevy::asset::{LoadState, RenderAssetUsages};
use bevy::image::{ImageLoaderSettings, ImageSampler};
use bevy::prelude::*;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat};

use crate::terrain::heightmap::MapHeightmap;
use crate::types::MapsData;
use crate::water::{WaterData, WaterMaterial};

pub struct RiverWaterPlugin;

impl Plugin for RiverWaterPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, bake_river_water.run_if(resource_exists::<MapsData>).run_if(resource_exists::<WaterData>));
    }
}

/// One map's images being loaded: origin, heightmap, look, flow.
type Pending = Vec<(Vec3, MapHeightmap, Handle<Image>, Handle<Image>)>;

#[derive(Default)]
enum BakeState {
    #[default]
    Start,
    Loading(Pending),
    Done,
}

fn bake_river_water(
    maps_data: Res<MapsData>,
    water: Res<WaterData>,
    ass: Res<AssetServer>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<WaterMaterial>>,
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
            let pending: Pending = maps_data
                .data
                .values()
                .filter_map(|meta| {
                    let hm = meta.heightmap.as_ref()?;
                    let (look, flow) = (hm.water_look_file.as_ref()?, hm.water_flow_file.as_ref()?);
                    let load = |file: &str| ass.load_builder().with_settings(cpu_only).load(format!("game_map/{}/terrain/{file}", meta.name));
                    Some((Vec3::from(meta.origin), hm.clone(), load(look), load(flow)))
                })
                .collect();
            *state = if pending.is_empty() { BakeState::Done } else { BakeState::Loading(pending) };
        }
        BakeState::Loading(pending) => {
            let waiting = |h: &Handle<Image>| images.get(h).is_none() && !matches!(ass.load_state(h), LoadState::Failed(_));
            if pending.iter().any(|(_, _, a, b)| waiting(a) || waiting(b)) {
                return;
            }
            let baked = bake(pending, &images);
            *state = BakeState::Done;
            let Some((min, texel, w, h, look, flow)) = baked else { return };
            let image = |data: Vec<u8>| {
                let mut img = Image::new(
                    Extent3d { width: w, height: h, depth_or_array_layers: 1 },
                    TextureDimension::D2,
                    data,
                    TextureFormat::Rgba8Unorm,
                    RenderAssetUsages::RENDER_WORLD,
                );
                img.sampler = ImageSampler::linear();
                img
            };
            let (look, flow) = (images.add(image(look)), images.add(image(flow)));
            if let Some(mut material) = materials.get_mut(&water.material) {
                let size = Vec2::new(w as f32, h as f32) * texel;
                material.extension.river_look = Some(look);
                material.extension.river_flow = Some(flow);
                material.extension.sea.rivers = Vec4::new(min.x, min.y, 1.0 / size.x, 1.0 / size.y);
            }
            info!("[Water] River water: {w} x {h} texels of {texel} units");
        }
        BakeState::Done => {}
    }
}

/// Lays every map's images into world textures: (world XZ of texel (0, 0)'s corner, texel size,
/// width, height, look, flow). Texel centres sit on the heightmap samples.
fn bake(pending: &Pending, images: &Assets<Image>) -> Option<(Vec2, f32, u32, u32, Vec<u8>, Vec<u8>)> {
    let texel = pending.first()?.1.spacing().x;
    let (mut lo, mut hi) = (Vec2::splat(f32::MAX), Vec2::splat(f32::MIN));
    for (origin, hm, ..) in pending {
        let half = Vec2::new(hm.width, hm.depth) / 2.0;
        lo = lo.min(origin.xz() - half);
        hi = hi.max(origin.xz() + half);
    }
    let min = lo - texel / 2.0;
    let (w, h) = (((hi - lo).x / texel).round() as u32 + 1, ((hi - lo).y / texel).round() as u32 + 1);
    let mut look = vec![0u8; (w * h * 4) as usize];
    let mut flow: Vec<u8> = std::iter::repeat_n([128u8, 128, 0, 0], (w * h) as usize).flatten().collect();
    for (origin, hm, look_h, flow_h) in pending {
        if (hm.spacing().x - texel).abs() > 1e-3 {
            warn!("[Water] Map at {origin} has another sample spacing; its river water is left out");
            continue;
        }
        let (Some(a), Some(b)) = (images.get(look_h), images.get(flow_h)) else { continue };
        let (Some(a), Some(b)) = (a.data.as_ref(), b.data.as_ref()) else { continue };
        let n = hm.samples as usize;
        if a.len() != n * n * 4 || b.len() != n * n * 4 {
            warn!("[Water] Map at {origin}: river water images of the wrong size");
            continue;
        }
        for j in 0..n {
            for i in 0..n {
                let p = origin.xz() + hm.local_xz(i as u32, j as u32) - lo;
                let (x, z) = ((p.x / texel).round() as usize, (p.y / texel).round() as usize);
                if x >= w as usize || z >= h as usize {
                    continue;
                }
                let (src, dst) = ((j * n + i) * 4, (z * w as usize + x) * 4);
                // Shared edge samples: keep the one with river water.
                if a[src + 3] >= look[dst + 3] {
                    look[dst..dst + 4].copy_from_slice(&a[src..src + 4]);
                    flow[dst..dst + 4].copy_from_slice(&b[src..src + 4]);
                }
            }
        }
    }
    Some((min, texel, w, h, look, flow))
}

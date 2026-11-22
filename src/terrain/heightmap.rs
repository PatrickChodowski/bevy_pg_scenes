//! Terrain chunks built from a heightmap.
//!
//! A map's terrain is stored as one 16-bit greyscale PNG of the whole plane's vertex heights
//! (plus an optional RGBA8 PNG of vertex colours) instead of one mesh file per chunk. The game
//! builds each chunk's mesh from it when the chunk streams in.
//!
//! The heightmap has an *apron*: `apron` extra samples on every side, taken from the
//! neighbouring planes when the map was exported (or extrapolated where there is none). Normals
//! are central differences over the padded grid, so a vertex on a chunk or map edge gets exactly
//! the same normal from both sides: no lighting seams. The editor uses [`grid_normal`] too, so
//! terrain looks the same there.

use bevy::asset::RenderAssetUsages;
use bevy::mesh::{Indices, PrimitiveTopology};
use bevy::prelude::*;
use bevy::render::render_resource::TextureFormat;
use serde::{Deserialize, Serialize};

/// The heightmap part of a map's meta (`MapMeta::heightmap`).
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct MapHeightmap {
    /// 16-bit greyscale PNG in the map's `terrain/` folder, `samples + 2 * apron` a side.
    pub file: String,
    /// RGBA8 vertex colours (`samples` a side, no apron). None = all white.
    #[serde(default)]
    pub colors_file: Option<String>,
    /// Vertices per plane side.
    pub samples: u32,
    pub apron: u32,
    /// Plane size (world units).
    pub width: f32,
    pub depth: f32,
    /// Pixel 0 = `height_min`, 65535 = `height_max` (plane-local Y).
    pub height_min: f32,
    pub height_max: f32,
    /// Quads per chunk side.
    pub chunk_quads: u32,
}

impl MapHeightmap {
    /// Samples a side including the apron.
    pub fn stride(&self) -> usize {
        (self.samples + 2 * self.apron) as usize
    }

    /// Distance between neighbouring samples (x, z).
    pub fn spacing(&self) -> Vec2 {
        let quads = (self.samples.max(2) - 1) as f32;
        Vec2::new(self.width / quads, self.depth / quads)
    }

    /// Plane-local XZ of interior sample (i, j); (0, 0) is the -X -Z corner, like Plane3d.
    pub fn local_xz(&self, i: u32, j: u32) -> Vec2 {
        let s = self.spacing();
        Vec2::new(-self.width / 2.0 + i as f32 * s.x, -self.depth / 2.0 + j as f32 * s.y)
    }
}

/// Normal at padded-grid sample (x, z) from central differences. (x, z) must have neighbours on
/// all four sides (true for every interior sample when the apron is at least 1).
pub fn grid_normal(heights: &[f32], stride: usize, x: usize, z: usize, spacing: Vec2) -> Vec3 {
    let h = |x: usize, z: usize| heights[z * stride + x];
    let dx = (h(x + 1, z) - h(x - 1, z)) / (2.0 * spacing.x);
    let dz = (h(x, z + 1) - h(x, z - 1)) / (2.0 * spacing.y);
    Vec3::new(-dx, 1.0, -dz).normalize()
}

/// Quantises heights to 16 bits over their own range. Returns (pixels, min, max).
pub fn encode_heights(heights: &[f32]) -> (Vec<u16>, f32, f32) {
    let (min, max) = heights.iter().fold((f32::MAX, f32::MIN), |(lo, hi), &h| (lo.min(h), hi.max(h)));
    let (min, max) = if heights.is_empty() { (0.0, 0.0) } else { (min, max) };
    let span = (max - min).max(1e-6);
    let pixels = heights.iter().map(|&h| (((h - min) / span) * 65535.0).round().clamp(0.0, 65535.0) as u16).collect();
    (pixels, min, max)
}

/// Heights (plane-local Y, padded grid) from a loaded heightmap image.
pub fn decode_heights(image: &Image, hm: &MapHeightmap) -> Option<Vec<f32>> {
    let stride = match image.texture_descriptor.format {
        TextureFormat::R16Unorm | TextureFormat::R16Uint => 2,
        TextureFormat::Rgba16Unorm | TextureFormat::Rgba16Uint => 8,
        _ => return None,
    };
    let data = image.data.as_ref()?;
    let span = hm.height_max - hm.height_min;
    let heights: Vec<f32> = data
        .chunks_exact(stride)
        .map(|px| hm.height_min + u16::from_ne_bytes([px[0], px[1]]) as f32 / 65535.0 * span)
        .collect();
    (heights.len() == hm.stride() * hm.stride()).then_some(heights)
}

/// Vertex colours (linear, `samples` x `samples`) from a loaded RGBA8 image.
pub fn decode_colors(image: &Image, hm: &MapHeightmap) -> Option<Vec<[f32; 4]>> {
    if !matches!(image.texture_descriptor.format, TextureFormat::Rgba8Unorm | TextureFormat::Rgba8UnormSrgb) {
        return None;
    }
    let colors: Vec<[f32; 4]> = image
        .data
        .as_ref()?
        .chunks_exact(4)
        .map(|c| [c[0] as f32 / 255.0, c[1] as f32 / 255.0, c[2] as f32 / 255.0, c[3] as f32 / 255.0])
        .collect();
    (colors.len() == (hm.samples * hm.samples) as usize).then_some(colors)
}

/// The mesh of chunk (cx, cz), with vertices relative to `center` (the chunk's `location`).
/// UVs span the whole plane (0..1), like the editor's plane, so one splat covers every chunk.
pub fn chunk_mesh(hm: &MapHeightmap, heights: &[f32], colors: Option<&[[f32; 4]]>, cx: u32, cz: u32, center: Vec3) -> Mesh {
    let quads = hm.chunk_quads.max(1);
    let side = quads + 1;
    let (stride, apron, spacing) = (hm.stride(), hm.apron as usize, hm.spacing());
    let last = (hm.samples.max(2) - 1) as f32;

    let count = (side * side) as usize;
    let mut positions = Vec::with_capacity(count);
    let mut normals = Vec::with_capacity(count);
    let mut uvs = Vec::with_capacity(count);
    let mut vertex_colors = Vec::with_capacity(if colors.is_some() { count } else { 0 });

    for z in 0..side {
        for x in 0..side {
            let (i, j) = ((cx * quads + x).min(hm.samples - 1), (cz * quads + z).min(hm.samples - 1));
            let (px, pz) = (i as usize + apron, j as usize + apron);
            let xz = hm.local_xz(i, j);
            positions.push((Vec3::new(xz.x, heights[pz * stride + px], xz.y) - center).to_array());
            normals.push(grid_normal(heights, stride, px, pz, spacing).to_array());
            uvs.push([i as f32 / last, j as f32 / last]);
            if let Some(colors) = colors {
                vertex_colors.push(colors[(j * hm.samples + i) as usize]);
            }
        }
    }

    // Same winding as Plane3d (normals +Y).
    let mut indices = Vec::with_capacity((quads * quads * 6) as usize);
    for z in 0..quads {
        for x in 0..quads {
            let i = z * side + x;
            indices.extend_from_slice(&[i, i + side, i + 1, i + 1, i + side, i + side + 1]);
        }
    }

    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default());
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, uvs);
    if colors.is_some() {
        mesh.insert_attribute(Mesh::ATTRIBUTE_COLOR, vertex_colors);
    }
    mesh.insert_indices(Indices::U32(indices));
    mesh
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy::mesh::VertexAttributeValues;

    fn hm(samples: u32, chunk_quads: u32) -> MapHeightmap {
        MapHeightmap {
            file: String::new(),
            colors_file: None,
            samples,
            apron: 1,
            width: 10.0,
            depth: 10.0,
            height_min: 0.0,
            height_max: 1.0,
            chunk_quads,
        }
    }

    #[test]
    fn heights_round_trip_through_16_bits() {
        let heights = [-3.0, 0.0, 1.5, 12.0];
        let (px, min, max) = encode_heights(&heights);
        for (h, p) in heights.iter().zip(px) {
            let back = min + p as f32 / 65535.0 * (max - min);
            assert!((back - h).abs() < (max - min) / 65535.0, "{h} -> {back}");
        }
    }

    #[test]
    fn chunk_vertices_match_a_plane3d_of_the_same_size() {
        // 5 samples a side, flat except a bump; chunk (1, 0) of 2x2.
        let h = hm(5, 2);
        let stride = h.stride();
        let mut heights = vec![0.0; stride * stride];
        heights[stride + 4] = 2.0; // padded (4, 1) = interior sample (3, 0)
        let mesh = chunk_mesh(&h, &heights, None, 1, 0, Vec3::ZERO);
        let plane = Plane3d::default().mesh().size(10.0, 10.0).subdivisions(3).build();

        let Some(VertexAttributeValues::Float32x3(pos)) = mesh.attribute(Mesh::ATTRIBUTE_POSITION) else { panic!() };
        let Some(VertexAttributeValues::Float32x2(uv)) = mesh.attribute(Mesh::ATTRIBUTE_UV_0) else { panic!() };
        let Some(VertexAttributeValues::Float32x3(ppos)) = plane.attribute(Mesh::ATTRIBUTE_POSITION) else { panic!() };
        let Some(VertexAttributeValues::Float32x2(puv)) = plane.attribute(Mesh::ATTRIBUTE_UV_0) else { panic!() };
        // Chunk vertex (x, z) = plane vertex (2 + x, z).
        for z in 0..3 {
            for x in 0..3 {
                let (c, p) = (z * 3 + x, z * 5 + 2 + x);
                assert!((pos[c][0] - ppos[p][0]).abs() < 1e-5 && (pos[c][2] - ppos[p][2]).abs() < 1e-5, "xz of {x},{z}");
                assert!((uv[c][0] - puv[p][0]).abs() < 1e-5 && (uv[c][1] - puv[p][1]).abs() < 1e-5, "uv of {x},{z}");
            }
        }
        assert_eq!(pos[1][1], 2.0, "the bump is at chunk vertex (1, 0)");
    }

    #[test]
    fn neighbouring_chunks_get_the_same_edge_normals() {
        let h = hm(5, 2);
        let stride = h.stride();
        let heights: Vec<f32> = (0..stride * stride).map(|k| ((k % stride) as f32 * 0.7).sin() + (k / stride) as f32 * 0.3).collect();
        let left = chunk_mesh(&h, &heights, None, 0, 0, Vec3::ZERO);
        let right = chunk_mesh(&h, &heights, None, 1, 0, Vec3::ZERO);
        let Some(VertexAttributeValues::Float32x3(nl)) = left.attribute(Mesh::ATTRIBUTE_NORMAL) else { panic!() };
        let Some(VertexAttributeValues::Float32x3(nr)) = right.attribute(Mesh::ATTRIBUTE_NORMAL) else { panic!() };
        for z in 0..3 {
            assert_eq!(nl[z * 3 + 2], nr[z * 3], "shared edge vertex row {z}");
        }
    }

    #[test]
    fn normals_point_up_and_follow_the_slope() {
        let h = hm(5, 4);
        let stride = h.stride();
        let spacing = h.spacing();
        // y = x (45 degrees up towards +X).
        let heights: Vec<f32> = (0..stride * stride).map(|k| (k % stride) as f32 * spacing.x).collect();
        let n = grid_normal(&heights, stride, 3, 3, spacing);
        assert!((n - Vec3::new(-1.0, 1.0, 0.0).normalize()).length() < 1e-5, "{n}");
    }
}

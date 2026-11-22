//! Procedural, seamlessly tiling layer textures for terrain layers without their own textures:
//! colour variation (albedo) and a matching normal map from one noise height field.

use serde::{Deserialize, Serialize};

/// Shape of the generated detail.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, Default, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum NoisePattern {
    /// Soft blotches: grass, moss, mud.
    #[default]
    Smooth,
    /// Sharp ridges and cracks: rock, cliffs.
    Ridged,
    /// Blotches plus fine grain: dirt, gravel, sand.
    Speckled,
    /// Compacted, fairly even ground with scattered gravel: dirt roads, paths, yards.
    Packed,
}

/// Generates a `size`x`size` RGBA albedo (sRGB bytes) and normal map (OpenGL convention).
/// `variation` (0..1) is how much the brightness/hue varies; `bump` scales the normal map
/// (0 = flat). Everything wraps around, so it tiles without seams.
pub fn generate_layer(
    size: u32,
    color: [u8; 4],
    variation: f32,
    bump: f32,
    pattern: NoisePattern,
    seed: u32,
) -> (Vec<u8>, Vec<u8>) {
    let n = size as usize;
    let height: Vec<f32> = (0..n * n)
        .map(|i| {
            let (x, y) = ((i % n) as f32 / size as f32, (i / n) as f32 / size as f32);
            pattern_height(x, y, pattern, seed)
        })
        .collect();

    let mut albedo = vec![0u8; n * n * 4];
    let mut normal = vec![0u8; n * n * 4];
    let variation = variation.clamp(0.0, 1.0);
    // Bump scale independent of resolution: heights are per tile, gradients per pixel.
    let k = bump * size as f32 / 24.0;

    for y in 0..n {
        for x in 0..n {
            let i = y * n + x;
            let h = height[i];

            // Brightness from the height, plus a slow warm/cool hue drift.
            let brightness = 1.0 + variation * (h * 2.0 - 1.0);
            let hue = (value_noise(x as f32 / n as f32 * 2.0, y as f32 / n as f32 * 2.0, 2, seed ^ 0x9e37) - 0.5) * variation * 0.25;
            let tint = [1.0 + hue, 1.0, 1.0 - hue];
            for c in 0..3 {
                albedo[i * 4 + c] = (color[c] as f32 * brightness * tint[c]).round().clamp(0.0, 255.0) as u8;
            }
            albedo[i * 4 + 3] = 255;

            // Normal from wrapped central differences. OpenGL: green = image up = -y.
            let at = |dx: isize, dy: isize| {
                let xx = (x as isize + dx).rem_euclid(n as isize) as usize;
                let yy = (y as isize + dy).rem_euclid(n as isize) as usize;
                height[yy * n + xx]
            };
            let dhdx = (at(1, 0) - at(-1, 0)) * 0.5;
            let dhdy = (at(0, 1) - at(0, -1)) * 0.5;
            let (nx, ny, nz) = (-dhdx * k, dhdy * k, 1.0);
            let len = (nx * nx + ny * ny + nz * nz).sqrt();
            for (c, v) in [nx / len, ny / len, nz / len].into_iter().enumerate() {
                normal[i * 4 + c] = ((v * 0.5 + 0.5) * 255.0).round() as u8;
            }
            normal[i * 4 + 3] = 255;
        }
    }
    (albedo, normal)
}

/// Height in 0..1 at tile coordinates (x, y) in 0..1; periodic with period 1.
pub fn pattern_height(x: f32, y: f32, pattern: NoisePattern, seed: u32) -> f32 {
    match pattern {
        NoisePattern::Smooth => fbm(x, y, 4, 5, seed),
        NoisePattern::Ridged => {
            // Ridged multifractal: sharp crests where the noise crosses its middle value.
            let mut sum = 0.0;
            let mut amp = 0.5;
            let mut norm = 0.0;
            let mut period = 3;
            for octave in 0..5 {
                let v = value_noise(x * period as f32, y * period as f32, period, seed.wrapping_add(octave));
                let ridge = 1.0 - (v * 2.0 - 1.0).abs();
                sum += ridge * ridge * amp;
                norm += amp;
                amp *= 0.5;
                period *= 2;
            }
            sum / norm
        }
        NoisePattern::Speckled => {
            let grain = value_noise(x * 64.0, y * 64.0, 64, seed ^ 0x51ed);
            fbm(x, y, 6, 4, seed) * 0.65 + grain * 0.35
        }
        NoisePattern::Packed => {
            // Gentle base, plus sparse small stones: only the peaks of a fine noise stick out.
            let base = fbm(x, y, 3, 3, seed);
            let stones = value_noise(x * 48.0, y * 48.0, 48, seed ^ 0x2c1b);
            let stone = ((stones - 0.72) / 0.28).clamp(0.0, 1.0);
            base * 0.6 + 0.2 + stone * stone * 0.5
        }
    }
}

/// Fractal sum of periodic value noise, normalised to 0..1.
fn fbm(x: f32, y: f32, base_period: i32, octaves: u32, seed: u32) -> f32 {
    let mut sum = 0.0;
    let mut amp = 0.5;
    let mut norm = 0.0;
    let mut period = base_period;
    for octave in 0..octaves {
        sum += value_noise(x * period as f32, y * period as f32, period, seed.wrapping_add(octave)) * amp;
        norm += amp;
        amp *= 0.5;
        period *= 2;
    }
    sum / norm
}

/// Smooth value noise on an integer lattice that repeats every `period` cells.
fn value_noise(x: f32, y: f32, period: i32, seed: u32) -> f32 {
    let (x0, y0) = (x.floor(), y.floor());
    let (fx, fy) = (x - x0, y - y0);
    let (sx, sy) = (fx * fx * (3.0 - 2.0 * fx), fy * fy * (3.0 - 2.0 * fy));
    let (ix, iy) = (x0 as i32, y0 as i32);
    let corner = |dx: i32, dy: i32| hash((ix + dx).rem_euclid(period), (iy + dy).rem_euclid(period), seed);
    let top = corner(0, 0) + (corner(1, 0) - corner(0, 0)) * sx;
    let bottom = corner(0, 1) + (corner(1, 1) - corner(0, 1)) * sx;
    top + (bottom - top) * sy
}

/// Pseudo-random 0..1 for a lattice point.
fn hash(x: i32, y: i32, seed: u32) -> f32 {
    let mut h = (x as u32).wrapping_mul(0x8da6_b343) ^ (y as u32).wrapping_mul(0xd816_3841) ^ seed.wrapping_mul(0xcb1a_b31f);
    h ^= h >> 13;
    h = h.wrapping_mul(0x5bd1_e995);
    h ^= h >> 15;
    (h & 0x00ff_ffff) as f32 / 0x00ff_ffff as f32
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn patterns_tile_seamlessly() {
        for pattern in [NoisePattern::Smooth, NoisePattern::Ridged, NoisePattern::Speckled, NoisePattern::Packed] {
            for i in 0..20 {
                let t = i as f32 / 20.0;
                let (a, b) = (pattern_height(0.0, t, pattern, 7), pattern_height(1.0, t, pattern, 7));
                assert!((a - b).abs() < 1e-4, "{pattern:?} x seam at {t}: {a} vs {b}");
                let (a, b) = (pattern_height(t, 0.0, pattern, 7), pattern_height(t, 1.0, pattern, 7));
                assert!((a - b).abs() < 1e-4, "{pattern:?} y seam at {t}: {a} vs {b}");
            }
        }
    }

    #[test]
    fn generated_layer_has_variation_and_bumps() {
        let (albedo, normal) = generate_layer(64, [100, 120, 60, 255], 0.4, 1.0, NoisePattern::Ridged, 3);
        assert_eq!(albedo.len(), 64 * 64 * 4);
        let greens: Vec<u8> = albedo.chunks(4).map(|p| p[1]).collect();
        let (min, max) = (greens.iter().min().unwrap(), greens.iter().max().unwrap());
        assert!(max - min > 20, "brightness varies ({min}..{max})");
        assert!(normal.chunks(4).any(|p| p[0].abs_diff(128) > 10), "normal map isn't flat");
    }

    #[test]
    fn zero_bump_gives_flat_normals() {
        let (_, normal) = generate_layer(16, [100, 100, 100, 255], 0.3, 0.0, NoisePattern::Smooth, 1);
        assert!(normal.chunks(4).all(|p| p == [128, 128, 255, 255]));
    }
}

//! The open sea waves on the CPU: the same Gerstner waves as the water shader's vertex stage
//! (water.wesl: WAVES, wave_patch, openness, sea_wave), so gameplay can put things on the
//! surface the player sees (boats riding the swell).
//!
//! Keep this in sync with water.wesl. The time is `Time::elapsed_secs_wrapped()`, which is what
//! Bevy hands the shader as `globals.time`.

use bevy::prelude::*;

use crate::water::open_sea::OpenSeaField;
use crate::water::SeaUniform;

const TAU: f32 = std::f32::consts::TAU;
const GRAVITY: f32 = 9.81;
const WAVE_TIME: f32 = 0.7;
const WAVE_MIN_DEPTH: f32 = 0.4;
const WAVE_FULL_DEPTH: f32 = 2.5;
const TROUGH_CLEARANCE: f32 = 0.35;

/// water.wesl's limit_trough.
fn limit_trough(y: f32, depth: f32) -> f32 {
    if y >= 0.0 {
        return y;
    }
    let room = (depth - TROUGH_CLEARANCE).max(0.05);
    -room * (-y / room).tanh()
}

/// Direction (radians, relative to the swell's), wavelength, steepness, patchiness:
/// water.wesl's WAVES.
const WAVES: [[f32; 4]; 4] = [
    [0.0, 60.0, 0.16, 0.0],
    [0.4, 31.0, 0.16, 0.0],
    [-0.6, 17.0, 0.13, 0.025],
    [0.6, 9.5, 0.10, 0.04],
];

/// The sea as the shader sees it: where it's open (the baked field) and the wave settings.
#[derive(Clone, Copy)]
pub struct Sea<'a> {
    pub field: &'a OpenSeaField,
    pub uniform: &'a SeaUniform,
}

fn smoothstep(e0: f32, e1: f32, x: f32) -> f32 {
    let t = ((x - e0) / (e1 - e0)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

/// water.wesl's hash_cell.
fn hash_cell(x: i32, y: i32) -> f32 {
    let mut h = (x as u32).wrapping_mul(0x8da6b343) ^ (y as u32).wrapping_mul(0xd8163841);
    h = (h ^ (h >> 16)).wrapping_mul(0x7feb352d);
    h = (h ^ (h >> 15)).wrapping_mul(0x846ca68b);
    h ^= h >> 16;
    (h >> 8) as f32 / 16777215.0
}

/// water.wesl's hash_noise.
fn hash_noise(p: Vec2) -> f32 {
    let i = p.floor();
    let f = p - i;
    let u = f * f * (3.0 - 2.0 * f);
    let (x, y) = (i.x as i32, i.y as i32);
    let (a, b, c, d) = (hash_cell(x, y), hash_cell(x + 1, y), hash_cell(x, y + 1), hash_cell(x + 1, y + 1));
    let top = a + (b - a) * u.x;
    let bottom = c + (d - c) * u.x;
    top + (bottom - top) * u.y
}

/// water.wesl's wave_patch.
fn wave_patch(w: &[f32; 4], i: usize, xz: Vec2, time: f32, dir: f32) -> f32 {
    if w[3] <= 0.0 {
        return 1.0;
    }
    let wind = Vec2::new(dir.cos(), dir.sin());
    let p = (xz - wind * time * 1.5) * w[3] + Vec2::new(i as f32 * 5.1, i as f32 * 1.7);
    0.2 + 1.2 * hash_noise(p)
}

impl Sea<'_> {
    /// water.wesl's openness: 0 at the coast and over shallows .. 1 out at open sea.
    pub fn openness(&self, xz: Vec2) -> f32 {
        if self.uniform.enabled < 0.5 {
            return 0.0;
        }
        smoothstep(self.uniform.calm_distance, self.uniform.open_distance, self.field.distance(xz))
            * smoothstep(WAVE_MIN_DEPTH, WAVE_FULL_DEPTH, self.field.depth(xz))
    }

    /// Unit (x, z) direction the swell rolls towards (the stronger one while it turns).
    pub fn swell_towards(&self) -> Vec2 {
        let (a, b) = (self.uniform.swell_a, self.uniform.swell_b);
        let dir = if b.y > a.y { b.x } else { a.x };
        Vec2::new(dir.cos(), dir.sin())
    }

    /// How rough the sea is at `xz`: openness times wave scale (0 flat .. ~1.5 storm).
    pub fn roughness(&self, xz: Vec2) -> f32 {
        self.openness(xz) * self.uniform.wave_scale
    }

    /// Where the surface point that rests at `xz` is moved to (offset from `xz`, y = height
    /// above the water level). water.wesl's sea_wave with its strength.
    pub fn displacement(&self, xz: Vec2, time: f32) -> Vec3 {
        let strength = self.openness(xz) * self.uniform.wave_scale;
        if strength <= 0.0 {
            return Vec3::ZERO;
        }
        let t = time * WAVE_TIME;
        let mut offset = Vec3::ZERO;
        for swell in [self.uniform.swell_a, self.uniform.swell_b] {
            if swell.y <= 0.0 {
                continue;
            }
            for (i, w) in WAVES.iter().enumerate() {
                let dir = swell.x + w[0];
                let d = Vec2::new(dir.cos(), dir.sin());
                let k = TAU / w[1];
                let f = k * (d.dot(xz) - (GRAVITY / k).sqrt() * t);
                let a = w[2] * strength * swell.y * wave_patch(w, i, xz, time, swell.x) / k;
                let (sf, cf) = f.sin_cos();
                offset += Vec3::new(d.x * a * cf, a * sf, d.y * a * cf);
            }
        }
        offset.y = limit_trough(offset.y, self.field.depth(xz));
        // water.wesl's edge_fade.
        let focus = self.uniform.focus;
        if focus.z > 0.0 {
            let d = xz.distance(Vec2::new(focus.x, focus.y));
            let fade = 1.0 - smoothstep(focus.z - 45.0, focus.z - 8.0, d);
            offset.x *= fade;
            offset.z *= fade;
        }
        offset
    }

    /// Height of the surface (above the water level) at world `xz`. The waves also move the
    /// surface sideways, so this finds the resting point that ends up at `xz` first.
    pub fn height(&self, xz: Vec2, time: f32) -> f32 {
        let mut rest = xz;
        for _ in 0..3 {
            let d = self.displacement(rest, time);
            rest = xz - Vec2::new(d.x, d.z);
        }
        self.displacement(rest, time).y
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hash_noise_is_in_range_and_smooth() {
        for i in 0..200 {
            let p = Vec2::new(i as f32 * 0.37 - 30.0, i as f32 * -0.21 + 11.0);
            let n = hash_noise(p);
            assert!((0.0..=1.0).contains(&n));
            assert!((hash_noise(p + Vec2::splat(1e-3)) - n).abs() < 0.01);
        }
    }

    #[test]
    fn height_matches_displaced_point() {
        let field = OpenSeaField { min: Vec2::splat(-1000.0), texel: 8.0, width: 250, height: 250, dist: vec![255; 250 * 250], depth: vec![160; 250 * 250] };
        let uniform = SeaUniform { enabled: 1.0, ..default() };
        let sea = Sea { field: &field, uniform: &uniform };
        for i in 0..20 {
            let rest = Vec2::new(i as f32 * 7.3, i as f32 * -3.1);
            let d = sea.displacement(rest, 12.5);
            assert!(d.y.abs() > 0.0 || i == 0);
            let h = sea.height(rest + Vec2::new(d.x, d.z), 12.5);
            assert!((h - d.y).abs() < 0.02, "{h} vs {}", d.y);
        }
    }
}

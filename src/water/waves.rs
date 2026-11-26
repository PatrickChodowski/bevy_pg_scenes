//! The open sea waves on the CPU: the same Gerstner waves as the water shader's vertex stage
//! (water.wesl: WAVES, wave_patch, openness, sea_wave), so gameplay can put things on the
//! surface the player sees (boats riding the swell).
//!
//! Keep this in sync with water.wesl. The time is `Time::elapsed_secs_wrapped()`, which is what
//! Bevy hands the shader as `globals.time`.

use bevy::ecs::system::SystemParam;
use bevy::prelude::*;

use crate::water::open_sea::OpenSeaField;
use crate::water::{SeaUniform, WaterData, WaterMaterial};

/// The sea for gameplay systems: `sea()` once the field is baked and the water is there.
#[derive(SystemParam)]
pub struct SeaParams<'w> {
    field:     Option<Res<'w, OpenSeaField>>,
    water:     Option<Res<'w, WaterData>>,
    materials: Res<'w, Assets<WaterMaterial>>,
}

impl SeaParams<'_> {
    pub fn sea(&self) -> Option<Sea<'_>> {
        let field = self.field.as_deref()?;
        let material = self.materials.get(&self.water.as_ref()?.material)?;
        Some(Sea { field, uniform: &material.extension.sea })
    }

    /// Sea::ice, 0 without a sea.
    pub fn ice(&self, xz: Vec2) -> f32 {
        self.sea().map_or(0.0, |s| s.ice(xz))
    }
}

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

/// Sea ice (water.wesl's ice_at): water within ICE_SHELTER of land (world units) and shallower
/// than ICE_SHALLOW is sheltered and freezes first; out past them it's exposed, and the cold
/// has to beat ICE_EXPOSURE more. It freezes where that's over ICE_FROM, with a ragged edge.
const ICE_SHELTER: (f32, f32) = (3.0, 60.0);
const ICE_SHALLOW: (f32, f32) = (0.4, 4.0); // (the depth channel holds 0..5.1)
const ICE_EXPOSURE: f32 = 0.6;
const ICE_FROM: f32 = 0.18;
const ICE_EDGE: f32 = 0.05;
const ICE_RAGGED: f32 = 0.08;
const ICE_RAGGED_SCALE: f32 = 0.05;

impl Sea<'_> {
    /// water.wesl's openness: 0 at the coast and over shallows .. 1 out at open sea (and 0 under
    /// ice).
    pub fn openness(&self, xz: Vec2) -> f32 {
        if self.uniform.enabled < 0.5 {
            return 0.0;
        }
        smoothstep(self.uniform.calm_distance, self.uniform.open_distance, self.field.distance(xz))
            * smoothstep(WAVE_MIN_DEPTH, WAVE_FULL_DEPTH, self.field.depth(xz))
            * (1.0 - self.ice(xz))
    }

    /// How frozen the water is at `xz`: 0 open .. 1 ice (water.wesl's ice_at). Boats don't
    /// get through ice.
    pub fn ice(&self, xz: Vec2) -> f32 {
        let (u, amount) = (self.uniform.ice, self.uniform.ice_state.x);
        if amount <= 0.001 || self.uniform.enabled < 0.5 {
            return 0.0;
        }
        let cold = ((xz.dot(Vec2::new(u.x, u.y)) - u.z) / u.w).clamp(0.0, 1.0);
        let exposure = smoothstep(ICE_SHELTER.0, ICE_SHELTER.1, self.field.distance(xz)) * smoothstep(ICE_SHALLOW.0, ICE_SHALLOW.1, self.field.depth(xz));
        let ragged = (hash_noise(xz * ICE_RAGGED_SCALE) - 0.5) * ICE_RAGGED;
        let freeze = cold * amount - ICE_EXPOSURE * exposure + ragged;
        smoothstep(ICE_FROM, ICE_FROM + ICE_EDGE, freeze)
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
    fn sheltered_cold_water_freezes_open_sea_and_the_mild_west_dont() {
        // A strip of sea: near land and shallow at x < 0, far out and deep at x >= 0.
        let (w, h) = (250usize, 250usize);
        let mut dist = vec![255u8; w * h];
        let mut depth = vec![255u8; w * h];
        for j in 0..h {
            for i in 0..w / 2 {
                dist[j * w + i] = 1;
                depth[j * w + i] = 2;
            }
        }
        let field = OpenSeaField { min: Vec2::splat(-1000.0), texel: 8.0, width: w as u32, height: h as u32, dist, depth: depth.clone(), mean_depth: depth };
        // Cold rises from 0 at z = -1000 to 1 at z = 0 (towards +z).
        let mut uniform = SeaUniform { enabled: 1.0, ice: Vec4::new(0.0, 1.0, -1000.0, 1000.0), ice_state: Vec4::new(1.0, 0.0, 0.0, 0.0), ..default() };
        let sea = Sea { field: &field, uniform: &uniform };
        // Middling cold (0.5, like the central Baltic): sheltered water freezes, the open sea
        // doesn't; at full cold (Bothnia, Ladoga) even open water does.
        let sheltered_cold = Vec2::new(-500.0, -500.0);
        let open_cold = Vec2::new(500.0, -500.0);
        let open_coldest = Vec2::new(500.0, -10.0);
        let sheltered_mild = Vec2::new(-500.0, -950.0);
        assert!(sea.ice(sheltered_cold) > 0.9, "a cold sound freezes");
        assert!(sea.ice(open_cold) < 0.1, "the open sea doesn't");
        assert!(sea.ice(open_coldest) > 0.9, "where it's coldest, even the open water");
        assert!(sea.ice(sheltered_mild) < 0.1, "nor the mild coast");
        assert_eq!(sea.openness(sheltered_cold), 0.0, "no waves under ice");
        uniform.ice_state.x = 0.0;
        let sea = Sea { field: &field, uniform: &uniform };
        assert_eq!(sea.ice(sheltered_cold), 0.0, "summer");
    }

    #[test]
    fn height_matches_displaced_point() {
        let field = OpenSeaField { min: Vec2::splat(-1000.0), texel: 8.0, width: 250, height: 250, dist: vec![255; 250 * 250], depth: vec![160; 250 * 250], mean_depth: vec![160; 250 * 250] };
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

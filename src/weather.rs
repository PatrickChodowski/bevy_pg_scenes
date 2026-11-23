//! Weather. For now the wind: a direction and a strength that drift between random targets,
//! with gusts on top. The sea follows it (water/mod.rs `follow_wind`: wave height from the
//! strength, the swell turning towards its direction) and gameplay reads it (boats sail faster
//! with it behind them).
//!
//! Storms later: a storm is a [`WindDriver`] with a higher strength range and quicker, bigger
//! changes (and its own effects: rain, darker light, lightning), so the sea and the boats need
//! nothing new for it.

use bevy::prelude::*;
use rand::RngExt;

pub struct WeatherPlugin;

impl Plugin for WeatherPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<Wind>()
            .init_resource::<WindDriver>()
            .add_systems(Update, drive_wind);
    }
}

/// The wind now.
#[derive(Resource, Clone, Copy, Debug, Reflect)]
pub struct Wind {
    /// Where the wind blows *towards*, radians in the XZ plane: (cos, sin) is its (x, z)
    /// direction. The swell rolls the same way.
    pub direction: f32,
    /// Sustained strength: 0 calm .. 1 gale (storms may go above 1).
    pub strength: f32,
    /// Current gust, added to the strength (-0.3..0.3 of it): for things that feel the wind
    /// moment to moment (sails), not for the sea.
    pub gust: f32,
}

impl Default for Wind {
    fn default() -> Self {
        Self { direction: 0.35, strength: 0.5, gust: 0.0 }
    }
}

impl Wind {
    /// Unit (x, z) vector the wind blows towards.
    pub fn towards(&self) -> Vec2 {
        Vec2::new(self.direction.cos(), self.direction.sin())
    }

    /// Strength including the current gust.
    pub fn gusting(&self) -> f32 {
        (self.strength * (1.0 + self.gust)).max(0.0)
    }

    /// A name for the strength (roughly the Beaufort scale's).
    pub fn describe(&self) -> &'static str {
        match self.strength {
            s if s < 0.08 => "Calm",
            s if s < 0.25 => "Light breeze",
            s if s < 0.45 => "Moderate breeze",
            s if s < 0.65 => "Fresh breeze",
            s if s < 0.85 => "Strong wind",
            s if s < 1.0 => "Gale",
            _ => "Storm",
        }
    }
}

/// What the wind does over time: every so often it picks a new target direction and strength
/// and eases towards them. Weather (later: storms) changes the ranges.
#[derive(Resource, Clone, Debug, Reflect)]
pub struct WindDriver {
    /// Range the target strength is picked from.
    pub strength_range: (f32, f32),
    /// The target direction turns at most this far (radians) from the current one.
    pub direction_swing: f32,
    /// Seconds between new targets (picked in this range).
    pub change_every: (f32, f32),
    /// How fast the wind follows its targets: strength per second, radians per second.
    pub strength_rate: f32,
    pub turn_rate: f32,
    /// How strong the gusts are (fraction of the strength) and how often they come (per second).
    pub gustiness: f32,
    pub gust_frequency: f32,
    pub target_direction: f32,
    pub target_strength: f32,
    /// Seconds until the next target.
    pub timer: f32,
}

impl Default for WindDriver {
    fn default() -> Self {
        Self {
            strength_range: (0.2, 0.7),
            direction_swing: 0.9,
            change_every: (60.0, 180.0),
            strength_rate: 0.01,
            turn_rate: 0.012,
            gustiness: 0.25,
            gust_frequency: 0.15,
            target_direction: 0.35,
            target_strength: 0.5,
            timer: 90.0,
        }
    }
}

impl WindDriver {
    /// Sets the wind's targets now (it still eases there), and holds them for `hold` seconds.
    pub fn force(&mut self, direction: f32, strength: f32, hold: f32) {
        self.target_direction = direction;
        self.target_strength = strength.max(0.0);
        self.timer = hold;
    }
}

/// Shortest signed angle from `a` to `b`.
pub fn angle_between(a: f32, b: f32) -> f32 {
    let d = (b - a).rem_euclid(std::f32::consts::TAU);
    if d > std::f32::consts::PI { d - std::f32::consts::TAU } else { d }
}

fn drive_wind(time: Res<Time>, mut wind: ResMut<Wind>, mut driver: ResMut<WindDriver>) {
    let dt = time.delta_secs();
    driver.timer -= dt;
    if driver.timer <= 0.0 {
        let mut rng = rand::rng();
        let (lo, hi) = driver.strength_range;
        driver.target_strength = rng.random_range(lo..=hi.max(lo));
        let swing = driver.direction_swing;
        driver.target_direction = (wind.direction + rng.random_range(-swing..=swing)).rem_euclid(std::f32::consts::TAU);
        let (a, b) = driver.change_every;
        driver.timer = rng.random_range(a..=b.max(a));
    }
    let ds = driver.target_strength - wind.strength;
    wind.strength += ds.clamp(-driver.strength_rate * dt, driver.strength_rate * dt);
    let turn = angle_between(wind.direction, driver.target_direction);
    wind.direction = (wind.direction + turn.clamp(-driver.turn_rate * dt, driver.turn_rate * dt)).rem_euclid(std::f32::consts::TAU);

    // Gusts: two slow sines at unrelated frequencies, so they don't repeat visibly.
    let t = time.elapsed_secs() * driver.gust_frequency * std::f32::consts::TAU;
    wind.gust = driver.gustiness * (0.6 * t.sin() + 0.4 * (t * 2.37 + 1.3).sin()).clamp(-1.0, 1.0);
}

#[cfg(test)]
mod tests {
    use super::angle_between;

    #[test]
    fn shortest_angle() {
        assert!((angle_between(0.1, 6.2) - (6.2 - std::f32::consts::TAU - 0.1)).abs() < 1e-5);
        assert!((angle_between(6.2, 0.1) + (6.2 - std::f32::consts::TAU - 0.1)).abs() < 1e-5);
        assert!((angle_between(1.0, 2.0) - 1.0).abs() < 1e-5);
    }
}

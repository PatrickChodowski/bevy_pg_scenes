
use bevy::color::palettes::css::BLUE;
use bevy::prelude::*;
use bevy::pbr::{ExtendedMaterial, MaterialExtension};
use bevy::reflect::Reflect;
use bevy::light::NotShadowCaster;
use bevy::render::render_resource::*;
use bevy::shader::ShaderRef;
use bevy::asset::embedded_asset;
use libm::sinf;
use bevy_pg_core::prelude::{GameState, EditorAsset, WaterChunk};

pub mod depth;
pub mod open_sea;
pub mod waves;
use crate::water::depth::WaterdepthPlugin;
use crate::water::open_sea::OpenSeaPlugin;

pub struct WaterPlugin;

impl Plugin for WaterPlugin {
    fn build(&self, app: &mut App) {
        embedded_asset!(app, "water.wesl");
        app
        .insert_resource(WaveUpdate::new())
        .add_plugins(MaterialPlugin::<WaterMaterial> {
            ..default()
        })
        // .add_systems(Update, animate_water.run_if(in_state(GameState::Play)))
        .add_plugins(WaterdepthPlugin)
        .add_plugins(OpenSeaPlugin)
        .add_systems(PostUpdate, update_water_chunk_data)
        .add_systems(Update, (follow_wind, track_streaming_focus))
        .add_systems(Startup, setup_water_material)
        // After transforms are final, and before the asset events are flushed: a material change made
        // after that flush only reaches the renderer next frame, so the hull foam lagged the boat.
        .add_systems(PostUpdate, update_wakes
            .after(bevy::transform::TransformSystems::Propagate)
            .before(bevy::asset::AssetEventSystems))
        ;


    }
}

const WATER_SHADER: &str = "embedded://bevy_pg_scenes/water/water.wesl";

/// The shared water material ([`WaterData`], used by [`nice_water`] and by the chunk streamer
/// for exported water), unless the app made its own. Shoreline foam is off
/// (`water_height` outside 0..1): it needs the depth camera of `setup_water_depth`, which only
/// works for a single big water plane.
fn setup_water_material(
    mut commands: Commands,
    existing: Option<Res<WaterData>>,
    depth: Option<Res<crate::water::depth::WakeDepthTextures>>,
    mut materials: ResMut<Assets<WaterMaterial>>,
) {
    if existing.is_some() {
        return;
    }
    let material = materials.add(WaterMaterial {
        base: StandardMaterial {
            perceptual_roughness: 0.1,
            alpha_mode: AlphaMode::Blend,
            ..default()
        },
        extension: WaterMaterialExtension {
            scale: Vec2::ONE,
            water_height: 2.0,
            depth_texture: depth.map(|d| d.main_depth.clone()),
            wakes: WakeUniform::default(),
            open_sea: None,
            sea: SeaUniform::default(),
        },
    });
    commands.insert_resource(WaterData { material, animate_height: false });
}

fn update_water_chunk_data(
    mut query: Query<(&mut WaterChunk, &Transform), Changed<Transform>>
){
    for (mut chunk, transform) in query.iter_mut(){

        if chunk.just_spawned {
            chunk.just_spawned = false;
        } else {
            if transform.scale.x != chunk.dims.x {
                chunk.dims.x = transform.scale.x;
            }
            if transform.scale.z != chunk.dims.y {
                chunk.dims.y = transform.scale.z;
            }
        }
    }
}


#[derive(Clone, Resource)]
pub struct WaterData{
    pub material: Handle<WaterMaterial>,
    pub animate_height: bool
}


pub fn base_water_scene(
    loc:               &Vec3,
    dims:              &Vec2,
) -> impl Scene {
    
    let transform = Transform::from_translation(*loc);
    let asset = EditorAsset::Water;

    return bsn! {
        WaterChunk{dims: {*dims}}
        transform
        Name::from("water")
        NotShadowCaster
        asset
    }
}

pub fn nice_water(
    loc:              &Vec3,
    dims:             &Vec2,
    water_material:   Handle<WaterMaterial>,
) -> impl Scene {
    bsn! {
        @base_water_scene(loc, dims)
        MeshMaterial3d<WaterMaterial>(water_material)
        Mesh3d(asset_value(water_plane(*dims)))
    }
}

/// World units between the water mesh's vertices: the waves bend the surface, so it needs them
/// (the shortest wave is about 9 units long).
pub const WATER_VERTEX_SPACING: f32 = 1.5;

/// Quads per side of the (unit, scaled) water plane the chunk streamer uses.
pub const WATER_CHUNK_QUADS: u32 = 32;

/// A water plane of `size`, subdivided for the waves.
pub fn water_plane(size: Vec2) -> Mesh {
    let quads = (size.max_element() / WATER_VERTEX_SPACING).ceil().clamp(1.0, 256.0) as u32;
    Plane3d::default().mesh().size(size.x, size.y).subdivisions(quads - 1).build()
}

pub fn simple_water(
    loc:               &Vec3,
    dims:              &Vec2
) -> impl Scene {

    bsn! {
        @base_water_scene(loc, dims)
        MeshMaterial3d::<StandardMaterial>(asset_value(Color::from(BLUE).with_alpha(1.0)))
        Mesh3d(asset_value(Plane3d::default().mesh().size(dims.x, dims.y)))
    }
}

#[derive(Resource)]
pub struct WaveUpdate {
    pub timer: Timer,
    pub last_factor: f32
} 
impl WaveUpdate {
    fn new() ->  Self {
        WaveUpdate { timer: Timer::from_seconds(0.016, TimerMode::Repeating), last_factor: 0.0}
    }
}


fn animate_water(
    time:      Res<Time>,
    mut water: Query<&mut Transform, With<WaterChunk>>,
    mut wave:  ResMut<WaveUpdate>,
    water_data: Res<WaterData>
){
    if !water_data.animate_height {
        return
    }
    wave.timer.tick(time.delta());
    if wave.timer.just_finished(){
        let factor_y = sinf(time.elapsed_secs())*0.018;
        wave.last_factor = factor_y;
        for mut transform in water.iter_mut(){
            transform.translation.y += factor_y;
        }
    }
}


pub type WaterMaterial = ExtendedMaterial<StandardMaterial, WaterMaterialExtension>;



#[derive(Asset, AsBindGroup, Reflect, Debug, Clone)]
pub struct WaterMaterialExtension {

    #[uniform(100)]
    pub scale: Vec2,

    #[uniform(101)]
    pub water_height: f32,

    #[texture(102, sample_type = "depth")]
    #[sampler(103, sampler_type = "comparison")]
    pub depth_texture: Option<Handle<Image>>,

    /// Boats (anything with `Wakes`) near the camera: foam around their hulls and behind them
    /// (updated every frame by `update_wakes`).
    #[uniform(104)]
    pub wakes: WakeUniform,

    /// Distance to land over the world (R8: world units / `open_sea::DIST_UNIT`), baked by
    /// `open_sea`: the waves grow with it.
    #[texture(105)]
    #[sampler(106)]
    pub open_sea: Option<Handle<Image>>,

    #[uniform(107)]
    pub sea: SeaUniform,
}

/// Open sea waves.
#[derive(ShaderType, Reflect, Debug, Clone)]
pub struct SeaUniform {
    /// The `open_sea` texture's world rect: min x, min z, 1 / width, 1 / depth.
    pub rect: Vec4,
    /// Distance from land (world units) where the waves start, and where they're full size.
    pub calm_distance: f32,
    pub open_distance: f32,
    /// Wave size at full open sea (1 = the shader's wave set as designed; much more and the
    /// crests fold over). Set from the wind's strength by `follow_wind`.
    pub wave_scale: f32,
    /// 1 once the `open_sea` texture is baked; 0 = all water is coastal (flat).
    pub enabled: f32,
    /// The swell: direction it rolls towards (radians, like `Wind::direction`), weight, -, -.
    /// When the wind turns, `follow_wind` cross-fades to `swell_b` and then makes it `swell_a`.
    pub swell_a: Vec4,
    pub swell_b: Vec4,
    /// The chunk streaming focus: x, z, load radius, -. Towards the edge of the loaded area the
    /// waves stop moving the surface sideways: there's no next water plane there, and the edge
    /// pulling in would show a strip of sea floor. Radius 0 = no fade.
    pub focus: Vec4,
}

impl Default for SeaUniform {
    fn default() -> Self {
        Self {
            rect: Vec4::ZERO,
            calm_distance: 30.0,
            open_distance: 350.0,
            wave_scale: 0.8,
            enabled: 0.0,
            swell_a: Vec4::new(0.35, 1.0, 0.0, 0.0),
            swell_b: Vec4::ZERO,
            focus: Vec4::ZERO,
        }
    }
}

/// Keeps `SeaUniform::focus` on the chunk streaming focus (updated when it has moved a little).
fn track_streaming_focus(
    streaming: Option<Res<crate::follower::ChunkStreaming>>,
    focus: Query<&GlobalTransform, With<crate::types::StreamingFocus>>,
    water: Option<Res<WaterData>>,
    mut materials: ResMut<Assets<WaterMaterial>>,
) {
    let (Some(streaming), Some(water)) = (streaming, water) else { return };
    let Ok(focus) = focus.single() else { return };
    let p = focus.translation().xz();
    let want = Vec4::new(p.x, p.y, streaming.load_radius, 0.0);
    let Some(material) = materials.get(&water.material) else { return };
    let old = material.extension.sea.focus;
    if old.xy().distance(want.xy()) > 2.0 || old.z != want.z {
        if let Some(mut material) = materials.get_mut(&water.material) {
            material.extension.sea.focus = want;
        }
    }
}

/// Wave scale in a calm and in a gale (`Wind::strength` 0 and 1); storms above 1 go a little
/// further, up to MAX_WAVE_SCALE (past it the crests fold over).
const CALM_WAVE_SCALE: f32 = 0.15;
const GALE_WAVE_SCALE: f32 = 1.35;
const MAX_WAVE_SCALE: f32 = 1.5;
/// The swell turns (cross-fades to the wind's direction) once the wind is this far off it
/// (radians), over this many seconds.
const SWELL_TURN: f32 = 0.35;
const SWELL_FADE_SECS: f32 = 25.0;

/// The sea follows the wind: wave height from its strength, and the swell turning to its
/// direction (a cross-fade from the old swell to a new one; waves can't be rotated in place).
fn follow_wind(
    time: Res<Time>,
    wind: Res<crate::weather::Wind>,
    water: Option<Res<WaterData>>,
    mut materials: ResMut<Assets<WaterMaterial>>,
    mut fade: Local<Option<f32>>,
) {
    let Some(water) = water else { return };
    let Some(material) = materials.get(&water.material) else { return };
    let mut sea = material.extension.sea.clone();

    let target = (CALM_WAVE_SCALE + (GALE_WAVE_SCALE - CALM_WAVE_SCALE) * wind.strength).clamp(0.0, MAX_WAVE_SCALE);
    // The sea builds up and calms down slower than the wind changes.
    sea.wave_scale += (target - sea.wave_scale) * (time.delta_secs() * 0.1).min(1.0);

    match *fade {
        Some(progress) => {
            let p = (progress + time.delta_secs() / SWELL_FADE_SECS).min(1.0);
            // Constant energy: cos / sin weights.
            let angle = p * std::f32::consts::FRAC_PI_2;
            sea.swell_a.y = angle.cos();
            sea.swell_b.y = angle.sin();
            *fade = Some(p);
            if p >= 1.0 {
                sea.swell_a = Vec4::new(sea.swell_b.x, 1.0, 0.0, 0.0);
                sea.swell_b = Vec4::ZERO;
                *fade = None;
            }
        }
        None if crate::weather::angle_between(sea.swell_a.x, wind.direction).abs() > SWELL_TURN => {
            sea.swell_b = Vec4::new(wind.direction, 0.0, 0.0, 0.0);
            *fade = Some(0.0);
        }
        None => {}
    }

    let old = &material.extension.sea;
    let changed = (old.wave_scale - sea.wave_scale).abs() > 1e-4 || old.swell_a != sea.swell_a || old.swell_b != sea.swell_b;
    if changed {
        if let Some(mut material) = materials.get_mut(&water.material) {
            material.extension.sea = sea;
        }
    }
}

/// How many boats get foam at once (the nearest to the camera).
pub const MAX_WAKES: usize = 16;
/// Wake trail points shared by all boats (each boat's trail is a run of consecutive points).
pub const MAX_TRAIL: usize = 128;
/// A boat drops a trail point every this many world units.
const TRAIL_SPACING: f32 = 2.5;
/// Seconds a trail point lasts: the wake's length is this times the boat's speed.
const TRAIL_LIFE: f32 = 3.5;
/// Speed (units/s) at which the wake is at full strength; keep equal to the shader's.
const WAKE_FULL_SPEED: f32 = 12.0;

#[derive(ShaderType, Reflect, Debug, Clone)]
pub struct WakeUniform {
    pub count: u32,
    /// Per boat: world x, z, and its forward direction (x, z).
    pub items: [Vec4; MAX_WAKES],
    /// Per boat: hull half length, half width, speed (units/s), unused.
    pub sizes: [Vec4; MAX_WAKES],
    pub trail_count: u32,
    /// Trail points, oldest first per boat: world x, z, age (0 new .. 1 gone), hull half width.
    pub trail: [Vec4; MAX_TRAIL],
    /// Per trail point: strength (0..1, the boat's speed then), 1 if it starts a boat's trail, -, -.
    pub trail_info: [Vec4; MAX_TRAIL],
}

impl Default for WakeUniform {
    fn default() -> Self {
        Self {
            count: 0,
            items: [Vec4::ZERO; MAX_WAKES],
            sizes: [Vec4::ZERO; MAX_WAKES],
            trail_count: 0,
            trail: [Vec4::ZERO; MAX_TRAIL],
            trail_info: [Vec4::ZERO; MAX_TRAIL],
        }
    }
}

/// One point of a boat's wake: where its stern was, when, and how fast it was going (0..1).
#[derive(Clone, Copy)]
struct TrailPoint {
    pos: Vec2,
    time: f32,
    strength: f32,
}

/// Per wake source: last hull centre, heading, smoothed speed, and its trail.
#[derive(Default)]
struct WakeState {
    pos: Vec3,
    dir: Vec2,
    speed: f32,
    trail: std::collections::VecDeque<TrailPoint>,
}

#[derive(Default)]
struct WakeHistory(bevy::platform::collections::HashMap<Entity, WakeState>);

/// Fills the water material's wake list from the boats nearest the camera.
fn update_wakes(
    sources: Query<(Entity, &GlobalTransform, Option<&bevy::camera::primitives::Aabb>), With<crate::water::depth::Wakes>>,
    cameras: Query<&GlobalTransform, With<Camera3d>>,
    water: Option<Res<WaterData>>,
    mut materials: ResMut<Assets<WaterMaterial>>,
    time: Res<Time>,
    mut history: Local<WakeHistory>,
) {
    let Some(water) = water else { return };
    let dt = time.delta_secs().max(1e-4);
    let now = time.elapsed_secs();
    let eye = cameras.iter().next().map(|c| c.translation()).unwrap_or(Vec3::ZERO);

    // (distance to the camera, entity, item, size)
    let mut boats: Vec<(f32, Entity, Vec4, Vec4)> = Vec::new();
    let mut seen = Vec::new();
    for (entity, transform, aabb) in &sources {
        let (scale, rotation, _) = transform.to_scale_rotation_translation();
        // The hull's centre (the model's origin may be anywhere, e.g. at the stern).
        let pos = aabb.map_or(transform.translation(), |a| transform.transform_point(Vec3::from(a.center)));
        // Hull size from the mesh bounds: the longest horizontal axis is its length.
        let (mut half_len, mut half_wid, mut long_axis) = (6.0, 2.0, Vec2::Y);
        if let Some(aabb) = aabb {
            let half = Vec3::from(aabb.half_extents) * scale;
            let mut axes: Vec<(f32, Vec2)> = [(half.x, Vec3::X), (half.y, Vec3::Y), (half.z, Vec3::Z)]
                .into_iter()
                .map(|(h, axis)| (h, (rotation * axis).xz()))
                .filter(|(_, dir)| dir.length() > 0.7) // horizontal in the world
                .map(|(h, dir)| (h, dir.normalize()))
                .collect();
            axes.sort_by(|a, b| b.0.total_cmp(&a.0));
            if let Some(&(h, dir)) = axes.first() {
                (half_len, long_axis) = (h, dir);
            }
            if let Some(&(h, _)) = axes.get(1) {
                half_wid = h;
            }
        }
        let state = history.0.entry(entity).or_insert_with(|| WakeState { pos, dir: long_axis, ..default() });
        let velocity = (pos - state.pos).xz() / dt;
        let raw_speed = velocity.length();
        // Smoothed (~0.5 s), so frame jitter doesn't make the wake flicker.
        state.speed += (raw_speed - state.speed) * (dt * 2.0).min(1.0);
        // Heading: along the hull, towards where it's going (when moving), else the end closest to
        // the last heading. Only travel roughly along the hull counts: a boat rocking on the waves
        // also moves its centre a little sideways.
        let travel = if raw_speed > 0.2 { velocity / raw_speed } else { state.dir };
        state.dir = if long_axis.dot(travel) >= 0.0 { long_axis } else { -long_axis };
        state.pos = pos;

        // The trail: a point at the stern every TRAIL_SPACING while moving; old points expire.
        let stern = pos.xz() - state.dir * half_len * 0.8;
        let strength = (state.speed / WAKE_FULL_SPEED).clamp(0.0, 1.0);
        if strength > 0.05 && state.trail.back().is_none_or(|p| p.pos.distance(stern) >= TRAIL_SPACING) {
            state.trail.push_back(TrailPoint { pos: stern, time: now, strength });
        }
        while state.trail.front().is_some_and(|p| now - p.time > TRAIL_LIFE) {
            state.trail.pop_front();
        }

        seen.push(entity);
        boats.push((
            pos.distance_squared(eye),
            entity,
            Vec4::new(pos.x, pos.z, state.dir.x, state.dir.y),
            Vec4::new(half_len, half_wid, state.speed, 0.0),
        ));
    }
    history.0.retain(|e, _| seen.contains(e));
    boats.sort_by(|a, b| a.0.total_cmp(&b.0));

    let mut uniform = WakeUniform::default();
    let mut n = 0;
    for (k, (_, entity, item, size)) in boats.iter().take(MAX_WAKES).enumerate() {
        uniform.items[k] = *item;
        uniform.sizes[k] = *size;
        uniform.count = k as u32 + 1;

        // The boat's trail, then a live point at the stern now, so the wake reaches the boat.
        let state = &history.0[entity];
        if state.trail.is_empty() {
            continue;
        }
        let stern = Vec2::new(item.x, item.y) - Vec2::new(item.z, item.w) * size.x * 0.8;
        let head = TrailPoint { pos: stern, time: now, strength: (size.z / WAKE_FULL_SPEED).clamp(0.0, 1.0) };
        let points: Vec<TrailPoint> = state.trail.iter().copied().chain(std::iter::once(head)).collect();
        // If the list is nearly full, keep this boat's newest points.
        let room = MAX_TRAIL - n;
        if room < 2 {
            break;
        }
        let half_wid = size.y * 0.38; // the hull at the waterline, without the oars (shader's HULL_WIDTH)
        for (j, p) in points[points.len().saturating_sub(room)..].iter().enumerate() {
            let age = ((now - p.time) / TRAIL_LIFE).clamp(0.0, 1.0);
            uniform.trail[n] = Vec4::new(p.pos.x, p.pos.y, age, half_wid);
            uniform.trail_info[n] = Vec4::new(p.strength, if j == 0 { 1.0 } else { 0.0 }, 0.0, 0.0);
            n += 1;
        }
    }
    uniform.trail_count = n as u32;
    let Some(material) = materials.get(&water.material) else { return };
    if uniform.count == 0 && material.extension.wakes.count == 0 && material.extension.wakes.trail_count == 0 {
        return; // nothing to update (keeps the material from re-uploading every frame)
    }
    if let Some(mut material) = materials.get_mut(&water.material) {
        material.extension.wakes = uniform;
    }
}


impl MaterialExtension for WaterMaterialExtension {
    fn enable_prepass() -> bool { false }
    fn enable_shadows() -> bool { true }

    fn vertex_shader() -> ShaderRef {
        WATER_SHADER.into()
    }
    fn fragment_shader() -> ShaderRef {
        WATER_SHADER.into()
    }
    fn deferred_fragment_shader() -> ShaderRef {
        WATER_SHADER.into()
    }
}

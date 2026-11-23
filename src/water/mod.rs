
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
use crate::water::depth::WaterdepthPlugin;

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
        .add_systems(PostUpdate, update_water_chunk_data)
        .add_systems(Startup, setup_water_material)
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
        Mesh3d(asset_value(Plane3d::default().mesh().size(dims.x, dims.y)))
    }
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

    // #[texture(104, sample_type = "depth")]
    // #[sampler(105, sampler_type = "comparison")]
    // pub wakes_texture: Option<Handle<Image>>
}


impl MaterialExtension for WaterMaterialExtension {
    fn enable_prepass() -> bool { false }
    fn enable_shadows() -> bool { true }

    fn fragment_shader() -> ShaderRef {
        WATER_SHADER.into()
    }
    fn deferred_fragment_shader() -> ShaderRef {
        WATER_SHADER.into()
    }
}

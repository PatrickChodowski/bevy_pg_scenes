
use bevy::prelude::*;
use bevy::pbr::{ExtendedMaterial, MaterialExtension};
use bevy::reflect::Reflect;
use bevy::light::NotShadowCaster;
use bevy::render::render_resource::*;
use bevy::shader::ShaderRef;
use libm::sinf;
use bevy_pg_core::prelude::GameState;

use crate::scenes::{Chunk, Tile};

pub mod depth;
use crate::water::depth::WaterdepthPlugin;

pub const WATER_HEIGHT: f32 = 180.0;

pub struct WaterPlugin;

impl Plugin for WaterPlugin {
    fn build(&self, app: &mut App) {
        app
        .insert_resource(WaveUpdate::new())
        .add_plugins(MaterialPlugin::<WaterMaterial> {
            prepass_enabled: false,
            shadows_enabled: true,
            ..default()
        })
        .add_systems(Update, animate_water.run_if(in_state(GameState::Play)))
        .add_plugins(WaterdepthPlugin)
        ;


    }
}


#[derive(Clone, Resource)]
pub struct WaterData{
    pub material: Handle<WaterMaterial>
}



pub fn water_bundle(
    meshes:   &mut ResMut<Assets<Mesh>>,
    tile:     &Tile,
    chunk:    &Chunk,
    water_material:    Handle<WaterMaterial>,
    water_dim: f32
) -> impl Bundle {
    return(
        Mesh3d(meshes.add(Plane3d::default().mesh().size(water_dim, water_dim))),
        MeshMaterial3d(water_material),
        Transform::from_xyz(chunk.loc_x, WATER_HEIGHT, chunk.loc_y),
        WaterChunk{
            tile: *tile
        }, 
        Name::from("water"),
        NotShadowCaster,
        Pickable::IGNORE,
        DespawnOnExit(GameState::Play),
    );
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
    mut wave:  ResMut<WaveUpdate>
){
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
    fn fragment_shader() -> ShaderRef {
        "shaders/water.wgsl".into()
    }
    fn deferred_fragment_shader() -> ShaderRef {
        "shaders/water.wgsl".into()
    }
}

#[derive(Component, Debug, Reflect)]

pub struct WaterChunk {
    pub tile: Tile
}

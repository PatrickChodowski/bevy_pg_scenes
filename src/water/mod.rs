
use bevy::color::palettes::css::{WHITE, BLUE};
use bevy::prelude::*;
use bevy::pbr::{ExtendedMaterial, MaterialExtension};
use bevy::reflect::Reflect;
use bevy::light::NotShadowCaster;
use bevy::render::render_resource::*;
use bevy::shader::ShaderRef;
use libm::sinf;
use bevy_pg_core::prelude::{GameState, Tile};

// use crate::scenes::Chunk;

pub mod depth;
use crate::water::depth::WaterdepthPlugin;

pub struct WaterPlugin;

impl Plugin for WaterPlugin {
    fn build(&self, app: &mut App) {
        app
        .insert_resource(WaveUpdate::new())
        .add_plugins(MaterialPlugin::<WaterMaterial> {
            ..default()
        })
        
        .add_systems(Update, animate_water.run_if(in_state(GameState::Play)))
        .add_plugins(WaterdepthPlugin)
        ;


    }
}


#[derive(Clone, Resource)]
pub struct WaterData{
    pub material: Handle<WaterMaterial>,
    pub animate_height: bool
}



pub fn spawn_water(
    commands:          &mut Commands,
    meshes:            &mut ResMut<Assets<Mesh>>,
    materials:         &mut ResMut<Assets<StandardMaterial>>,
    water_material:    Handle<WaterMaterial>,
    loc:               &Vec3,
    dims:              &Vec2,
    for_editor:        bool
) -> Entity {
    let water_entity = commands.spawn((
        Transform::from_translation(*loc),
        WaterChunk{dims: *dims},
        Name::from("water"),
        NotShadowCaster,
        Pickable{should_block_lower: true, ..default()},
        DespawnOnExit(GameState::Play),
    )).id();

    water_mm(water_entity, commands, meshes, materials, water_material, dims, for_editor);

    return water_entity;
}

pub fn water_mm(
    water_entity:      Entity,
    commands:          &mut Commands,
    meshes:            &mut ResMut<Assets<Mesh>>,
    materials:         &mut ResMut<Assets<StandardMaterial>>,
    water_material:    Handle<WaterMaterial>,
    dims: &Vec2,
    for_editor: bool
) {
    if for_editor {
        commands.entity(water_entity).insert(
            (
                Mesh3d(meshes.add(Plane3d::default().mesh().size(dims.x, dims.y))),
                MeshMaterial3d(materials.add(StandardMaterial::from_color(Color::from(BLUE).with_alpha(1.0))))
            )
        );
    } else {
        commands.entity(water_entity).insert(
            (
                Mesh3d(meshes.add(Plane3d::default().mesh().size(dims.x, dims.y))),
                MeshMaterial3d(water_material),
            )
        );
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
        "shaders/water.wgsl".into()
    }
    fn deferred_fragment_shader() -> ShaderRef {
        "shaders/water.wgsl".into()
    }
}

#[derive(Component, Debug, Reflect)]

pub struct WaterChunk{
    pub dims: Vec2
}
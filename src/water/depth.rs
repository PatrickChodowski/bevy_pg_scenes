use std::f32::consts::FRAC_PI_2;

use bevy::{
    asset::RenderAssetUsages, camera::RenderTarget, core_pipeline::{
        core_3d::graph::{Core3d, Node3d},
        prepass::DepthPrepass,
    }, ecs::{query::QueryItem, system::lifetimeless::Read}, image::{ImageCompareFunction, ImageSampler, ImageSamplerDescriptor}, prelude::*, render::{
        RenderApp, camera::ExtractedCamera, extract_resource::{ExtractResource, ExtractResourcePlugin}, render_asset::RenderAssets, render_graph::{
            NodeRunError, RenderGraphContext, RenderGraphExt as _, RenderLabel, ViewNode,
            ViewNodeRunner,
        }, render_resource::{CommandEncoderDescriptor, Extent3d, Origin3d, TexelCopyTextureInfo, TextureAspect, TextureDimension, TextureFormat
        }, renderer::RenderContext, texture::GpuImage, view::ViewDepthTexture
    }
};
use bevy::camera::visibility::RenderLayers;

use bevy_pg_core::prelude::GameState;
use crate::water::{WaterData, WaterMaterial, WaterMaterialExtension, WATER_HEIGHT};

const MAIN_CAMERA_ORDER: isize = -1;
const DEPTH_RENDER_LAYER: usize = 1;
const DEPTH_TEXTURE_SIZE: u32 = 4096;

pub fn render_to_depth() -> RenderLayers {
    return RenderLayers::layer(0).with(DEPTH_RENDER_LAYER);
}
pub struct WaterdepthPlugin;

impl Plugin for WaterdepthPlugin {
    fn build(&self, app: &mut App) {
        app
        .add_plugins(ExtractResourcePlugin::<WakeDepthTextures>::default())
        .init_resource::<WakeDepthTextures>()
        .add_systems(OnEnter(GameState::Play), setup_water_depth)
        .add_systems(Update, spawn_wakes.run_if(in_state(GameState::Play)))
        ;

        let render_app = app.get_sub_app_mut(RenderApp).expect("Render app should be present");
        render_app.add_render_graph_node::<ViewNodeRunner<CopyDepthTextureNode>>(
            Core3d,
            CopyDepthTexturePass,
        );
        render_app.add_render_graph_edges(
            Core3d,
            (
                Node3d::EndPrepasses,
                CopyDepthTexturePass,
                Node3d::MainOpaquePass,
            ),
        );

    }
}

#[derive(Component)]
pub struct WakeHitter {
    pub entity: Entity
}

#[derive(Component)]
pub struct Wakes {
    pub entities: Vec<Entity>
}
impl Wakes {
    pub fn new() -> Self {
        Wakes {
            entities: Vec::new()
        }
    }
    pub fn add(&mut self, entity: Entity){
        self.entities.push(entity);
    }
}

// Later will be done by some marker component etc.
fn spawn_wakes(
    mut names:     Query<(Entity, &Transform, &Name, &mut Wakes), Added<Wakes>>,
    mut commands:  Commands,
    ass:           Res<AssetServer>,
    mut meshes:    ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>
){

    for (entity, &transform, name, mut wakes) in names.iter_mut(){

        match name.as_str() {
            "SM_Veh_Boat_01" => {
                    let loc = transform.translation;
                    let s2 = transform.scale*Vec3::new(6.0, 2.5, 6.0);
                    let wake_entity = wake_cone(
                        entity, 
                        Vec3::new(loc.x, loc.y - 4.1*transform.scale.y, loc.z), 
                        s2, &ass, &mut materials, &mut commands
                    );
                    wakes.add(wake_entity);
            }
            "SM_Veh_Oars_01" => {}
            "SM_Prop_Dock_01" => {
                    // 4 poles
                    let loc = transform.translation;
                    let y_angle =  -transform.rotation.to_euler(EulerRot::ZYX).1;
                    let o = Vec2::new(1.72, 1.97)*transform.scale.xz();
                    let y = WATER_HEIGHT as f32 -1.1*transform.scale.y;
                    let locs = vec![
                        rotate_point(&Vec3::new(loc.x + o.x, y, loc.z+o.y), &loc, y_angle),
                        rotate_point(&Vec3::new(loc.x - o.x, y, loc.z+o.y), &loc, y_angle),
                        rotate_point(&Vec3::new(loc.x + o.x, y, loc.z-o.y), &loc, y_angle),
                        rotate_point(&Vec3::new(loc.x - o.x, y, loc.z-o.y), &loc, y_angle),
                    ];
                    let s2 = transform.scale/2.0;
                    for wloc in locs {
                        let wake_entity = wake_conical_frustum(entity, wloc, s2, &mut meshes, &mut materials, &mut commands);
                        wakes.add(wake_entity);
                    }
            }
            "SM_Prop_Dock_02" => {
                    // 2 poles
                    let loc = transform.translation;
                    let y_angle =  -transform.rotation.to_euler(EulerRot::ZYX).1;
                    let o = Vec2::new(1.72, 1.97)*transform.scale.xz();
                    let y = WATER_HEIGHT -1.1*transform.scale.y;
                    let locs = vec![
                        rotate_point(&Vec3::new(loc.x + o.x, y, loc.z+o.y), &loc, y_angle),
                        rotate_point(&Vec3::new(loc.x - o.x, y, loc.z+o.y), &loc, y_angle)
                    ];
                    let s2 = transform.scale/2.0;
                    for wloc in locs {
                        let wake_entity = wake_conical_frustum(entity, wloc, s2, &mut meshes, &mut materials, &mut commands);
                        wakes.add(wake_entity);
                    }
            }
            "SM_Bld_BoatHouse_01" => {
                    let wake_entity = wake_conical_frustum(entity, transform.translation, transform.scale, &mut meshes, &mut materials, &mut commands);
                    wakes.add(wake_entity);
            }
            "SM_Bld_BoatHouse_02" => {
                    let wake_entity = wake_conical_frustum(entity, transform.translation, transform.scale, &mut meshes, &mut materials, &mut commands);
                    wakes.add(wake_entity);
            }
            _ => {continue;}
        }
    }
}


pub fn wake_cone(
    entity: Entity,
    loc: Vec3,
    scale: Vec3,
    ass:    &Res<AssetServer>,
    materials: &mut ResMut<Assets<StandardMaterial>>,
    commands: &mut Commands
) -> Entity {

    let cone_mesh: Handle<Mesh> = ass.load(
        GltfAssetLabel::Primitive{primitive:0, mesh:0}.from_asset("utils/WakeCone.glb"),
    );

    return commands.spawn((
        Transform::from_translation(loc).with_scale(scale),
        // render_to_depth(),
        RenderLayers::layer(DEPTH_RENDER_LAYER),
        WakeHitter{entity},
        Mesh3d(cone_mesh),
        MeshMaterial3d(materials.add(StandardMaterial::from_color(Color::WHITE.with_alpha(1.0))))
    )).id();
}


pub fn wake_conical_frustum(
    entity: Entity,
    loc: Vec3,
    scale: Vec3,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<StandardMaterial>>,
    commands: &mut Commands
) -> Entity {

    return commands.spawn((
        Transform::from_translation(loc).with_scale(scale),
        // render_to_depth(),
        RenderLayers::layer(DEPTH_RENDER_LAYER),
        WakeHitter{entity},
        Mesh3d(meshes.add(ConicalFrustum{radius_bottom: 2.5, radius_top: 0.3, height: 5.0})),
        MeshMaterial3d(materials.add(StandardMaterial::from_color(Color::WHITE.with_alpha(1.0))))
    )).id();
}

pub fn setup_water_depth(
    mut commands:        Commands,
    wake_depth_textures: Res<WakeDepthTextures>,
    mut water_materials: ResMut<Assets<WaterMaterial>>
){

    let min_point: f32 = 0.0;
    let max_point: f32 = 500.0;
    let plane_size = 5000.0;

    let mut camera_transform = Transform::from_xyz(22471.0, max_point, 17500.0);
    camera_transform.look_at(Vec3::new(22471.0, min_point, 17500.0), Vec3::Y);
    camera_transform.rotate_y(-FRAC_PI_2);

    let half_size = plane_size / 2.0;
    let projection = OrthographicProjection {
        near: min_point,
        far: max_point,
        viewport_origin: Vec2::new(0.5, 0.5),
        scaling_mode: bevy::camera::ScalingMode::Fixed{width:plane_size, height:plane_size}, // important: disables auto-resize scaling
        scale: 1.0,
        area: Rect {
            min: Vec2::new(-half_size, -half_size),
            max: Vec2::new(half_size, half_size),
        },
    };

    commands.spawn((
        Camera3d::default(),
        DespawnOnExit(GameState::Play),
        Camera {
            target: RenderTarget::None {
                size: UVec2::splat(DEPTH_TEXTURE_SIZE),
            },
            order: MAIN_CAMERA_ORDER,
            ..Camera::default()
        },
        Projection::Orthographic(projection.clone()),
        DepthPrepass,
        RenderLayers::layer(DEPTH_RENDER_LAYER),
        Msaa::Off,
        camera_transform,
    ));
    
    let water_height_depth: f32 = (WATER_HEIGHT - min_point)/(max_point - min_point);
    let water_material = water_materials.add(
        WaterMaterial {
            base: StandardMaterial {
                perceptual_roughness: 0.10,
                alpha_mode: AlphaMode::Blend,
                ..default()
            },
            extension: WaterMaterialExtension{
                scale: Vec2::ONE,  
                water_height: water_height_depth,
                depth_texture: Some(wake_depth_textures.main_depth.clone())
            }
    });
    commands.insert_resource(WaterData{
        material: water_material
    });
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, RenderLabel)]
struct CopyDepthTexturePass;

#[derive(Default)]
struct CopyDepthTextureNode;

#[derive(Clone, Resource)]
pub struct WakeDepthTextures{
    pub main_depth: Handle<Image>
}

impl WakeDepthTextures {
    fn new(
        main_depth: Handle<Image>
    ) -> Self {
        WakeDepthTextures{
            main_depth
        }
    }
}

impl ViewNode for CopyDepthTextureNode {
    type ViewQuery = (Read<ExtractedCamera>, Read<ViewDepthTexture>);

    fn run<'w>(
        &self,
        _: &mut RenderGraphContext,
        render_context: &mut RenderContext<'w>,
        (camera, depth_texture): QueryItem<'w, '_, Self::ViewQuery>,
        world: &'w World,
    ) -> Result<(), NodeRunError> {

        // Grab the texture we're going to copy to.
        let wake_depth_textures = world.resource::<WakeDepthTextures>();
        let depth_image: &GpuImage;
        match camera.order {
            MAIN_CAMERA_ORDER => {
                let image_assets = world.resource::<RenderAssets<GpuImage>>();
                if let Some(main_depth_image) = image_assets.get(wake_depth_textures.main_depth.id()){
                    depth_image = main_depth_image;
                } else {
                    return Ok(());
                };
            }
            _ => {return Ok(())}
        } 

        // Perform the copy.
        render_context.add_command_buffer_generation_task(move |render_device| {
            let mut command_encoder =
                render_device.create_command_encoder(&CommandEncoderDescriptor {
                    label: Some("copy depth to demo texture command encoder"),
                });
            command_encoder.push_debug_group("copy depth to demo texture");

            // Copy from the view's depth texture to the destination depth
            // texture.
            command_encoder.copy_texture_to_texture(
                TexelCopyTextureInfo {
                    texture: &depth_texture.texture,
                    mip_level: 0,
                    origin: Origin3d::default(),
                    aspect: TextureAspect::DepthOnly,
                },
                TexelCopyTextureInfo {
                    texture: &depth_image.texture,
                    mip_level: 0,
                    origin: Origin3d::default(),
                    aspect: TextureAspect::DepthOnly,
                },
                Extent3d {
                    width: DEPTH_TEXTURE_SIZE,
                    height: DEPTH_TEXTURE_SIZE,
                    depth_or_array_layers: 1,
                },
            );

            command_encoder.pop_debug_group();
            command_encoder.finish()
        });

        Ok(())
    }
}

impl FromWorld for WakeDepthTextures {
    fn from_world(world: &mut World) -> Self {
        let mut images = world.resource_mut::<Assets<Image>>();

        let mut depth_image = Image::new_uninit(
            Extent3d {
                width: DEPTH_TEXTURE_SIZE,
                height: DEPTH_TEXTURE_SIZE,
                depth_or_array_layers: 1,
            },
            TextureDimension::D2,
            TextureFormat::Depth32Float,
            RenderAssetUsages::default(),
        );

        depth_image.sampler = ImageSampler::Descriptor(ImageSamplerDescriptor {
            label: Some("custom depth image sampler".to_owned()),
            compare: Some(ImageCompareFunction::Always),
            ..ImageSamplerDescriptor::default()
        });

        let depth_image_handle = images.add(depth_image);
        return WakeDepthTextures::new(depth_image_handle);
    }
}

impl ExtractResource for WakeDepthTextures {
    type Source = Self;
    fn extract_resource(source: &Self::Source) -> Self {
        (*source).clone()
    }
}




fn rotate_point(p: &Vec3, origin: &Vec3, angle_y: f32) -> Vec3 {

    // translate point back to origin:
    let p2x = p.x - origin.x;
    let p2z = p.z - origin.z;
  
    let x_new = p2x * angle_y.cos() - p2z * angle_y.sin();
    let z_new = p2x * angle_y.sin() + p2z * angle_y.cos();
    
    // translate point back:
    let p3x = x_new + origin.x;
    let p3z = z_new + origin.z;
    return Vec3::new(p3x, p.y, p3z);
  }

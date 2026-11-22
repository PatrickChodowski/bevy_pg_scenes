use bevy::prelude::*;
use bevy::platform::collections::HashSet;
use bevy_pg_core::prelude::EditorAsset;
use std::{
    collections::{HashMap},
    time::Duration,
};
use bevy::mesh::{MeshDeserializer, SerializedMesh};
use crate::types::{MapsData, MapChunk, MapMeta};
use bevy::{asset::LoadState, time::common_conditions::on_timer};
use bevy::asset::{io::Reader, AssetLoader, LoadContext};
use crate::prelude::SceneData;
use crate::types::StreamingFocus;
use bevy_pg_nav::prelude::{PGNavmesh, PGNavmeshType};
use crate::terrain::{TerrainLayers, TerrainMaterial, new_splat_image, splat_loader_settings, terrain_material};
use crate::terrain::heightmap::{chunk_mesh, decode_colors, decode_heights};
use bevy::asset::RenderAssetUsages;
use bevy::image::ImageLoaderSettings;


pub struct PGSceneFollowerPlugin;

impl Plugin for PGSceneFollowerPlugin {
    fn build(&self, app: &mut App) {
        app
        .init_asset_loader::<SerializedMeshLoader>()
        .init_resource::<LoadedNavmeshes>()
        .init_resource::<DesiredChunks>()
        .init_resource::<LoadedChunks>()
        .insert_resource(ChunkStreaming { load_radius: 60.0, unload_radius: 80.0 })
        .init_resource::<ChunkMaterials>()
        .add_systems(
            Update,
            (
                (
                    locate_chunks.run_if(on_timer(Duration::from_millis(250))),
                    sync_navmeshes,
                    redraw_chunks.run_if(resource_changed::<DesiredChunks>),
                )
                    .chain(),
                spawn_chunk_objects,
                build_heightmap_chunks,
                attach_navmeshes,
            )
                .run_if(resource_exists::<MapsData>),
        );
        ;
    }
}



// Follows player entity and manages spawning and despawning chunks
#[derive(Component, Clone, Default)]
pub struct MapNavmesh(pub String);


#[derive(Component)]
struct NavmeshPending {
    handle: Handle<PGNavmesh>,
    kind: PGNavmeshType,
}

/// The navmesh entities of the maps near the focus: terrain (people) and water (boats) per map.
#[derive(Resource, Default)]
pub struct LoadedNavmeshes(HashMap<(String, PGNavmeshType), Entity>);

/// Navmesh kinds loaded for every map. A map without water simply has no water file.
const NAVMESH_KINDS: [PGNavmeshType; 2] = [PGNavmeshType::Terrain, PGNavmeshType::Water];

/// Where PGNavmesh::filename saves them, as asset paths.
fn navmesh_path(map: &str, kind: PGNavmeshType) -> String {
    let suffix = match kind {
        PGNavmeshType::Terrain => "terrain",
        PGNavmeshType::Water => "water",
    };
    format!("game_map/{map}/navmesh/{map}_{suffix}.nav.json")
}

fn sync_navmeshes(
    mut commands: Commands,
    desired: Res<DesiredChunks>,
    mut loaded: ResMut<LoadedNavmeshes>,
    ass: Res<AssetServer>,
) {
    let active: HashSet<String> = desired.0.iter().map(|id| id.map.clone()).collect();

    loaded.0.retain(|(map, _), entity| {
        if active.contains(map) {
            true
        } else {
            commands.entity(*entity).despawn();
            false
        }
    });

    for map in active {
        for kind in NAVMESH_KINDS {
            let key = (map.clone(), kind);
            if loaded.0.contains_key(&key) {
                continue;
            }
            let handle: Handle<PGNavmesh> = ass.load(navmesh_path(&map, kind));
            let entity = commands
                .spawn((
                    Name::new(format!("Navmesh {map} ({kind:?})")),
                    MapNavmesh(map.clone()),
                    NavmeshPending { handle, kind },
                ))
                .id();
            loaded.0.insert(key, entity);
        }
    }
}

/// Every frame: moves a finished navmesh from the asset into the entity.
fn attach_navmeshes(
    mut commands: Commands,
    ass: Res<AssetServer>,
    navmeshes: Res<Assets<PGNavmesh>>,
    pending: Query<(Entity, &MapNavmesh, &NavmeshPending)>,
) {
    for (entity, map, pending) in &pending {
        if let Some(navmesh) = navmeshes.get(&pending.handle) {
            info!("{:?} navmesh for '{}' loaded", pending.kind, map.0);
            // Inserting PGNavmesh makes bevy_pg_nav tag it NavmeshTerrain / NavmeshWater.
            commands
                .entity(entity)
                .insert(navmesh.clone())
                .remove::<NavmeshPending>();
        } else if matches!(ass.load_state(&pending.handle), LoadState::Failed(_)) {
            match pending.kind {
                PGNavmeshType::Terrain => warn!("Terrain navmesh for '{}' failed to load", map.0),
                // Maps without water have no water navmesh: not a problem.
                PGNavmeshType::Water => info!("No water navmesh for '{}'", map.0),
            }
            commands.entity(entity).remove::<NavmeshPending>();
        }
    }
}


#[derive(Resource)]
pub struct ChunkStreaming {
    /// A chunk is loaded when the focus is within this distance of its rectangle (world units).
    pub load_radius: f32,
    /// A loaded chunk is only dropped beyond this. Keep it larger than load_radius so
    /// walking along a border doesn't spawn and despawn the same chunk repeatedly.
    pub unload_radius: f32,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ChunkId {
    pub map: String,
    pub chunk: String,
}

#[derive(Resource, Default)]
pub struct DesiredChunks(HashSet<ChunkId>);

/// Chunks that exist right now: id -> root entity.
#[derive(Resource, Default)]
pub struct LoadedChunks(HashMap<ChunkId, Entity>);

#[derive(Component)]
pub struct Chunk(pub ChunkId);

#[derive(Component)]
pub struct ChunkTerrain;

#[derive(Component)]
pub struct ChunkObject;

/// On a chunk root until its scene JSON has loaded and its objects are spawned.
#[derive(Component)]
struct ChunkPending(Handle<SceneData>);

/// One terrain material per map (they differ by splat texture).
#[derive(Resource, Default)]
struct ChunkMaterials(HashMap<String, Handle<TerrainMaterial>>);

#[derive(Default, TypePath)]
pub struct SerializedMeshLoader;

impl AssetLoader for SerializedMeshLoader {
    type Asset = Mesh;
    type Settings = ();
    type Error = Box<dyn std::error::Error + Send + Sync>;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &(),
        _load_context: &mut LoadContext<'_>,
    ) -> Result<Mesh, Self::Error> {
        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes).await?;
        let serialized: SerializedMesh = serde_json::from_slice(&bytes)?;
        let mesh = MeshDeserializer::new().deserialize(serialized);
        Ok(mesh)
    }

    fn extensions(&self) -> &[&str] {
        &["mesh.json"]
    }
}


/// The map's terrain material: its splat texture if it has one, else unpainted (which looks like
/// the old plain white material tinted by vertex colours).
fn map_material(
    meta: &MapMeta,
    cache: &mut ChunkMaterials,
    ass: &AssetServer,
    layers: &TerrainLayers,
    images: &mut Assets<Image>,
    materials: &mut Assets<TerrainMaterial>,
) -> Handle<TerrainMaterial> {
    cache
        .0
        .entry(meta.name.clone())
        .or_insert_with(|| {
            let mut load = |file: &Option<String>| match file {
                Some(file) => ass
                    .load_builder()
                    .with_settings(splat_loader_settings)
                    .load(format!("game_map/{}/terrain/{file}", meta.name)),
                None => images.add(new_splat_image(1, 1)),
            };
            let splats = [load(&meta.splat_file), load(&meta.splat_file_2)];
            materials.add(terrain_material(layers, splats))
        })
        .clone()
}

// ───────────────────────── locate ─────────────────────────

/// Minimum XZ corner of a chunk in world space.
/// ASSUMPTION: `location` is the min corner. It matches saved_loc and the first vertex (-12.5, -12.5)
/// of your example chunk. If it is the center, return `loc - size / 2.0` instead.
fn chunk_min_xz(chunk: &MapChunk, size: Vec2) -> Vec2 {
    Vec2::new(chunk.location[0], chunk.location[2]) - size / 2.0
}

fn distance_to_rect(point: Vec2, min: Vec2, size: Vec2) -> f32 {
    point.distance(point.clamp(min, min + size))
}

fn locate_chunks(
    focus: Single<&GlobalTransform, With<StreamingFocus>>,
    maps: Res<MapsData>,
    settings: Res<ChunkStreaming>,
    loaded: Res<LoadedChunks>,
    mut desired: ResMut<DesiredChunks>,
) {
    let pos = focus.translation().xz();
    let mut next: HashSet<ChunkId> = HashSet::new();

    for (map_name, meta) in &maps.data {
        let size = Vec2::new(meta.chunk_width, meta.chunk_height);
        let origin = Vec2::new(meta.origin[0], meta.origin[2]);

        for chunk in &meta.chunks {
            let id = ChunkId { map: map_name.clone(), chunk: chunk.name.clone() };
            let distance = distance_to_rect(pos, origin + chunk_min_xz(chunk, size), size);

            // Hysteresis: loaded chunks use the larger radius.
            let limit = if loaded.0.contains_key(&id) {
                settings.unload_radius
            } else {
                settings.load_radius
            };

            if distance <= limit {
                next.insert(id);
            }
        }
    }

    // Only write on change, so resource_changed::<DesiredChunks> is meaningful.
    if desired.0 != next {
        desired.0 = next;
    }
}

// ───────────────────────── redraw ─────────────────────────

fn redraw_chunks(
    mut commands: Commands,
    desired: Res<DesiredChunks>,
    mut loaded: ResMut<LoadedChunks>,
    maps: Res<MapsData>,
    ass: Res<AssetServer>,
    layers: Res<TerrainLayers>,
    mut cache: ResMut<ChunkMaterials>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<TerrainMaterial>>,
) {
    // Despawn chunks that are no longer wanted. Despawn is recursive and drops the handles,
    // so the mesh and scene assets unload with them.
    loaded.0.retain(|id, entity| {
        if desired.0.contains(id) {
            true
        } else {
            commands.entity(*entity).despawn();
            false
        }
    });

    // Spawn chunks that are wanted but missing.
    for id in &desired.0 {
        if loaded.0.contains_key(id) {
            continue;
        }
        let Some(meta) = maps.data.get(&id.map) else { continue };
        let Some(chunk) = meta.chunks.iter().find(|c| c.name == id.chunk) else { continue };

        let material = map_material(meta, &mut cache, &ass, &layers, &mut images, &mut materials);
        let root = spawn_chunk(&mut commands, &ass, material, meta, chunk, id);
        loaded.0.insert(id.clone(), root);
    }
}

fn spawn_chunk(
    commands: &mut Commands,
    ass: &AssetServer,
    material: Handle<TerrainMaterial>,
    meta: &MapMeta,
    chunk: &MapChunk,
    id: &ChunkId,
) -> Entity {
    // ASSUMPTION: chunk files live next to the meta file: game_map/{map}/{file}.
    let base = format!("game_map/{}", meta.name);
    let scene: Handle<SceneData> = ass.load(format!("{base}/scenes/{}", chunk.scene_file));

    // The root sits at the map's origin; the terrain and objects are placed relative to it
    // (chunk.location and the scene's saved_loc are plane-local).
    let mut root = commands.spawn((
        Name::new(format!("Chunk {}", chunk.name)),
        Chunk(id.clone()),
        ChunkPending(scene),
        Transform::from_translation(Vec3::from(meta.origin)),
        Visibility::default(),
    ));
    let terrain = (ChunkTerrain, MeshMaterial3d(material), Transform::from_translation(Vec3::from(chunk.location)));
    match &meta.heightmap {
        // The mesh is built from the map's heightmap once it's loaded (build_heightmap_chunks).
        Some(hm) => {
            // CPU-side only: the heights become a mesh, the image itself is never drawn.
            let cpu_only = |s: &mut ImageLoaderSettings| {
                s.is_srgb = false;
                s.asset_usage = RenderAssetUsages::MAIN_WORLD;
            };
            let heights = ass.load_builder().with_settings(cpu_only).load(format!("{base}/terrain/{}", hm.file));
            let colors = hm.colors_file.as_ref().map(|f| ass.load_builder().with_settings(cpu_only).load(format!("{base}/terrain/{f}")));
            root.with_child((
                terrain,
                ChunkHeightmapPending { map: meta.name.clone(), heights, colors, x: chunk.local_x_index, z: chunk.local_y_index },
            ));
        }
        None => {
            // DEPRECATED older export: one mesh file per chunk (SerializedMeshLoader). Kept so
            // those maps still load until they're re-exported.
            let mesh: Handle<Mesh> = ass.load(format!("{base}/terrain/{}", chunk.terrain_file));
            root.with_child((terrain, Mesh3d(mesh)));
        }
    }
    root.id()
}

/// On a chunk's terrain until its mesh is built from the map's heightmap.
#[derive(Component)]
struct ChunkHeightmapPending {
    map: String,
    heights: Handle<Image>,
    colors: Option<Handle<Image>>,
    x: u32,
    z: u32,
}

fn build_heightmap_chunks(
    mut commands: Commands,
    ass: Res<AssetServer>,
    maps: Res<MapsData>,
    images: Res<Assets<Image>>,
    mut meshes: ResMut<Assets<Mesh>>,
    pending: Query<(Entity, &ChunkHeightmapPending, &Transform)>,
) {
    for (entity, pending, transform) in &pending {
        let Some(hm) = maps.data.get(&pending.map).and_then(|m| m.heightmap.as_ref()) else {
            commands.entity(entity).remove::<ChunkHeightmapPending>();
            continue;
        };
        let failed = |h: &Handle<Image>| matches!(ass.load_state(h), LoadState::Failed(_));
        if failed(&pending.heights) {
            warn!("Heightmap of map '{}' failed to load", pending.map);
            commands.entity(entity).remove::<ChunkHeightmapPending>();
            continue;
        }
        let Some(image) = images.get(&pending.heights) else { continue }; // still loading
        let colors = match &pending.colors {
            Some(handle) if failed(handle) => None, // fall back to white
            Some(handle) => match images.get(handle) {
                Some(image) => decode_colors(image, hm),
                None => continue, // still loading
            },
            None => None,
        };
        let Some(heights) = decode_heights(image, hm) else {
            warn!("Heightmap of map '{}' has an unexpected format or size", pending.map);
            commands.entity(entity).remove::<ChunkHeightmapPending>();
            continue;
        };
        // Vertices relative to the chunk terrain's own position (its `location`).
        let mesh = chunk_mesh(hm, &heights, colors.as_deref(), pending.x, pending.z, transform.translation);
        commands.entity(entity).insert(Mesh3d(meshes.add(mesh))).remove::<ChunkHeightmapPending>();
    }
}

// ───────────────────────── objects ─────────────────────────

fn spawn_chunk_objects(
    mut commands: Commands,
    ass: Res<AssetServer>,
    scenes: Res<Assets<SceneData>>,
    pending: Query<(Entity, &ChunkPending)>,
) {
    for (root, pending) in &pending {
        let Some(scene) = scenes.get(&pending.0) else {
            if matches!(ass.load_state(&pending.0), LoadState::Failed(_)) {
                warn!("Chunk scene failed to load for {root}");
                commands.entity(root).remove::<ChunkPending>();
            }
            continue; // still loading
        };

        commands
            .entity(root)
            .with_children(|parent| {
                let origin = Vec3::from(scene.saved_loc);
                for objects in scene.objects.values() {
                    for obj in objects {
                        let transform = Transform {
                            translation: origin + Vec3::from(obj.location),
                            // ASSUMPTION: Euler XYZ radians. Match whatever your editor saves.
                            rotation: Quat::from_euler(
                                EulerRot::XYZ,
                                obj.rotation[0],
                                obj.rotation[1],
                                obj.rotation[2],
                            ),
                            scale: Vec3::from(obj.scale),
                        };

                        let mut entity = parent.spawn((
                            ChunkObject,
                            transform,
                            Visibility::default(),
                            obj.editor_asset.clone(),
                        ));

                        match &obj.editor_asset {
                            EditorAsset::Scene(name) => {
                                // PLACEHOLDER: use your existing model-spawning code.
                                // The path and the component name (SceneRoot in older Bevy,
                                // WorldAssetRoot in newer ones) depend on your setup.
                                entity.insert(WorldAssetRoot(ass.load(
                                    GltfAssetLabel::Scene(0).from_asset(format!("objects/{name}.glb")),
                                )));
                            }
                            _ => {
                                // TODO: Spawner / Marker / Water. Hand off to the code that
                                // builds them from obj.data in your current scene loader.
                            }
                        }
                    }
                }
            })
            .remove::<ChunkPending>();
    }
}

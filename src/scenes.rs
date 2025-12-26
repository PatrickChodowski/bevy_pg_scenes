
use bevy::prelude::*;
use bevy::mesh::Mesh;
use serde::{Serialize, Deserialize};
use bevy_pg_core::prelude::{GameStateTransition, GameState, AABB, Player, TerrainChunk, Tile};
use bevy::mesh::SerializedMesh;
use bevy::gltf::GltfLoaderSettings;
use bevy::asset::{LoadState, RenderAssetUsages};
use bevy_pg_nav::prelude::{PGNavmesh, GenerateNavMesh};
use bevy::platform::collections::{HashMap, HashSet};
use bevy::math::Vec3;
use bevy_common_assets::json::JsonAssetPlugin;

use crate::water::{WaterChunk, WaterData, water_bundle};
use crate::water::depth::render_to_depth;

#[derive(Resource)]
pub struct PGScenesSettings {
    pub map_resolution: usize,
    pub water_height: f32,
    pub markers_mapping: fn(name: String) -> Marker,
    pub spawners_mapping: fn(name: String, option: Option<String>) -> Spawner
}

pub struct PGScenesPlugin {
    pub map_resolution: usize,
    pub water_height:   f32,
    pub markers_mapping: fn(name: String) -> Marker,
    pub spawners_mapping: fn(name: String, option: Option<String>) -> Spawner
}

#[derive(Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct PGSerializedMesh {
    pub data: SerializedMesh
}

#[derive(Component)]
struct UpdateMesh {
    entity: Entity,
    handle: Handle<PGSerializedMesh>
}



impl Plugin for PGScenesPlugin {
    fn build(&self, app: &mut App) {
        app
        .add_plugins(JsonAssetPlugin::<PGSerializedMesh>::new(&["mesh.json"]))
        .insert_resource(PGScenesSettings{
            map_resolution: self.map_resolution,
            water_height: self.water_height,
            spawners_mapping: self.spawners_mapping,
            markers_mapping: self.markers_mapping
        })
        .insert_resource(CurrentChunk::new())
        .insert_resource(Scenes::new())
        .add_systems(Update, add_chunk_aabbs.run_if(resource_added::<MapsData>))
        .add_systems(OnExit(GameState::Play), clear_current_chunk)
        .add_systems(Update, (
            locate, 
            track_navmesh_load.run_if(any_with_component::<LoadingNavMeshHandle>)
        ).chain().in_set(ScenesSet::MapsPlayer))
        .add_systems(Update, (redraw.run_if(resource_changed::<CurrentChunk>)).chain().in_set(ScenesSet::Maps))
        .add_observer(load_asset)
        .add_systems(OnEnter(GameStateTransition::UnloadScene),   unload_scene)
        .add_systems(OnEnter(GameStateTransition::LoadSceneData), load_scene_data)
        .add_systems(OnEnter(GameStateTransition::LoadStatic), load_static)
        .add_systems(OnEnter(GameStateTransition::LoadRest), rest_loaded)
        .add_systems(Update, close_transition.run_if(in_state(GameStateTransition::Ready)))
        .add_systems(OnEnter(GameStateTransition::Done),  switch_state)
        .add_systems(Update, (
                loading_scene_from_save.run_if(resource_exists::<LoadSaveResources>),
            ).run_if(in_state(GameState::Transition))
        )
        .add_systems(Update, track_pg_meshes.run_if(any_with_component::<UpdateMesh>))
        ;
    }
}



fn load_static(
    mut commands:           Commands,
    ass:                    Res<AssetServer>,
    mut meshes:             ResMut<Assets<Mesh>>,
    currentchunk:           Res<CurrentChunk>,
    mapdata:                Res<MapsData>,
    scenes:                 Res<Scenes>,
    mapchunks:              Query<(Entity, &TerrainChunk)>,
    scenechunks:            Query<(Entity, &SceneChunk)>,
    waterchunks:            Query<(Entity, &WaterChunk)>,
    mut next_gst:           ResMut<NextState<GameStateTransition>>,
    water_data:             Res<WaterData>
){
    info!("[SCENES] [LOAD_STATIC]");
    for (entity, terrainchunk) in mapchunks.iter(){
        if currentchunk.to_despawn.contains(&terrainchunk.tile){
            info!("[Terrain] Despawning terrain {}: {}", terrainchunk.map_name, terrainchunk.chunk_id);
            commands.entity(entity).despawn();
        }
    }
    for (entity, scenechunk) in scenechunks.iter(){
        if currentchunk.to_despawn.contains(&scenechunk.tile){
            info!("[Terrain] Despawning scene element");
            commands.entity(entity).despawn();
        }
    }
    for (entity, waterchunk) in waterchunks.iter(){
        if currentchunk.to_despawn.contains(&waterchunk.tile){
            info!("[Terrain] Despawning water chunk");
            commands.entity(entity).despawn();
        }
    }

    for tile in currentchunk.to_spawn.iter(){
        let Some(chunk) = mapdata.get(tile) else {continue;};
        let sd = scenes.data.get(&chunk.chunk_id).unwrap();
        spawn_terrain_chunk(&mut commands, &ass, &chunk.map_name, tile, chunk, mapdata.chunk_size);
        let mut bundles = Vec::with_capacity(sd.objects.len());
        for (name, sods) in sd.objects.iter(){

            let asset_path = format!("objects/{}.glb", name);
            let mesh: Mesh3d = Mesh3d(ass.load(
                GltfAssetLabel::Primitive{primitive:0, mesh:0}.from_asset(asset_path.clone()),
            ));
            let material: MeshMaterial3d::<StandardMaterial> = MeshMaterial3d(ass.load(
                GltfAssetLabel::Material { index: 0, is_scale_inverted: false}.from_asset(asset_path)
            ));

            for sod in sods.iter(){
                bundles.push((
                    mesh.clone(), 
                    material.clone(),
                    sod.transform(), 
                    SceneChunk{tile: *tile}, 
                    name.clone(),
                    AssignComponents,
                    DespawnOnExit(GameState::Play)
                ));
            }
        }
        commands.spawn_batch(bundles);
        commands.spawn(water_bundle(&mut meshes, &tile, &chunk, water_data.material.clone(), mapdata.chunk_size));
        
    }
    next_gst.set(GameStateTransition::LoadRest);

}

fn switch_state(
    mut commands:    Commands,
    mut next_gs:     ResMut<NextState<GameState>>,
    transition:      Res<SceneTransition>
){
    info!(" [SCENES] Closing transition, setting game state to {:?}", transition.next_state);
    next_gs.set(transition.next_state);
    commands.remove_resource::<SceneTransition>();
}


fn unload_scene(
    mut current_chunk:  ResMut<CurrentChunk>,
    mut next_gst:       ResMut<NextState<GameStateTransition>>
){
    // Clear all the resources
    info!(" [SCENES] Transition: Unload Scene");
    current_chunk.reset();
    next_gst.set(GameStateTransition::LoadSceneData)
}


#[derive(Resource)]
pub struct LoadSaveResources {
    pub loaded_entities:        bool,
    pub loaded_resources:       bool,
    pub handle_save_entities:   Handle<DynamicScene>
}

impl LoadSaveResources {
    pub fn ready(&self) -> bool {
        self.loaded_entities & self.loaded_resources
    }
}


#[derive(Resource)]
pub struct LoadStartScene {
    pub player_ready: bool
}

impl LoadStartScene {
    pub fn ready(&self) -> bool {
        self.player_ready
    }
}


fn load_scene_data(
    mut commands:   Commands,
    ass:            Res<AssetServer>,
    transition:     Res<SceneTransition>
) {

    info!(" [SCENES] Transition Loading new scene {:?}", transition.set_scene);
    match transition.set_scene {
        SetScene::Start => {
            commands.insert_resource(
                LoadStartScene{player_ready: false}
            );
        }
        SetScene::Save(ref save_name) => {
            commands.insert_resource(
                LoadSaveResources {
                    loaded_entities: false,
                    loaded_resources: false,
                    handle_save_entities:  ass.load(format!("saves/{}.scn.ron", save_name))
            });
        }
    }
}

fn close_transition(
    mut next_gst:       ResMut<NextState<GameStateTransition>>,
    mut transition:     ResMut<SceneTransition>,
    time:               Res<Time>
) {
    transition.timer.tick(time.delta());
    if transition.timer.is_finished() {
        next_gst.set(GameStateTransition::Done);
    }
}

fn rest_loaded(
    mut next_gst: ResMut<NextState<GameStateTransition>>,
){
    next_gst.set(GameStateTransition::Ready);
}

#[derive(Resource)]
pub struct SceneTransition {
    timer:         Timer,
    next_state:    GameState,
    set_scene:     SetScene
}

impl SceneTransition {
    pub fn new(
        next_state: GameState,
        set_scene: SetScene, 
        delay: f32
    ) -> Self {

        let st = SceneTransition {
            timer: Timer::from_seconds(delay, TimerMode::Once),
            next_state,
            set_scene: set_scene.clone()
        };
        return st;
    }
}


#[derive(Resource, Debug, Clone, Serialize, Deserialize)]
pub enum SetScene  {
    Start,
    Save(String),
}


pub fn loading_scene_from_save(
    mut commands:       Commands, 
    ass:                Res<AssetServer>,
    mut scene_spawner:  ResMut<SceneSpawner>,
    mut lsr:            ResMut<LoadSaveResources>,
    mut next_gst:       ResMut<NextState<GameStateTransition>>
){

    info!(" [SCENES] LOADING SCENE FROM SAVE");
    if !lsr.loaded_entities {
        if let Some(scene_load_state) = ass.get_load_state(&lsr.handle_save_entities){
            lsr.loaded_entities = scene_load_state.is_loaded();
        }
    } 

    if lsr.ready() {
        info!(" [SCENES] Scene data loaded into memory");

        info!(" [SCENES] Populating entities");
        scene_spawner.spawn_dynamic(lsr.handle_save_entities.clone());

        info!(" [SCENES] Dropping Load Scene Resources");
        commands.remove_resource::<LoadSaveResources>();
        next_gst.set(GameStateTransition::LoadStatic);

    }
}



fn locate(
    mut commands:       Commands,
    player_transform:   Single<&Transform, (With<Player>, Changed<Transform>)>,
    maps:               Res<MapsData>,
    terrain_chunks:     Query<(Entity, &TerrainChunk)>,
    mut current_chunk:  ResMut<CurrentChunk>,
    ass:                Res<AssetServer>,
    scenes_settings:        Res<PGScenesSettings>
){

    for chunk in maps.chunks_data.iter(){
        if !chunk.aabb.has_point(player_transform.translation.xz()){
            continue;
        }

        if current_chunk.x == chunk.x && current_chunk.y == chunk.y {
            continue;
        }

        info!(" [LOCATE] Player should be on chunk {} map: {}", chunk.chunk_id, chunk.map_name);
        current_chunk.map_name = chunk.map_name.to_string();
        current_chunk.chunk_id = chunk.chunk_id.clone();
        current_chunk.x = chunk.x;
        current_chunk.y = chunk.y;
        current_chunk.get_visible(scenes_settings.map_resolution, maps.chunks_split as u8);

        current_chunk.reset_tiles(&terrain_chunks);

        let nav_filepaths = [
            format!("navmesh/{}_{}_terrain.navmesh.json", chunk.map_name, chunk.chunk_id),
            format!("navmesh/{}_{}_water.navmesh.json", chunk.map_name, chunk.chunk_id),
        ];

        for nav_filepath in nav_filepaths.iter(){
            info!("[SCENES] Spawning LoadingNavMeshHandle for {}", nav_filepath);
            commands.spawn(LoadingNavMeshHandle{
                name: format!("maps/{}_{}", chunk.map_name, chunk.chunk_id),
                handle: ass.load(nav_filepath),
                map_name: chunk.map_name.clone(),
                chunk_id: chunk.chunk_id.clone()
            });
        }
        
        break;
    }
}


// Load NavMesh into a resource after asset is loaded
fn track_navmesh_load(
    mut commands:    Commands,
    ass:             Res<AssetServer>,
    query:           Query<(Entity, &LoadingNavMeshHandle)>,
    ass_nav:         Res<Assets<PGNavmesh>>,
    mapsdata:        Res<MapsData>
){

    for (entity, loading_nav) in query.iter() {

        if let Some(load_state) = ass.get_load_state(&loading_nav.handle){
            if load_state.is_loaded() {
                let navmesh = ass_nav.get(&loading_nav.handle).unwrap().clone();
                commands.spawn(navmesh);
                commands.entity(entity).despawn();
            }

            if load_state.is_failed(){
                commands.write_message(GenerateNavMesh{
                    name: loading_nav.name.clone(),
                    map_name: loading_nav.map_name.clone(), 
                    chunk_id: loading_nav.chunk_id.clone(),
                    chunk_size: mapsdata.chunk_size
                });
                commands.entity(entity).despawn();
            }
        }
    }
}



#[derive(Component)]
struct  LoadingNavMeshHandle{
    name:           String,
    handle:         Handle<PGNavmesh>,
    chunk_id:       String,
    map_name:       String
}




#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub enum ScenesSet {
    MapsPlayer,
    Maps,
    Markers,
    Spawners
}


fn redraw(
    mut commands:           Commands,
    ass:                    Res<AssetServer>,
    mut meshes:             ResMut<Assets<Mesh>>,
    currentchunk:           Res<CurrentChunk>,
    mapdata:                Res<MapsData>,
    scenes:                 Res<Scenes>,
    terrainchunks:          Query<(Entity, &TerrainChunk)>,
    scenechunks:            Query<(Entity, &SceneChunk)>,
    waterchunks:            Query<(Entity, &WaterChunk)>,
    water_data:             Res<WaterData>,
    scenes_settings:        Res<PGScenesSettings>
){
    info!("[SCENES] REDRAW");

    for (entity, terrainchunk) in terrainchunks.iter(){
        if currentchunk.to_despawn.contains(&terrainchunk.tile){
            commands.entity(entity).despawn();
        }
    }
    for (entity, scenechunk) in scenechunks.iter(){
        if currentchunk.to_despawn.contains(&scenechunk.tile){
            commands.entity(entity).despawn();
        }
    }
    for (entity, waterchunk) in waterchunks.iter(){
        if currentchunk.to_despawn.contains(&waterchunk.tile){
            commands.entity(entity).despawn();
        }
    }

    for tile in currentchunk.to_spawn.iter(){

        let Some(chunk) = mapdata.get(tile) else {continue;};
        let sd = scenes.data.get(&chunk.chunk_id).unwrap();
        spawn_terrain_chunk(&mut commands, &ass, &chunk.map_name, tile, chunk, mapdata.chunk_size);
        let mut bundles = Vec::with_capacity(sd.objects.len());

        for (name, sods) in sd.objects.iter(){

            if !(name.contains("Spawner_") | name.contains("Marker_")){

                let asset_path = format!("objects/{}.glb", name);
                let mesh: Handle<Mesh> = ass.load(
                    GltfAssetLabel::Primitive{primitive:0, mesh:0}.from_asset(asset_path.clone()),
                );
                let material: Handle<StandardMaterial> = ass.load(
                    GltfAssetLabel::Material { index: 0, is_scale_inverted: false}.from_asset(asset_path.clone())
                );
                for sod in sods.iter(){
                    bundles.push((
                        Mesh3d(mesh.clone()), 
                        MeshMaterial3d(material.clone()), 
                        sod.transform(), 
                        SceneChunk{tile: *tile}, 
                        AssignComponents,
                        DespawnOnExit(GameState::Play),
                        name.clone(),
                        AssetSource::new_mm(asset_path.clone())
                    ));
                }
                
            } else if name.contains("Spawner_") {
                for sod in sods.iter(){
                    commands.spawn((
                        sod.transform(),
                        SceneChunk{tile: *tile}, 
                        (scenes_settings.spawners_mapping)(name.to_string(), sod.option.clone()),
                        DespawnOnExit(GameState::Play),
                        name.clone()
                    ));
                }
            } else if name.contains("Marker_") {
                for sod in sods.iter(){
                    commands.spawn((
                        sod.transform(),
                        SceneChunk{tile: *tile}, 
                        (scenes_settings.markers_mapping)(name.to_string()),
                        DespawnOnExit(GameState::Play),
                        name.clone()
                    ));
                }
            }
        }

        commands.spawn_batch(bundles);
        commands.spawn(water_bundle(&mut meshes, &tile, &chunk, water_data.material.clone(), mapdata.chunk_size));
        
    }
}






fn clear_current_chunk(
    mut currentchunk:  ResMut<CurrentChunk>
){
    currentchunk.reset();
}

fn add_chunk_aabbs(
    mut mapsdata: ResMut<MapsData>
){
    let chunk_size = mapsdata.chunk_size;
    for chunk in mapsdata.chunks_data.iter_mut(){
        chunk.aabb = AABB::from_loc_dims(Vec2::new(chunk.loc_x, chunk.loc_y), Vec2::splat(chunk_size));
    }
}




pub fn spawn_terrain_chunk(
    commands:         &mut Commands,
    ass:              &Res<AssetServer>,
    map_name:         &String,
    tile:             &Tile,
    chunk:            &Chunk,
    chunk_size:       f32     
){
    let base_name = format!("maps/{}_{}", map_name, chunk.chunk_id);
    let terrain_filepath = format!("{}.glb", base_name);

    let terrain_scene_mesh: Handle<Mesh> = ass.load_with_settings(
        GltfAssetLabel::Primitive{primitive:0, mesh:0}.from_asset(terrain_filepath.clone()),
        |settings: &mut GltfLoaderSettings| settings.load_meshes = RenderAssetUsages::all()
    );

    let terrain_scene_mat: Handle<StandardMaterial> = ass.load(
        GltfAssetLabel::Material { index: 0, is_scale_inverted: false}.from_asset(terrain_filepath)
    );

    let terrain_chunk_entity = commands.spawn((
        Mesh3d(terrain_scene_mesh),
        MeshMaterial3d(terrain_scene_mat),
        Transform::from_xyz(chunk.loc_x, 0.0, chunk.loc_y),
        TerrainChunk{
            loc:  Vec2::new(chunk.loc_x, chunk.loc_y),
            tile: *tile, 
            dims: Vec2::new(chunk_size, chunk_size),
            map_name: map_name.clone(),
            chunk_id: chunk.chunk_id.clone()
        }, 
        Name::from(base_name.clone()),
        Pickable::IGNORE,
        DespawnOnExit(GameState::Play),
        render_to_depth()
    )).id();

    let possible_path = format!("meshes/maps/{}_{}.mesh.json", map_name, chunk.chunk_id);
    let handle_serialized_mesh: Handle<PGSerializedMesh> = ass.load(possible_path);

    commands.spawn(UpdateMesh{entity: terrain_chunk_entity, handle: handle_serialized_mesh});
}

#[derive(Resource, Debug)]
pub struct Scenes {
    pub data: HashMap<String, SceneData>
}
impl Scenes {
    pub fn new() -> Self {
        Scenes{data: HashMap::default()}
    }
}

#[derive(Resource, Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct MapsData {
    pub number_of_chunks: usize,
    pub chunks_data:      Vec<Chunk>,
    pub plane_size:       f32,
    pub chunk_size:       f32,
    pub chunks_split:     usize
}

impl MapsData {
    pub fn get(&self, tile: &Tile) -> Option<&Chunk> {
        for chunk in self.chunks_data.iter(){
            if chunk.x == tile.x && chunk.y == tile.y {
                return Some(chunk);
            }
        }
        return None;
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct Chunk {
    pub map_name:   String,
    pub chunk_id:   String,
    pub loc_x:      f32,
    pub loc_y:      f32,
    pub x:          usize,
    pub y:          usize,
    #[serde(skip)]
    pub aabb:       AABB
}

#[derive(Resource, Reflect, Clone)]
#[reflect(Resource)]
pub struct CurrentChunk {
    pub x:                  usize,
    pub y:                  usize,
    pub visible:            Vec<Tile>,
    pub to_spawn:           HashSet<Tile>,
    pub to_despawn:         HashSet<Tile>,
    pub visited:            HashSet<Tile>,
    pub chunk_id:           String,
    pub map_name:           String
}

impl CurrentChunk {
    pub fn new() -> Self {
        CurrentChunk{
            x: 0, 
            y: 0, 
            to_spawn: HashSet::default(),
            to_despawn: HashSet::default(),
            visited: HashSet::default(),
            visible: vec![Tile::new(0, 0)],
            chunk_id: "000".into(), 
            map_name: "hedeby".into()
        }
    }

    pub fn reset(&mut self) {
        *self = CurrentChunk::new();
    }

    pub fn get_visible(
        &mut self, 
        map_resolution: usize,
        n_chunks: u8
    ){

        self.visible.clear();

        let mut start: i32 = -1;
        let mut end: i32 = 1;

        if map_resolution >= 1 && map_resolution % 2 == 1 {
            start = (map_resolution as i32 -1)/2 * -1;
            end = (map_resolution as i32 -1)/2;
        }

        for i in  start..=end {
            for j in start..=end {
                let new_x: i32 = self.x as i32 + i;
                let new_y: i32 = self.y as i32 + j;
                if new_x >= 0 && new_x < n_chunks as i32 && new_y >= 0 && new_y < n_chunks as i32{
                    self.visible.push(Tile::new(new_x as usize, new_y as usize));
                } 
            }
        }

    }

    pub fn reset_tiles(&mut self, mapchunks: &Query<(Entity, &TerrainChunk)>) {
        let mut new_map_tiles: HashSet<Tile> = HashSet::default();
        let mut old_map_tiles: HashSet<Tile> = HashSet::default();
        let mut to_despawn: HashSet<Tile> = HashSet::default();
        for tile in self.visible.iter(){
            new_map_tiles.insert(*tile);
        }
        for (_entity, terrain_chunk) in mapchunks.iter(){
            old_map_tiles.insert(terrain_chunk.tile);
            if !new_map_tiles.contains(&terrain_chunk.tile){
                to_despawn.insert(terrain_chunk.tile);
            }
        }
        self.to_spawn = new_map_tiles.difference(&old_map_tiles).cloned().collect::<HashSet<Tile>>();
        self.to_despawn = to_despawn;

        for tile in self.to_spawn.iter(){
            self.visited.insert(*tile);
        }
    }
}

#[derive(Resource, Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct SceneData {
    pub map_name: String,
    pub chunk_id: String,
    pub objects:  HashMap<Name, Vec<SceneObjectData>>
}


#[derive(Resource, Debug, Clone, Serialize, Deserialize, bevy::asset::Asset, bevy::reflect::TypePath)]
pub struct SceneObjectData {
    pub location:   Vec3,
    pub rotation:   Vec3,
    pub scale:      Vec3,
    pub option:     Option<String>  // Hold NPC Type etc.
} 
impl SceneObjectData {
    pub fn transform(&self) -> Transform {
        Transform::from_translation(self.location)
                  .with_rotation(Quat::from_euler(EulerRot::XYZ, self.rotation.x, self.rotation.y, self.rotation.z))
                  .with_scale(self.scale)
    }
}


#[derive(Component, Reflect)]
#[reflect(Component)]
pub struct SceneChunk{
    pub tile: Tile
}

#[derive(Component, Reflect)]
pub struct Spawnee;


#[derive(Component, Reflect, Clone)]
pub struct Spawner {
    pub id:     usize,
    pub option: Option<String>, 
    pub active: bool
}

impl Spawner {
    pub fn new(id: usize) -> Self {
        Spawner{id, active: true, option: None}
    }
    pub fn with_option(&mut self, option: String) -> Self {
        self.option = Some(option);
        self.clone()
    }
}


#[derive(Component, Reflect)]
pub struct Marker {
    pub id: usize,
    pub processed: bool
}
#[derive(Component, Reflect)]
pub struct Markee; // Result of Markers processing

impl Marker {
    pub fn new(id: usize) -> Self {
        Marker{id, processed: false}
    }
}


// All non moving/non spawnees entities: Buildings, Props
#[derive(Component, Reflect)]
pub struct Static;

#[derive(Deserialize, Serialize, Debug, Clone, Copy, Component)]
pub struct AssignComponents;

#[derive(Reflect, Debug)]
pub enum AssetSourceType{
    MeshMaterial,
    Scene
}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
pub struct AssetSource {
    pub path: String,
    pub typ:  AssetSourceType
}

impl AssetSource {
    pub fn new_mm(path: String) -> Self {
        AssetSource{path, typ: AssetSourceType::MeshMaterial}
    }
    pub fn new_scene(path: String) -> Self {
        AssetSource{path, typ: AssetSourceType::Scene}
    }
}


pub fn load_asset(
    trigger:        On<Add, AssetSource>,
    mut commands:   Commands,
    asset_sources:  Query<(Entity, &AssetSource)>,
    ass:            Res<AssetServer>,
    gst:            Option<Res<State<GameStateTransition>>>
){

    if gst.is_none(){
        return;
    }

    let Ok((entity, asset_source)) = asset_sources.get(trigger.entity) else {return;};
    match asset_source.typ {
        AssetSourceType::MeshMaterial => {
            let mesh: Handle<Mesh> = ass.load(
                GltfAssetLabel::Primitive{primitive:0, mesh:0}.from_asset(asset_source.path.clone()),
            );
            let material: Handle<StandardMaterial> = ass.load(
                GltfAssetLabel::Material { index: 0, is_scale_inverted: false}.from_asset(asset_source.path.clone())
            );
    
            commands.entity(entity).insert((
                Mesh3d(mesh),
                MeshMaterial3d(material)
            ));
        }
        AssetSourceType::Scene => {
            let scene_handle = ass.load(asset_source.path.clone());
            commands.entity(entity).insert(SceneRoot(scene_handle));
        }
    }
}


fn track_pg_meshes(
    mut commands:   Commands,
    ass:            Res<AssetServer>,
    pgmeshes:       Res<Assets<PGSerializedMesh>>,
    mut meshes:     ResMut<Assets<Mesh>>,
    query:          Query<(Entity, &UpdateMesh)>,
    terrain_chunks: Query<&Mesh3d, With<TerrainChunk>>,
){
    for (entity, update_mesh) in query.iter(){
        if let Some(load_state) = ass.get_load_state(&update_mesh.handle){
            match load_state {
                LoadState::Failed(_) => {commands.entity(entity).despawn();}
                LoadState::Loaded => {
                    if let Some(pgmesh) = pgmeshes.get(&update_mesh.handle){
                        let terrain_mesh: Mesh = pgmesh.data.clone().into_mesh();

                        if let Ok(current_mesh) = terrain_chunks.get(update_mesh.entity){
                            meshes.remove(&current_mesh.0);
                            // commands.entity(update_mesh.entity).remove::<Mesh3d>();
                            commands.entity(update_mesh.entity).insert(Mesh3d(meshes.add(terrain_mesh)));
                            commands.entity(entity).despawn();

                        } else {
                            commands.entity(entity).despawn();
                        }

                    } else {
                        commands.entity(entity).despawn();
                    }
                }
                _ => {}
            }
        } else {
            commands.entity(entity).despawn();
        }
    }

}
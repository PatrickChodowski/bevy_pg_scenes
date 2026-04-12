use bevy::prelude::*;
use bevy::platform::collections::HashSet;
use bevy_pg_core::prelude::{Player, Tile, TerrainChunk};

use crate::{maps::{SceneMap, MapSettings}, prelude::ScenesSet};



// Follows player entity and manages spawning and despawning chunks


pub struct PGSceneFollowerPlugin;

impl Plugin for PGSceneFollowerPlugin {
    fn build(&self, app: &mut App) {
        app
        .insert_resource(CurrentTile::new())
        .add_systems(Update, locate.in_set(ScenesSet::MapsPlayer))
        ;
    }
}



fn locate(
    player_transform:   Single<&Transform, (With<Player>, Changed<Transform>)>,
    scene_map:          Res<SceneMap>,
    // terrain_chunks:     Query<(Entity, &TerrainChunk)>,
    mut current_tile:   ResMut<CurrentTile>,
    map_settings:       Res<MapSettings>
){

    for (tile, entry) in scene_map.data.iter(){
        if !entry.aabb.has_point(player_transform.translation.xz()){
            continue;
        }

        if &current_tile.tile == tile {
            continue;
        }

        info!(" [LOCATE] Player should be on tile: {}", entry.name);
        current_tile.name = entry.name.clone();
        current_tile.tile = *tile;
        current_tile.get_visible(map_settings.render_tiles, scene_map.tiles_side);
        // current_chunk.reset_tiles(&terrain_chunks);

        break;
    }
}



#[derive(Resource, Reflect, Clone)]
#[reflect(Resource)]
pub struct CurrentTile {
    pub tile:               Tile,
    pub visible:            Vec<Tile>,
    pub to_spawn:           HashSet<Tile>,
    pub to_despawn:         HashSet<Tile>,
    pub visited:            HashSet<Tile>,
    pub name:               String
}

impl CurrentTile {
    pub fn new() -> Self {
        CurrentTile{
            tile: Tile::new(0, 0),
            to_spawn: HashSet::default(),
            to_despawn: HashSet::default(),
            visited: HashSet::default(),
            visible: vec![Tile::new(0, 0)],
            name: "test".to_string()
        }
    }

    pub fn reset(&mut self) {
        *self = CurrentTile::new();
    }

    pub fn get_visible(
        &mut self, 
        map_resolution: u8,
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
                let new_x: i32 = self.tile.x as i32 + i;
                let new_y: i32 = self.tile.y as i32 + j;
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

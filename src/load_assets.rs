use bevy::prelude::*;
use bevy::asset::LoadState;

use crate::types::{GameMap, MapMeta, MapsData};


#[derive(Resource)]
pub(crate) struct LoadMapTracker {
    handles: Vec<(String, Handle<MapMeta>)>,
    started: bool
}

impl Default for LoadMapTracker {
    fn default() -> Self {
        LoadMapTracker{
            handles: Vec::new(),
            started: false
        }
    }
}


pub(crate) fn collect_map_data(
    game_map:     Res<GameMap>,
    ass:          Res<AssetServer>,
    metas:        Res<Assets<MapMeta>>,
    mut commands: Commands,
    mut tracker:  Local<LoadMapTracker>
){

    info!("collect map data");
    if !tracker.started {
        tracker.started = true;
        for entry in game_map.data.iter() {
            let path = format!("game_map/{0}/{0}.meta.json", entry.name);
            tracker.handles.push((entry.name.clone(), ass.load(path)));
        }
    }

    let pending = tracker
        .handles
        .iter()
        .filter(|(_, h)| matches!(ass.load_state(h), LoadState::NotLoaded | LoadState::Loading))
        .count();

    if pending > 0 {
        return;
    }

    let mut maps_data = MapsData::default();
    for (name, handle) in tracker.handles.drain(..) {
        match metas.get(&handle) {
            Some(meta) => {
                maps_data.data.insert(name, meta.clone());
            }
            None => warn!("Map meta for '{name}' failed to load"),
        }
    }

    info!("Loaded {} map metas", maps_data.data.len());
    commands.insert_resource(maps_data);


}
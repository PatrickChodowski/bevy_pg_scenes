// Water: animated voronoi highlights, depth-based colour and opacity, and shoreline foam.
//
// Depth comes from the camera's depth prepass (add `DepthPrepass` to the camera): the distance
// along the view ray between the water surface and the ground behind it ("thickness"). Shallow
// water is clear and teal, deep water dark and nearly opaque, and foam forms where the
// thickness falls to zero: along every shore and around anything standing in the water, for any
// number of water planes and with no extra camera. Without a depth prepass the water is drawn
// as deep (opaque, no foam).
//
// The waves are in world space, so neighbouring water planes (one per terrain chunk) continue
// each other's pattern.
//
// Open sea: the vertex shader adds real (Gerstner) waves that grow with the distance to land
// (the `open_sea` texture, baked in open_sea.rs): flat along the coasts, rolling swell far out,
// with whitecaps on the crests. The water planes are subdivided for it.
//
// WESL (not plain WGSL): Bevy 0.20 only resolves imports and @if conditionals in .wesl files.

import bevy_pbr::render::{
    pbr_fragment::pbr_input_from_standard_material,
    mesh_view_bindings::globals,
    mesh_functions,
    view_transformations::{depth_ndc_to_view_z, position_world_to_clip},
};
@if(DEPTH_PREPASS)
import bevy_pbr::prepass::utils::prepass_depth;

@if(PREPASS_PIPELINE)
import bevy_pbr::{
    prepass::io::{VertexOutput, FragmentOutput},
    deferred::functions::deferred_output,
};
@else
import bevy_pbr::render::{
    forward_io::{Vertex, VertexOutput, FragmentOutput, decompress_vertex},
    pbr_functions::{apply_pbr_lighting, main_pass_post_lighting_processing},
};

// Kept for WaterMaterialExtension's layout; the old depth-camera foam no longer uses them.
@group(constants::MATERIAL_BIND_GROUP) @binding(100) var<uniform> scale: vec2<f32>;
@group(constants::MATERIAL_BIND_GROUP) @binding(101) var<uniform> water_height: f32;
@group(constants::MATERIAL_BIND_GROUP) @binding(102) var depth_texture: texture_depth_2d;
@group(constants::MATERIAL_BIND_GROUP) @binding(103) var depth_sampler: sampler_comparison;

// Boats near the camera (water/mod.rs, update_wakes): foam along each hull and a wake behind.
struct Wakes {
    count: u32,
    items: array<vec4<f32>, 16>, // world x, z, forward x, z
    sizes: array<vec4<f32>, 16>, // hull half length, half width, speed, -
    trail_count: u32,
    // Wake trails, oldest point first per boat: world x, z, age (0 new .. 1 gone), hull half width.
    trail: array<vec4<f32>, 128>,
    // Per trail point: strength (the boat's speed then, 0..1), 1 if it starts a boat's trail.
    trail_info: array<vec4<f32>, 128>,
}
@group(constants::MATERIAL_BIND_GROUP) @binding(104) var<uniform> wakes: Wakes;

// Open sea (water/mod.rs, SeaUniform).
struct Sea {
    rect: vec4<f32>,      // the texture's world rect: min x, min z, 1 / width, 1 / depth
    calm_distance: f32,   // distance from land where the waves start ...
    open_distance: f32,   // ... and where they're full size
    wave_scale: f32,      // from the wind's strength (water/mod.rs follow_wind)
    enabled: f32,         // 0 until the texture is baked
    // The two swells: direction (radians, where they roll to), weight. Normally only swell_a;
    // when the wind turns, the swell turns by cross-fading to swell_b (rotating the waves
    // themselves would slide distant waves across the sea).
    swell_a: vec4<f32>,
    swell_b: vec4<f32>,
    // The chunk streaming focus: x, z, load radius (0 = none), -. See edge_fade.
    focus: vec4<f32>,
}
@group(constants::MATERIAL_BIND_GROUP) @binding(105) var open_sea: texture_2d<f32>;
@group(constants::MATERIAL_BIND_GROUP) @binding(106) var open_sea_sampler: sampler;
@group(constants::MATERIAL_BIND_GROUP) @binding(107) var<uniform> sea: Sea;

/// World units per texture step (open_sea.rs DIST_UNIT, DEPTH_UNIT).
const DIST_UNIT: f32 = 4.0;
const DEPTH_UNIT: f32 = 0.02;
/// Water depth over which the waves grow from nothing to full size (they die down over shallow
/// banks and around low islands instead of washing over them).
const WAVE_MIN_DEPTH: f32 = 0.4;
const WAVE_FULL_DEPTH: f32 = 2.5;
/// Seabed depth (world units) where the water stops showing the bottom.
const BED_CLEAR_DEPTH: f32 = 0.8;
const BED_DEEP_DEPTH: f32 = 2.2;
const TAU: f32 = 6.28318530718;
const GRAVITY: f32 = 9.81;
/// Slows the waves down from real deep-water speed (it reads better from above).
const WAVE_TIME: f32 = 0.7;
/// Gerstner waves: direction (radians, relative to the swell's), wavelength (world units), steepness (0..1, their sum
/// must stay under 1 or the crests fold over), patchiness (0 = the same everywhere; else the
/// frequency of the noise that varies its strength, see wave_patch). A wave's height is
/// steepness / (2 pi / length).
/// All within ~35 degrees of one wind direction: waves from opposite sides would add up to a
/// pattern that bobs in place instead of a swell rolling across the sea. The long swell is the
/// same everywhere; the shorter waves come in patches ("short-crested", as at sea), or their long
/// straight crests line up into stripes in the distance.
const WAVES: array<vec4<f32>, 4> = array<vec4<f32>, 4>(
    vec4<f32>(0.0, 60.0, 0.16, 0.0),
    vec4<f32>(0.4, 31.0, 0.16, 0.0),
    vec4<f32>(-0.6, 17.0, 0.13, 0.025),
    vec4<f32>(0.6, 9.5, 0.10, 0.04),
);
/// The open-sea highlights' frame: fixed, as turning it would swim the pattern.
const HIGHLIGHT_DIR: f32 = 0.35;

/// Swell `s` (0 = a, 1 = b): its direction and weight.
fn swell(s: u32) -> vec2<f32> {
    return select(sea.swell_b.xy, sea.swell_a.xy, s == 0u);
}

/// How strong wave `w` (number `i`) of the swell rolling towards `dir` is at `xz`: 1, or for the
/// patchy ones 0.2..1.4 from noise drifting slowly with the swell.
/// Mirrored on the CPU (water/waves.rs, for boats riding the waves): keep the two the same.
fn wave_patch(w: vec4<f32>, i: u32, xz: vec2<f32>, dir: f32) -> f32 {
    if w.w <= 0.0 {
        return 1.0;
    }
    let wind = vec2(cos(dir), sin(dir));
    let p = (xz - wind * globals.time * 1.5) * w.w + vec2(f32(i) * 5.1, f32(i) * 1.7);
    return 0.2 + 1.2 * hash_noise(p);
}

/// 0..1 from integer cell coordinates (an integer hash: identical on the CPU, unlike the sin()
/// hash, whose large arguments the GPU computes imprecisely).
fn hash_cell(c: vec2<i32>) -> f32 {
    var h = (bitcast<u32>(c.x) * 0x8da6b343u) ^ (bitcast<u32>(c.y) * 0xd8163841u);
    h = (h ^ (h >> 16u)) * 0x7feb352du;
    h = (h ^ (h >> 15u)) * 0x846ca68bu;
    h = h ^ (h >> 16u);
    return f32(h >> 8u) / 16777215.0;
}

/// Smooth value noise in 0..1 on hash_cell.
fn hash_noise(p: vec2<f32>) -> f32 {
    let i = floor(p);
    let f = p - i;
    let u = f * f * (3.0 - 2.0 * f);
    let c = vec2<i32>(i);
    let a = hash_cell(c);
    let b = hash_cell(c + vec2(1, 0));
    let cc = hash_cell(c + vec2(0, 1));
    let d = hash_cell(c + vec2(1, 1));
    return mix(mix(a, b, u.x), mix(cc, d, u.x), u.y);
}
/// Open sea water is darker; crests let more light through (lighter, greener), troughs are darker.
/// Kept close to DEEP_COLOR, so open sea is the same water, just rougher.
const OPEN_SEA_COLOR: vec3<f32> = vec3<f32>(0.0, 0.025, 0.1);
const CREST_COLOR: vec3<f32> = vec3<f32>(0.02, 0.075, 0.17);
/// How far (voronoi cells per unit of normal tilt) the waves bend the highlights at sea.
const HIGHLIGHT_WARP: f32 = 2.5;
/// Sky reflected by the wave faces tilted away from the camera (a stylised fresnel: there's no
/// environment map for the PBR reflections to show).
const SKY_COLOR: vec3<f32> = vec3<f32>(0.42, 0.52, 0.62);
const SKY_REFLECTION: f32 = 0.4;
/// Direction the wave faces are shaded from (towards the sun, roughly).
const SEA_LIGHT: vec3<f32> = vec3<f32>(-0.45, 0.8, -0.4);

/// Distance to land and water depth (world units) at `xz` (only valid when sea.enabled).
fn sea_field(xz: vec2<f32>) -> vec2<f32> {
    let uv = (xz - sea.rect.xy) * sea.rect.zw;
    return textureSampleLevel(open_sea, open_sea_sampler, uv, 0.0).rg * 255.0 * vec2(DIST_UNIT, DEPTH_UNIT);
}

/// 0 at the coast and over shallows .. 1 out at open sea.
fn openness(xz: vec2<f32>) -> f32 {
    if sea.enabled < 0.5 {
        return 0.0;
    }
    let field = sea_field(xz);
    return smoothstep(sea.calm_distance, sea.open_distance, field.x) * smoothstep(WAVE_MIN_DEPTH, WAVE_FULL_DEPTH, field.y);
}

/// 1 .. 0 towards the edge of the loaded water (the streaming focus' load radius): how much the
/// waves may move the surface sideways. There's no next water plane past the edge, and the edge
/// pulling in would show a strip of sea floor. Mirrored in waves.rs.
fn edge_fade(xz: vec2<f32>) -> f32 {
    if sea.focus.z <= 0.0 {
        return 1.0;
    }
    let d = distance(xz, sea.focus.xy);
    return 1.0 - smoothstep(sea.focus.z - 45.0, sea.focus.z - 8.0, d);
}

/// Troughs flatten out above the seabed instead of cutting into it (and showing the sand): a
/// smooth limit at TROUGH_CLEARANCE above the bottom. Mirrored in waves.rs.
const TROUGH_CLEARANCE: f32 = 0.35;
fn limit_trough(y: f32, depth: f32) -> f32 {
    if y >= 0.0 {
        return y;
    }
    let room = max(depth - TROUGH_CLEARANCE, 0.05);
    return -room * tanh(-y / room);
}

struct SeaWave {
    offset: vec3<f32>,
    normal: vec3<f32>,
}

/// The waves at world point `xz` at `strength` (openness * wave scale): how far the surface
/// moves and its normal.
fn sea_wave(xz: vec2<f32>, strength: f32) -> SeaWave {
    var waves = WAVES;
    var offset = vec3(0.0);
    var tangent = vec3(1.0, 0.0, 0.0);
    var binormal = vec3(0.0, 0.0, 1.0);
    let t = globals.time * WAVE_TIME;
    for (var sw = 0u; sw < 2u; sw++) {
        let sv = swell(sw);
        if sv.y <= 0.0 {
            continue;
        }
        for (var i = 0u; i < 4u; i++) {
            let w = waves[i];
            let dir = sv.x + w.x;
            let d = vec2(cos(dir), sin(dir));
            let k = TAU / w.y;
            let f = k * (dot(d, xz) - sqrt(GRAVITY / k) * t);
            let s = w.z * strength * sv.y * wave_patch(w, i, xz, sv.x);
            let a = s / k;
            let sf = sin(f);
            let cf = cos(f);
            offset += vec3(d.x * a * cf, a * sf, d.y * a * cf);
            tangent += vec3(-d.x * d.x * s * sf, d.x * s * cf, -d.x * d.y * s * sf);
            binormal += vec3(-d.x * d.y * s * sf, d.y * s * cf, -d.y * d.y * s * sf);
        }
    }
    return SeaWave(offset, normalize(cross(binormal, tangent)));
}

/// The waves at a pixel, all in one pass over them (each wave's sin / cos and patch noise once):
/// the normal, and how high the waves are (-1 trough .. 1 the highest crest, whatever their
/// strength): both swells by weight, and each swell's own (x = a, y = b).
struct SeaSurface {
    normal: vec3<f32>,
    crest: f32,
    swell_crests: vec2<f32>,
}

/// `strength` as for sea_wave. `footprint`: world units a pixel covers here; waves much shorter
/// than that fade out of the normal instead of shimmering into stripes in the distance.
fn sea_surface(xz: vec2<f32>, strength: f32, footprint: f32) -> SeaSurface {
    var waves = WAVES;
    var tangent = vec3(1.0, 0.0, 0.0);
    var binormal = vec3(0.0, 0.0, 1.0);
    var crests = vec2(0.0);
    var crest = 0.0;
    var weights = 0.0;
    let t = globals.time * WAVE_TIME;
    for (var sw = 0u; sw < 2u; sw++) {
        let sv = swell(sw);
        if sv.y <= 0.0 {
            continue;
        }
        var h = 0.0;
        var total = 0.0;
        for (var i = 0u; i < 4u; i++) {
            let w = waves[i];
            let dir = sv.x + w.x;
            let d = vec2(cos(dir), sin(dir));
            let k = TAU / w.y;
            let f = k * (dot(d, xz) - sqrt(GRAVITY / k) * t);
            let amount = wave_patch(w, i, xz, sv.x);
            let sf = sin(f);
            let cf = cos(f);
            let fade = 1.0 - smoothstep(w.y * 0.12, w.y * 0.35, footprint);
            let s = w.z * strength * sv.y * amount * fade;
            tangent += vec3(-d.x * d.x * s * sf, d.x * s * cf, -d.x * d.y * s * sf);
            binormal += vec3(-d.x * d.y * s * sf, d.y * s * cf, -d.y * d.y * s * sf);
            let a = w.z / k;
            h += a * amount * sf;
            total += a;
        }
        let c = h / total;
        if sw == 0u { crests.x = c; } else { crests.y = c; }
        crest += c * sv.y;
        weights += sv.y;
    }
    return SeaSurface(normalize(cross(binormal, tangent)), crest / max(weights, 1e-4), crests);
}

@if(!PREPASS_PIPELINE)
@vertex
fn vertex(vertex_in: Vertex) -> VertexOutput {
    var out: VertexOutput;
    let vertex = decompress_vertex(vertex_in, vertex_in.instance_index);
    let world_from_local = mesh_functions::get_world_from_local(vertex_in.instance_index);
    var world = mesh_functions::mesh_position_local_to_world(world_from_local, vec4<f32>(vertex.position, 1.0));
    var normal = mesh_functions::mesh_normal_local_to_world(vertex.normal, vertex_in.instance_index);
    let strength = openness(world.xz) * sea.wave_scale;
    if strength > 0.0 {
        let wave = sea_wave(world.xz, strength);
        var offset = wave.offset;
        offset.y = limit_trough(offset.y, sea_field(world.xz).y);
        let sideways = edge_fade(world.xz);
        offset.x *= sideways;
        offset.z *= sideways;
        world = vec4(world.xyz + offset, 1.0);
        normal = wave.normal;
    }
    out.world_position = world;
    out.world_normal = normal;
    out.position = position_world_to_clip(world.xyz);
@if(VERTEX_UVS_A)
    out.uv = vertex.uv;
@if(VERTEX_UVS_B)
    out.uv_b = vertex.uv_b;
@if(VERTEX_TANGENTS)
    out.world_tangent = mesh_functions::mesh_tangent_local_to_world(world_from_local, vertex.tangent, vertex_in.instance_index);
@if(VERTEX_COLORS)
    out.color = vertex.color;
@if(VERTEX_OUTPUT_INSTANCE_INDEX)
    out.instance_index = vertex_in.instance_index;
@if(VISIBILITY_RANGE_DITHER)
    out.visibility_range_dither = mesh_functions::get_visibility_range_dither_level(vertex_in.instance_index, world_from_local[3]);
    return out;
}

/// The hull at the waterline, as a fraction of the boat's bounds (half width, half length).
const HULL_WIDTH: f32 = 0.38;
const HULL_LENGTH: f32 = 0.85;
/// Water within this distance (view ray) in front of something, inside a hull's outline, is
/// water on its deck (see on_deck).
const DECK_THICKNESS: f32 = 1.0;
/// Wake speed (units/s) at which the trail is at full strength.
const WAKE_FULL_SPEED: f32 = 12.0;

const NEG_HALF_PI: f32 = -1.57079632679;
const Y_SPEED: f32 = 0.4;
const X_SPEED: f32 = 0.3;
/// Wave cells per world unit.
const WAVE_FREQUENCY: f32 = 0.15;

const DEEP_COLOR: vec3<f32> = vec3<f32>(0.0, 0.03, 0.12);
const SHALLOW_COLOR: vec3<f32> = vec3<f32>(0.03, 0.17, 0.22);
const HIGHLIGHT_COLOR: vec3<f32> = vec3<f32>(0.62, 0.7, 0.8);
const FOAM_COLOR: vec3<f32> = vec3<f32>(0.75, 0.82, 0.86);
/// Thickness (world units along the view ray) at which water is fully deep-coloured / opaque.
const DEEP_THICKNESS: f32 = 3.0;
const SHALLOW_ALPHA: f32 = 0.6;
/// Over this last bit of thickness the water fades out completely, so the line where the flat
/// water cuts the terrain's triangles becomes a soft edge instead of a jagged one.
const SHORE_FADE: f32 = 0.3;
/// How strong the foam gets (0..1).
const FOAM_STRENGTH: f32 = 0.45;
const DEEP_ALPHA: f32 = 0.97;
/// Width of the foam band at the shore (world units of thickness).
const FOAM_WIDTH: f32 = 0.3;

fn rotate2d(theta: f32) -> mat2x2<f32> {
    let c = cos(theta);
    let s = sin(theta);
    return mat2x2<f32>(c, s, -s, c);
}

fn random2(p: vec2<f32>) -> vec2<f32> {
    return fract(sin(vec2<f32>(dot(p, vec2<f32>(127.1, 311.7)), dot(p, vec2<f32>(269.5, 183.3)))) * 43758.5453);
}

/// Smooth value noise in 0..1.
fn value_noise(p: vec2<f32>) -> f32 {
    let i = floor(p);
    let f = fract(p);
    let u = f * f * (3.0 - 2.0 * f);
    let a = random2(i).x;
    let b = random2(i + vec2(1.0, 0.0)).x;
    let c = random2(i + vec2(0.0, 1.0)).x;
    let d = random2(i + vec2(1.0, 1.0)).x;
    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

/// Foam patches: 0..1 coverage of noise `n` (0..1) at `density` 0..1 (0 = none, 1 = nearly all),
/// with soft edges.
fn patches(n: f32, density: f32) -> f32 {
    let threshold = 1.0 - density;
    return smoothstep(threshold, threshold + 0.25, n) * step(0.001, density);
}

fn voronoi(p: vec2<f32>, density: f32) -> f32 {
    let id = floor(p * density);
    var min_dist = 1.0;
    for (var dy: i32 = -1; dy <= 1; dy++) {
        for (var dx: i32 = -1; dx <= 1; dx++) {
            let neighbor = id + vec2<f32>(f32(dx), f32(dy));
            let point = neighbor + random2(neighbor);
            min_dist = min(min_dist, length(point - p * density));
        }
    }
    return min_dist;
}

@fragment
fn fragment(
    in: VertexOutput,
    @builtin(front_facing) is_front: bool,
) -> FragmentOutput {
    var pbr_input = pbr_input_from_standard_material(in, is_front);

    let t = globals.time;
    let offset = vec2<f32>(cos(t * 0.4) * X_SPEED, -t * Y_SPEED);

    // Water thickness behind this pixel (large without a depth prepass: deep water).
    var thickness = 1000.0;
@if(DEPTH_PREPASS)
    let ground_z = depth_ndc_to_view_z(prepass_depth(in.position, 0u));
@if(DEPTH_PREPASS)
    thickness = max(depth_ndc_to_view_z(in.position.z) - ground_z, 0.0);

    // How deep the water looks: from the thickness, but at least as deep as the seabed really is
    // (the baked depth, which doesn't move with the waves): every passing trough would otherwise
    // change the thickness enough to fade patches of sand in and out. Water over ~2 deep is
    // simply deep; only real shallows show the bottom.
    var bed_deep = 0.0;
    if sea.enabled > 0.5 {
        bed_deep = smoothstep(BED_CLEAR_DEPTH, BED_DEEP_DEPTH, sea_field(in.world_position.xz).y);
    }
    let deep = max(smoothstep(0.0, DEEP_THICKNESS, thickness), bed_deep);
    // Out at sea the waves carry the look: a per-pixel wave normal (lighting and the faces
    // shaded towards / away from the light), lighter crests and darker troughs.
    let xz = in.world_position.xz;
    let open = openness(xz);
    let strength = open * sea.wave_scale;
    let footprint = length(fwidth(xz)); // (derivatives need uniform control flow: out here)
    var crest = 0.0;
    var swell_crests = vec2(0.0);
    var face = 0.0;
    var n = vec3(0.0, 1.0, 0.0);
    var water_color = mix(SHALLOW_COLOR, DEEP_COLOR, deep);
    if open > 0.01 {
        let surface = sea_surface(xz, strength, footprint);
        n = surface.normal;
        pbr_input.N = n;
        pbr_input.world_normal = n;
        crest = surface.crest;
        swell_crests = surface.swell_crests;
        face = dot(n, normalize(SEA_LIGHT)) - dot(vec3(0.0, 1.0, 0.0), normalize(SEA_LIGHT));
        water_color = mix(water_color, OPEN_SEA_COLOR, open * 0.35);
        water_color = mix(water_color, CREST_COLOR, smoothstep(0.45, 1.0, crest) * open * 0.5);
        water_color *= 1.0 - 0.15 * open * smoothstep(0.0, -0.9, crest);
        water_color *= clamp(1.0 + face * 2.5 * open, 0.8, 1.5);
        let v = normalize(pbr_input.V);
        let grazing = 1.0 - max(dot(n, v), 0.0);
        let flat_grazing = 1.0 - max(v.y, 0.0);
        // Only what the waves add over a flat surface, and only brightening: faces turned towards
        // the camera would otherwise darken the (already dark) water to pure navy in the distance.
        // Fades with distance too, where it only adds glare.
        let far_fade = 1.0 - 0.6 * smoothstep(0.4, 1.5, footprint);
        let fresnel = max(pow(grazing, 3.0) - pow(flat_grazing, 3.0), 0.0) * far_fade;
        water_color += SKY_COLOR * fresnel * SKY_REFLECTION * open;
    }

    // Highlights: the voronoi light pattern. Along the coast as it always was. Out at sea the same
    // pattern, but carried by the swell: drifting with the wind, stretched along the crests, bent
    // by the waves (like light refracting through them) and lit by them (bright on the crests
    // and the faces towards the light, faint in the troughs). Two variants blended by openness,
    // each only where it shows (blending the coordinates instead would smear the pattern more
    // and more over time).
    var h = 0.0;
    if open < 0.99 {
        let p = xz * WAVE_FREQUENCY * rotate2d(NEG_HALF_PI);
        let calm = pow(clamp((voronoi(p + offset, 1.0) + voronoi(p + offset * 0.2, 2.0)) * 0.4, 0.0, 1.0), 4.5);
        h += calm * (1.0 - open);
    }
    if open > 0.01 {
        let d0 = vec2(cos(HIGHLIGHT_DIR), sin(HIGHLIGHT_DIR));
        // (along the crests, across them = with the wind), stretched along the crests.
        let p = vec2(dot(xz, vec2(-d0.y, d0.x)) * 0.85, dot(xz, d0) * 1.1) * WAVE_FREQUENCY + n.xz * HIGHLIGHT_WARP;
        let drift = vec2(cos(t * 0.4) * X_SPEED * 0.5, -t * Y_SPEED * 1.6);
        let rough = pow(clamp((voronoi(p + drift, 1.0) + voronoi(p + drift * 0.3, 2.0)) * 0.4, 0.0, 1.0), 4.5);
        let lit = clamp(0.4 + face * 3.0 + crest * 0.45, 0.08, 1.5);
        // In the distance the cells get squashed by the view into rows of blotches (a checker
        // board): fade them to their average instead.
        let far = smoothstep(0.35, 1.2, footprint);
        h += mix(rough * lit, 0.06, far * 0.8) * open;
    }
    var color = water_color + vec3(h) * HIGHLIGHT_COLOR;
    var alpha = mix(SHALLOW_ALPHA, DEEP_ALPHA, deep);

    // Foam: a band at the shore with a ragged, moving edge, plus a fainter second line offshore.
    // Smooth noise (not cells) for a ragged, drifting edge: streaks rather than honeycomb.
    let q = in.world_position.xz;
    let foam_noise = value_noise(q * 0.8 + offset * 1.5) * 0.6 + value_noise(q * 2.3 - offset) * 0.4;
    let edge = thickness + (foam_noise - 0.5) * FOAM_WIDTH;
    // Foam starts just off the waterline, so it doesn't trace the jagged cut either.
    var foam = (1.0 - smoothstep(0.0, FOAM_WIDTH, edge)) * smoothstep(0.02, 0.15, thickness) * FOAM_STRENGTH;
    // One fainter line rolling in just offshore, broken up by noise so it doesn't read as a contour.
    let wave_line = 0.5 + 0.5 * sin(thickness * 4.0 - t * 1.2);
    let wave_mask = smoothstep(0.45, 0.75, voronoi(in.world_position.xz * 0.35 - offset, 1.0));
    let near_shore = 1.0 - smoothstep(FOAM_WIDTH, FOAM_WIDTH * 2.0, edge);
    foam = max(foam, smoothstep(0.85, 1.0, wave_line) * near_shore * wave_mask * 0.4);
    // Boats. All boat foam is patchy: noise thresholded by a "density" that is highest at the
    // hull and falls off sideways and with age, so the foam thins out into flecks instead of
    // ending at a hard edge. The noise is fixed in the world (only drifting with the waves).
    let fleck = value_noise(q * 1.4 + offset * 0.5) * 0.6 + value_noise(q * 3.3 - offset) * 0.4;

    // Whitecaps, only out at sea and more the stronger the wind: streaks along the crests of
    // each swell's main wave (noise stretched along the crest line), breaking only in some areas
    // at a time (big slow-drifting noise).
    if open > 0.01 {
        let gusts = smoothstep(0.4, 0.75, value_noise(q * 0.018 + vec2(t * 0.02, -t * 0.013)));
        let rough = open * smoothstep(0.3, 1.2, sea.wave_scale) * 1.5;
        for (var sw = 0u; sw < 2u; sw++) {
            let sv = swell(sw);
            if sv.y <= 0.0 {
                continue;
            }
            let d0 = vec2(cos(sv.x), sin(sv.x));
            let along = dot(q, vec2(-d0.y, d0.x));
            let across = dot(q, d0);
            let streak = value_noise(vec2(along * 0.16, across * 0.9) + offset) * 0.55 + value_noise(q * 2.0 - offset) * 0.45;
            let caps = rough * gusts * smoothstep(0.5, 0.95, select(swell_crests.y, swell_crests.x, sw == 0u)) * 0.45 * min(sv.y, 1.0);
            foam = max(foam, patches(streak, caps) * 0.45);
        }
    }

    // Shore foam is turned off right around boats: their bow and stern rise through the surface,
    // which would otherwise make solid shore foam at the tips.
    var boat_total = 0.0;
    var near_boat = 0.0;
    // 0 where water inside a hull's waterline outline lies on the deck: when a crest runs up
    // higher than the boat (heading into the waves). Only there: what's right behind it is the
    // deck (thin water). Where the seabed is behind it instead, it's water seen under a hull
    // that's riding up on a wave, and must stay, or the sand shows through.
    var inside_hulls = 0.0;
    for (var i = 0u; i < min(wakes.count, 16u); i++) {
        let item = wakes.items[i];
        let size = wakes.sizes[i];
        let fwd = normalize(item.zw);
        let side = vec2(-fwd.y, fwd.x);
        let d = q - item.xy;
        let along = dot(d, fwd);
        let across = dot(d, side);
        let half_len = max(size.x, 0.5);
        // The hull at the waterline, from the mesh bounds: narrower (the bounds include the oars)
        // and shorter (the curled stem and stern posts overhang the water).
        let hull_wid = max(size.y * HULL_WIDTH, 0.3);
        let hull_len = half_len * HULL_LENGTH;
        let moving = clamp(size.z / WAKE_FULL_SPEED, 0.0, 1.0);

        // Along the hull: the distance (world units) from the hull's outline seen from above, a
        // pointed lens: half width hull_wid * (1 - u^2)^0.6 at u = along / hull_len. At rest a
        // little lapping foam; under way a little more along the front half (the bow wave).
        let u = clamp(along / hull_len, -1.0, 1.0);
        let k = max(1.0 - u * u, 1e-3);
        let w = hull_wid * pow(k, 0.6);
        // The outline's slope turns the sideways gap into a distance across the outline.
        let slope = min(hull_wid * 1.2 * abs(u) * pow(k, -0.4) / hull_len, 10.0);
        let side_dist = (abs(across) - w) / sqrt(1.0 + slope * slope);
        let tip_dist = length(vec2(abs(along) - hull_len, across));
        let hull_dist = select(tip_dist, min(side_dist, tip_dist), abs(along) < hull_len);
        let bow = smoothstep(-0.3, 0.6, along / hull_len) * (1.0 - smoothstep(0.85, 1.05, along / hull_len));
        let band = 0.3 + 0.4 * moving * bow;
        let off_hull = max(hull_dist, 0.0) / band;
        let hull_density = (0.45 + 0.25 * moving * bow) * smoothstep(-0.3, 0.0, hull_dist) * exp(-off_hull * off_hull);
        boat_total = max(boat_total, patches(fleck, hull_density) * 0.75);
        near_boat = max(near_boat, 1.0 - smoothstep(0.3, 2.0, hull_dist));
        inside_hulls = max(inside_hulls, 1.0 - smoothstep(-0.3, -0.1, hull_dist));
    }

    // Wakes: foam along the path each boat actually took (its trail points), so it stays where
    // the boat was when it turns. Churned water in the middle that widens and thins with age,
    // and a faint line at each edge spreading outward (the bow waves).
    for (var j = 1u; j < min(wakes.trail_count, 128u); j++) {
        if wakes.trail_info[j].y > 0.5 {
            continue; // first point of another boat's trail: no segment to the previous one
        }
        let a = wakes.trail[j - 1u];
        let b = wakes.trail[j];
        let ab = b.xy - a.xy;
        let t = clamp(dot(q - a.xy, ab) / max(dot(ab, ab), 1e-4), 0.0, 1.0);
        let dist = length(q - (a.xy + ab * t));
        let age = mix(a.z, b.z, t);
        let width = b.w * (0.8 + 2.5 * age);
        if dist > width * 1.6 + 2.0 {
            continue;
        }
        let strength = mix(wakes.trail_info[j - 1u].x, wakes.trail_info[j].x, t);
        let life = 1.0 - age;
        let x = dist / width;
        let core = 0.45 * strength * exp(-x * x * 2.0) * life * life;
        let edge = (dist - width * 1.3) / (0.3 + age * 1.2);
        let arm = 0.3 * strength * exp(-edge * edge) * life;
        boat_total = max(boat_total, max(patches(fleck, core), patches(fleck, arm) * 0.7) * 0.6);
    }
    foam = max(foam * (1.0 - near_boat), boat_total);

    color = mix(color, FOAM_COLOR, foam);
    alpha = max(alpha, foam);
    let on_deck = inside_hulls * (1.0 - smoothstep(DECK_THICKNESS, DECK_THICKNESS * 1.8, thickness));
    alpha *= smoothstep(0.0, SHORE_FADE, thickness) * (1.0 - on_deck);

    pbr_input.material.base_color = vec4<f32>(color, alpha);

@if(PREPASS_PIPELINE)
    let out = deferred_output(in, pbr_input);
@if(!PREPASS_PIPELINE)
    var out: FragmentOutput;
@if(!PREPASS_PIPELINE)
    out.color = apply_pbr_lighting(pbr_input);
@if(!PREPASS_PIPELINE)
    out.color = main_pass_post_lighting_processing(pbr_input, out.color);

    return out;
}

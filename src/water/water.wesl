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
// WESL (not plain WGSL): Bevy 0.20 only resolves imports and @if conditionals in .wesl files.

import bevy_pbr::render::{
    pbr_fragment::pbr_input_from_standard_material,
    mesh_view_bindings::globals,
    view_transformations::depth_ndc_to_view_z,
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
    forward_io::{VertexOutput, FragmentOutput},
    pbr_functions::{apply_pbr_lighting, main_pass_post_lighting_processing},
};

// Kept for WaterMaterialExtension's layout; the old depth-camera foam no longer uses them.
@group(constants::MATERIAL_BIND_GROUP) @binding(100) var<uniform> scale: vec2<f32>;
@group(constants::MATERIAL_BIND_GROUP) @binding(101) var<uniform> water_height: f32;
@group(constants::MATERIAL_BIND_GROUP) @binding(102) var depth_texture: texture_depth_2d;
@group(constants::MATERIAL_BIND_GROUP) @binding(103) var depth_sampler: sampler_comparison;

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

    // Waves.
    var p = in.world_position.xz * WAVE_FREQUENCY;
    p = p * rotate2d(NEG_HALF_PI);
    let t = globals.time;
    let offset = vec2<f32>(cos(t * 0.4) * X_SPEED, -t * Y_SPEED);
    var h = voronoi(p + offset, 1.0) + voronoi(p + offset * 0.2, 2.0);
    h = pow(clamp(h * 0.4, 0.0, 1.0), 4.5);

    // Water thickness behind this pixel (large without a depth prepass: deep water).
    var thickness = 1000.0;
@if(DEPTH_PREPASS)
    let ground_z = depth_ndc_to_view_z(prepass_depth(in.position, 0u));
@if(DEPTH_PREPASS)
    thickness = max(depth_ndc_to_view_z(in.position.z) - ground_z, 0.0);

    let deep = smoothstep(0.0, DEEP_THICKNESS, thickness);
    var color = mix(SHALLOW_COLOR, DEEP_COLOR, deep) + vec3(h) * HIGHLIGHT_COLOR;
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
    color = mix(color, FOAM_COLOR, foam);
    alpha = max(alpha, foam);
    alpha *= smoothstep(0.0, SHORE_FADE, thickness);

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

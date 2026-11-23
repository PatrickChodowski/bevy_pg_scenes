// Splat-map terrain: blends up to 8 tiling layers by the weights in two RGBA splat textures
// (layers 0..3 and 4..7).
// Unpainted (weights summing to 0) = the plain StandardMaterial look (white * vertex colour).
//
// Anti-tiling: "hex tiling" (stochastic texturing, after Heitz & Neyret 2018 / Mikkelsen 2022).
// Texture space is split into a triangle grid; each grid vertex gets a random offset (and
// rotation), and every pixel blends the 3 samples of its triangle's vertices. Repeats no longer
// line up, so the tile pattern disappears. A slow world-space brightness noise breaks up large
// uniform areas on top.
//
// WESL (not plain WGSL): Bevy 0.20 only resolves imports and @if conditionals in .wesl files.

import bevy_pbr::render::{
    pbr_fragment::pbr_input_from_standard_material,
    pbr_functions::alpha_discard,
};

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

// Per-layer values for 8 layers: layer i is [i / 4][i % 4].
struct TerrainParams {
    tile_size: array<vec4<f32>, 2>,
    roughness: array<vec4<f32>, 2>,
    normal_strength: array<vec4<f32>, 2>,
    anti_tiling: array<vec4<f32>, 2>,
    random_rotation: array<vec4<f32>, 2>,
    macro_variation: array<vec4<f32>, 2>,
}

const LAYERS: i32 = 8;

@group(constants::MATERIAL_BIND_GROUP) @binding(100) var<uniform> params: TerrainParams;
@group(constants::MATERIAL_BIND_GROUP) @binding(101) var splat_texture: texture_2d<f32>;
@group(constants::MATERIAL_BIND_GROUP) @binding(102) var splat_sampler: sampler;
@group(constants::MATERIAL_BIND_GROUP) @binding(103) var splat2_texture: texture_2d<f32>;
@group(constants::MATERIAL_BIND_GROUP) @binding(104) var splat2_sampler: sampler;
@group(constants::MATERIAL_BIND_GROUP) @binding(105) var albedo_array: texture_2d_array<f32>;
@group(constants::MATERIAL_BIND_GROUP) @binding(106) var albedo_sampler: sampler;
@group(constants::MATERIAL_BIND_GROUP) @binding(107) var normal_array: texture_2d_array<f32>;
@group(constants::MATERIAL_BIND_GROUP) @binding(108) var normal_sampler: sampler;

fn layer_param(v: array<vec4<f32>, 2>, i: i32) -> f32 {
    return v[i / 4][i % 4];
}

const TAU: f32 = 6.28318530718;
/// Higher = crisper transitions between hex cells (less blurring, more visible seams).
const HEX_CONTRAST: f32 = 2.2;
/// Hex cell size in texture repeats: bigger cells = fewer seams, smaller = less repetition.
const HEX_CELL: f32 = 0.6;
/// Detail: a second sample of each layer at this many times its frequency (non-integer, so
/// the two never line up), modulating brightness and bumps up close.
const DETAIL_FREQUENCY: f32 = 3.7;
const DETAIL_STRENGTH: f32 = 0.25;
/// Height blending: how far (in layer "height", taken from texture brightness) a transition
/// reaches. Small = crisp, texture-shaped edges between layers; large = soft fades.
const BLEND_DEPTH: f32 = 0.15;

/// PCG2D integer hash (Jarzynski & Olano 2020). Exact on every GPU, unlike fract(sin(x)),
/// which loses precision for large arguments and then shows up as hard seams.
fn pcg2d(v_in: vec2<u32>) -> vec2<u32> {
    var v = v_in * 1664525u + 1013904223u;
    v.x += v.y * 1664525u;
    v.y += v.x * 1664525u;
    v = v ^ (v >> vec2(16u));
    v.x += v.y * 1664525u;
    v.y += v.x * 1664525u;
    v = v ^ (v >> vec2(16u));
    return v;
}

/// Random vec2 in 0..1 for a lattice point (p should be integral, e.g. from floor()).
fn hash22(p: vec2<f32>) -> vec2<f32> {
    let h = pcg2d(bitcast<vec2<u32>>(vec2<i32>(floor(p))));
    return vec2<f32>(h & vec2(0xffffffu)) / f32(0xffffff);
}

fn value_noise(p: vec2<f32>) -> f32 {
    let i = floor(p);
    let f = fract(p);
    let u = f * f * (3.0 - 2.0 * f);
    let a = hash22(i).x;
    let b = hash22(i + vec2(1.0, 0.0)).x;
    let c = hash22(i + vec2(0.0, 1.0)).x;
    let d = hash22(i + vec2(1.0, 1.0)).x;
    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

struct TriangleGrid {
    weights: vec3<f32>,
    v1: vec2<f32>,
    v2: vec2<f32>,
    v3: vec2<f32>,
}

/// The triangle of the (skewed) triangle grid containing `uv`, and barycentric weights.
fn triangle_grid(uv: vec2<f32>) -> TriangleGrid {
    let skewed = mat2x2<f32>(1.0, 0.0, -0.57735027, 1.15470054) * (uv * 3.4641016);
    let base = floor(skewed);
    let f = fract(skewed);
    let z = 1.0 - f.x - f.y;
    var out: TriangleGrid;
    if z > 0.0 {
        out.weights = vec3(z, f.y, f.x);
        out.v1 = base;
        out.v2 = base + vec2(0.0, 1.0);
        out.v3 = base + vec2(1.0, 0.0);
    } else {
        out.weights = vec3(-z, 1.0 - f.y, 1.0 - f.x);
        out.v1 = base + vec2(1.0, 1.0);
        out.v2 = base + vec2(1.0, 0.0);
        out.v3 = base + vec2(0.0, 1.0);
    }
    return out;
}

struct LayerSample {
    albedo: vec3<f32>,
    normal: vec3<f32>,   // tangent space of the terrain frame (T = +X, B = -Z)
}

/// One sample of layer `i` with the texture coordinates rotated by `rot` and moved by `offset`.
/// The normal is rotated back so lighting stays consistent across cells.
fn sample_rotated(i: i32, uv: vec2<f32>, ddx: vec2<f32>, ddy: vec2<f32>, rot: mat2x2<f32>, offset: vec2<f32>) -> LayerSample {
    let p = rot * uv + offset;
    let gx = rot * ddx;
    let gy = rot * ddy;
    var s: LayerSample;
    s.albedo = textureSampleGrad(albedo_array, albedo_sampler, p, i, gx, gy).rgb;
    let raw = textureSampleGrad(normal_array, normal_sampler, p, i, gx, gy).xyz * 2.0 - 1.0;
    let n = vec3(raw.xy * layer_param(params.normal_strength, i), max(raw.z, 0.001));
    // Texture frame (u, up = -v) -> image frame (u, v) -> undo the rotation -> world X/Z -> (T, B).
    let back = transpose(rot) * vec2(n.x, -n.y);
    s.normal = normalize(vec3(back.x, -back.y, n.z));
    return s;
}

fn luminance(c: vec3<f32>) -> f32 {
    return dot(c, vec3(0.2126, 0.7152, 0.0722));
}

/// Fine detail of layer `i` at `uv`: brightness factor (around 1) and a tangent-space bump.
fn layer_detail(i: i32, uv: vec2<f32>, ddx: vec2<f32>, ddy: vec2<f32>) -> vec4<f32> {
    let rot = mat2x2<f32>(0.8, 0.6, -0.6, 0.8); // turned against the base sample
    let d = sample_rotated(i, uv * DETAIL_FREQUENCY, ddx * DETAIL_FREQUENCY, ddy * DETAIL_FREQUENCY, rot, vec2(0.37, 0.71));
    // The layer's average colour: its smallest mip.
    let average = textureSampleLevel(albedo_array, albedo_sampler, vec2(0.5), i, 16.0).rgb;
    let factor = luminance(d.albedo) / max(luminance(average), 0.02);
    return vec4(mix(1.0, factor, DETAIL_STRENGTH), d.normal * DETAIL_STRENGTH);
}

fn sample_layer(i: i32, uv: vec2<f32>, ddx: vec2<f32>, ddy: vec2<f32>) -> LayerSample {
    let identity = mat2x2<f32>(1.0, 0.0, 0.0, 1.0);
    if layer_param(params.anti_tiling, i) < 0.5 {
        return sample_rotated(i, uv, ddx, ddy, identity, vec2(0.0));
    }

    let grid = triangle_grid(uv / HEX_CELL * 0.288675);  // 0.2887 = 1/3.4641: cells of HEX_CELL repeats
    var w = pow(grid.weights, vec3(HEX_CONTRAST));
    w /= max(w.x + w.y + w.z, 1e-5);

    var out: LayerSample;
    out.albedo = vec3(0.0);
    out.normal = vec3(0.0);
    let vertices = array<vec2<f32>, 3>(grid.v1, grid.v2, grid.v3);
    for (var k = 0; k < 3; k++) {
        let r = hash22(vertices[k] + vec2(f32(i) * 1013.0, 7.0));
        let angle = r.x * TAU * layer_param(params.random_rotation, i);
        let c = cos(angle);
        let s = sin(angle);
        let rot = mat2x2<f32>(c, s, -s, c);
        let offset = hash22(vertices[k] + vec2(4099.0, f32(i) * 523.0 + 11.0));
        let smp = sample_rotated(i, uv, ddx, ddy, rot, offset);
        out.albedo += smp.albedo * w[k];
        out.normal += smp.normal * w[k];
    }
    out.normal = normalize(out.normal);
    return out;
}

@fragment
fn fragment(
    in: VertexOutput,
    @builtin(front_facing) is_front: bool,
) -> FragmentOutput {
    var pbr_input = pbr_input_from_standard_material(in, is_front);

    var wa = vec4(0.0);
    var wb = vec4(0.0);
@if(VERTEX_UVS_A)
    wa = clamp(textureSample(splat_texture, splat_sampler, in.uv), vec4(0.0), vec4(1.0));
@if(VERTEX_UVS_A)
    wb = clamp(textureSample(splat2_texture, splat2_sampler, in.uv), vec4(0.0), vec4(1.0));
    let w = array<f32, 8>(wa.x, wa.y, wa.z, wa.w, wb.x, wb.y, wb.z, wb.w);

    let painted = min(dot(wa, vec4(1.0)) + dot(wb, vec4(1.0)), 1.0);
    let unpainted = 1.0 - painted;

    // Layer textures tile in world space, so neighbouring chunks line up.
    let world_xz = in.world_position.xz;
    // Derivatives in uniform control flow; per layer they're scaled by its tile size.
    let world_ddx = dpdx(world_xz);
    let world_ddy = dpdy(world_xz);

    // Slow brightness variation over tens of metres.
    let macro_noise = value_noise(world_xz * 0.03) * 0.65 + value_noise(world_xz * 0.11 + 517.0) * 0.35;

    var albedo = vec3(unpainted);                 // the unpainted part stays white
    var normal_ts = vec3(0.0, 0.0, unpainted);    // ...and flat
    var roughness = pbr_input.material.perceptual_roughness * unpainted;

    // Sample the painted layers, then blend them by weight *and* height: where two layers
    // meet, the "higher" one (brighter texture) wins pixel by pixel, so transitions follow the
    // textures' own shapes (grass between stones) instead of the splat's soft gradient.
    var samples: array<LayerSample, 8>;
    var heights: array<f32, 8>;
    var top = -1.0;
    for (var i = 0; i < LAYERS; i++) {
        if w[i] <= 0.0005 {
            continue; // not painted here: skip its texture samples
        }
        let inv_tile = 1.0 / max(layer_param(params.tile_size, i), 0.001);
        let uv = world_xz * inv_tile;
        var layer = sample_layer(i, uv, world_ddx * inv_tile, world_ddy * inv_tile);
        let detail = layer_detail(i, uv, world_ddx * inv_tile, world_ddy * inv_tile);
        layer.albedo *= detail.x;
        layer.normal = normalize(layer.normal + vec3(detail.yz, 0.0));
        samples[i] = layer;
        // Height = brightness relative to the layer's own average (around 0.5), so dark and
        // bright layers get an equal say; plus the painted weight.
        let average = luminance(textureSampleLevel(albedo_array, albedo_sampler, vec2(0.5), i, 16.0).rgb);
        heights[i] = clamp(luminance(layer.albedo) / max(average, 0.02) * 0.5, 0.0, 1.0) + w[i];
        top = max(top, heights[i]);
    }
    var blend = array<f32, 8>(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
    var blend_sum = 0.0;
    for (var i = 0; i < LAYERS; i++) {
        if w[i] <= 0.0005 {
            continue;
        }
        blend[i] = max(heights[i] - (top - BLEND_DEPTH), 0.0);
        blend_sum += blend[i];
    }
    for (var i = 0; i < LAYERS; i++) {
        if blend[i] <= 0.0 {
            continue;
        }
        // Rescaled to the painted share, so the unpainted remainder keeps its weight.
        let k = blend[i] / max(blend_sum, 1e-5) * painted;
        let brightness = 1.0 + layer_param(params.macro_variation, i) * (macro_noise * 2.0 - 1.0);
        albedo += samples[i].albedo * brightness * k;
        normal_ts += samples[i].normal * k;
        roughness += layer_param(params.roughness, i) * k;
    }

    // base_color already holds material colour * vertex colour: keep that as a tint.
    let tint = pbr_input.material.base_color;
    pbr_input.material.base_color = vec4(tint.rgb * albedo, tint.a);
    pbr_input.material.perceptual_roughness = clamp(roughness, 0.089, 1.0);

    // World-aligned tangent frame (layer UVs run along world X / Z). OpenGL-style normal maps:
    // texture +V (green) points to world -Z.
    let N = normalize(pbr_input.world_normal);
    let T = normalize(vec3(1.0, 0.0, 0.0) - N * N.x);
    let B = cross(N, T);
    pbr_input.N = normalize(T * normal_ts.x + B * normal_ts.y + N * max(normal_ts.z, 0.001));

    pbr_input.material.base_color = alpha_discard(
        pbr_input.material.flags,
        pbr_input.material.alpha_cutoff,
        pbr_input.material.base_color,
    );

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

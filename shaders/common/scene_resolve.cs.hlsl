/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
// The surface of the level's pixels from their ids, once a pixel: what scene_light lights, and the word the
// screen-space passes read. A pass a class of material (RESOLVE_CLASS, SceneClass_ of an id), a group a tile of that
// class's list. What default_fetch of common\surface_forward.hlsli makes of a material's maps. Not here yet: a map
// bound outside the buckets
#include "common\fixed_cb.hlsli"
#include "common\common_samplers.hlsli"
#include "common\scene_tables.hlsli"
#include "common\scene_gbuffer.hlsli"
#include "common\thin_surface.hlsli"
#include "common\surface_light.hlsli"

#ifndef RESOLVE_CLASS
#define RESOLVE_CLASS 0
#endif

Texture2D<uint> g_ids : register(t0);
Texture2D<float> g_depth : register(t1);
StructuredBuffer<SceneCluster> g_clusters : register(t2);
StructuredBuffer<SceneTransform> g_transforms : register(t3);
StructuredBuffer<SceneInstance> g_instances : register(t4);
Buffer<uint> g_words : register(t5);
// The buckets as one array of the set where the compiler indexes resources (D3D12), else a register each
#ifdef __hlsl_dx_compiler
Texture2DArray g_buckets[SCENE_TEXTURE_BUCKETS] : register(t6);
#else
Texture2DArray g_bucket0 : register(t6);
Texture2DArray g_bucket1 : register(t7);
Texture2DArray g_bucket2 : register(t8);
Texture2DArray g_bucket3 : register(t9);
Texture2DArray g_bucket4 : register(t10);
Texture2DArray g_bucket5 : register(t11);
Texture2DArray g_bucket6 : register(t12);
Texture2DArray g_bucket7 : register(t13);
Texture2DArray g_bucket8 : register(t14);
Texture2DArray g_bucket9 : register(t15);
Texture2DArray g_bucket10 : register(t16);
Texture2DArray g_bucket11 : register(t17);
Texture2DArray g_bucket12 : register(t18);
Texture2DArray g_bucket13 : register(t19);
Texture2DArray g_bucket14 : register(t20);
Texture2DArray g_bucket15 : register(t21);
#endif
StructuredBuffer<SceneMaterial> g_materials : register(t22); // the level's, a head being the one loaded now or none
StructuredBuffer<SceneTreeLight> g_tree_lights : register(t23);
StructuredBuffer<uint> g_tiles : register(t24);
RWTexture2D<uint2> g_surface : register(u0);
RWTexture2D<uint> g_thin : register(u1);

#define SCENE_PAGE_WORDS g_words
#include "common\scene_pages.hlsli"
#define SCENE_ID_TRANSFORMS g_transforms
#include "common\scene_id.hlsli"

// The frame's decals lie on the level, not on its trees
#if defined(USE_SURFACE_DECALS) && RESOLVE_CLASS != 2
#define RESOLVE_DECALS
#define DECAL_NO_DERIVATIVES
#include "common\decals.hlsli"
#endif

// Rain wets what has a normal map, at high shading quality
#if defined(USE_SURFACE_HIGH) && defined(USE_SURFACE_SUN_SHADOWS)
#define RESOLVE_WETNESS
#define RAIN_RIPPLES
#define RAIN_TAPS 8
Texture2DArray<float> s_smap_sun : register(t50);
SamplerComparisonState smp_smap : register(s5);
#include "common\wetness.hlsli"

// The terrain with a height in its layers holds water where it is low
#if defined(USE_RAIN_PUDDLES) && RESOLVE_CLASS == 1
#define RESOLVE_PUDDLES
#define RAIN_PUDDLE_COMPUTE
#define RAIN_PUDDLE_FORWARD
#ifndef PI
#define PI 3.141592653589793f
#endif
#ifndef EPS
#define EPS 0.0000100f
#endif
#include "common\RainPuddles.hlsli"
#endif
#endif

// Snow lies on the level, not on its trees nor on what is cut out by its texture
#if defined(USE_DYNAMIC_SNOW) && RESOLVE_CLASS != 2
#define RESOLVE_SNOW
Texture2D g_snow_base : register(t58);
Texture2D g_snow_bump_x : register(t59);
#endif

// How much of each corner a pixel has, and how that changes a pixel across and a pixel down
struct resolve_weights {
    float3 share;
    float3 share_dx;
    float3 share_dy;
};

// The corners as the id pass projected them; at: the pixel's centre in clip space over w; size: the view in pixels.
// The shares are those the rasteriser interpolates by. Found without a division by a corner's w: a corner of a near
// triangle may stand behind the eye
resolve_weights resolve_triangle(float4 clip0, float4 clip1, float4 clip2, float2 at, float2 size)
{
    // The columns of the inverse of the corners' x, y and w, but for its determinant, which the shares do not need
    float3 edge0 = cross(clip1.xyw, clip2.xyw);
    float3 edge1 = cross(clip2.xyw, clip0.xyw);
    float3 edge2 = cross(clip0.xyw, clip1.xyw);
    float3 here = float3(dot(edge0.xy, at) + edge0.z, dot(edge1.xy, at) + edge1.z, dot(edge2.xy, at) + edge2.z);
    // A pixel across, and a pixel down: down the screen is up in clip space
    float3 across = here + float3(edge0.x, edge1.x, edge2.x) * (2.0f / size.x);
    float3 down = here - float3(edge0.y, edge1.y, edge2.y) * (2.0f / size.y);
    resolve_weights weights;
    weights.share = here / dot(here, 1.0f);
    weights.share_dx = across / dot(across, 1.0f) - weights.share;
    weights.share_dy = down / dot(down, 1.0f) - weights.share;
    return weights;
}

// A value of the corners at the pixel, or by the slopes a pixel across and a pixel down
#define RESOLVE_AT(name, shares) (name[0] * shares.x + name[1] * shares.y + name[2] * shares.z)

// returns a slice of the buckets: slot is bucket << 16 | slice, uv in the slice's own repeats
float4 resolve_slice(uint slot, float2 uv, float2 uv_dx, float2 uv_dy)
{
    float3 location = float3(uv, float(slot & 0xFFFFu));

#ifdef __hlsl_dx_compiler
    return g_buckets[NonUniformResourceIndex(slot >> 16)].SampleGrad(smp_base, location, uv_dx, uv_dy);
#else
    switch (slot >> 16) {
    case 1u:
        return g_bucket1.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 2u:
        return g_bucket2.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 3u:
        return g_bucket3.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 4u:
        return g_bucket4.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 5u:
        return g_bucket5.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 6u:
        return g_bucket6.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 7u:
        return g_bucket7.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 8u:
        return g_bucket8.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 9u:
        return g_bucket9.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 10u:
        return g_bucket10.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 11u:
        return g_bucket11.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 12u:
        return g_bucket12.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 13u:
        return g_bucket13.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 14u:
        return g_bucket14.SampleGrad(smp_base, location, uv_dx, uv_dy);
    case 15u:
        return g_bucket15.SampleGrad(smp_base, location, uv_dx, uv_dy);
    default:
        return g_bucket0.SampleGrad(smp_base, location, uv_dx, uv_dy);
    }
#endif
}

// returns a map of a material: its head where that is loaded, else its tail (sample_map of scene_buckets.hlsli)
float4 resolve_map(uint tail, uint head, uint info, float2 uv, float2 uv_dx, float2 uv_dy)
{
    // One over two to the power of each repeat shift, made as the bits of the float
    float2 scale = asfloat((127u - uint2(info & 15u, (info >> 4) & 15u)) << 23u);
    return resolve_slice(head != SCENE_TEXTURE_ABSENT ? head : tail, uv * scale, uv_dx * scale, uv_dy * scale);
}

#define RESOLVE_MAP(material_idx, name, uv, uv_dx, uv_dy) resolve_map(g_materials[material_idx].name, g_materials[material_idx].name##_head, g_materials[material_idx].name##_info, uv, uv_dx, uv_dy)

float4 resolve_layer(uint material_idx, uint slot, float2 uv, float2 uv_dx, float2 uv_dy)
{
    uint4 map = g_materials[material_idx].terrain[slot];
    return resolve_map(map.x, map.y, map.z, uv, uv_dx, uv_dy);
}

// Parallax where the texture asks for it, at high shading quality: the class of its own, whose march holds more at
// once than anything else here, so that the warps of the other classes do not keep registers for it. What is cut out
// by its texture is left alone: the id pass cuts it where the surface is, not where the eye's ray meets its height
#if RESOLVE_CLASS == 3 && defined(ALLOW_STEEPPARALLAX) && defined(USE_SURFACE_HIGH)
#define RESOLVE_PARALLAX
#ifndef PLANE_CORRECTION_VAL
#define PLANE_CORRECTION_VAL 1e-3f
#endif

// surface_shift_uv of common\surface_forward.hlsli: where the eye's ray meets a height map. height_map: the tail,
// head and info of the map with the height in channel; ray: towards the eye in the surface's tangent frame; depth:
// the pixel's distance along the view
float2 resolve_shift_uv(uint3 height_map, uint channel, bool is_pbr, float2 uv, float2 uv_dx, float2 uv_dy, float3 ray, float depth)
{
    const float start_fade = 8.0f;
    const float stop_fade = 12.0f;
    const float stop_offset = 16.0f;

    [branch] if (height_map.x == SCENE_TEXTURE_ABSENT || depth >= stop_offset) {
        return uv;
    }

#ifdef PARALLAX_HEIGHT
    float scale = PARALLAX_HEIGHT;
#else
    float scale = is_pbr ? 0.02f : 0.01f;
#endif
    float height = resolve_map(height_map.x, height_map.y, height_map.z, uv, uv_dx, uv_dy)[channel];
    float2 reach = ray.xy * scale;
#ifdef USE_PARALLAX_PLANE_CORRECTION
    reach *= rcp(max(PLANE_CORRECTION_VAL, abs(ray.z)));
#endif
    float2 shifted = is_pbr ? uv - reach * (1.0f - height) : uv + reach * (height - 0.5f);

    [branch] if (depth < stop_fade) {
        float layer_depth = rcp(lerp(20.0f, 8.0f, ray.z * ray.z));
        float2 stride = reach * layer_depth;
        float2 march_uv = uv;
        float surface = 1.0f - height;
        float layer = is_pbr ? 0.0f : 0.5f;
        // The march reads one slice at the pixel's own level, worked out once
        float2 repeat = asfloat((127u - uint2(height_map.z & 15u, (height_map.z >> 4) & 15u)) << 23u);
        float2 axes = float2(dot(uv_dx * repeat, uv_dx * repeat), dot(uv_dy * repeat, uv_dy * repeat));
        float grain = sqrt(max(min(axes.x, axes.y), max(axes.x, axes.y) * (1.0f / 256.0f)));
        uint slice = height_map.y != SCENE_TEXTURE_ABSENT && grain * SCENE_TEXTURE_TAIL < 1.0f ? height_map.y : height_map.x;

        [loop] while (layer < surface) {
            layer += layer_depth;
            march_uv -= stride;
            surface = 1.0f - resolve_slice(slice, march_uv * repeat, float2(grain, 0.0f), float2(0.0f, grain))[channel];
        }

        float2 before = march_uv + stride;
        float after = surface - layer;
        surface = 1.0f - resolve_map(height_map.x, height_map.y, height_map.z, before, uv_dx, uv_dy)[channel];
        float ahead = surface - layer + layer_depth;
        march_uv = lerp(march_uv, before, after * rcp(after - ahead));
        shifted = lerp(shifted, march_uv, smoothstep(stop_fade, start_fade, depth));
    }

    return lerp(shifted, uv, smoothstep(stop_fade, stop_offset, depth));
}
#endif

[numthreads(SCENE_SHADE_TILE, SCENE_SHADE_TILE, 1)]
void main(uint3 group_id : SV_GroupID, uint3 inside : SV_GroupThreadID)
{
    uint tile = g_tiles[RESOLVE_CLASS * SCENE_SHADE_TILES + group_id.x];
    uint2 pixel = uint2(tile & 0xFFFFu, tile >> 16u) * SCENE_SHADE_TILE + inside.xy;
    // All that is read by the pixel alone, at once: read one after another each waits for the one before it
    uint id = g_ids[pixel];
    uint thin_id = (g_thin[pixel] >> 28u) & 3u;
    float seen_depth = g_depth[pixel];

    // The surfaces are cleared a frame: what holds no triangle of the class keeps what it has. A model or a blade of
    // grass in front has named itself in the thin target: nothing of the triangle is read
    if (id == 0u || (id & SCENE_ID_CLASS_MASK) != RESOLVE_CLASS || thin_id == THIN_ID_MOVING) {
        return;
    }

    uint cluster_idx = (id >> SCENE_ID_CLUSTER_SHIFT) - 1u;
    uint triangle_idx = (id >> SCENE_ID_TRIANGLE_SHIFT) & SCENE_ID_TRIANGLE_MASK;
    SceneCluster cluster = g_clusters[cluster_idx];
    float2 size = scaled_screen_res.xy;
    uint3 corners;
    float3 place[3];
    float4 clip[3];

    [unroll] for (uint corner_idx = 0u; corner_idx < 3u; corner_idx++) {
        corners[corner_idx] = scene_page_corner(cluster.index_start, triangle_idx * 3u + corner_idx);
        place[corner_idx] = scene_id_position(cluster, corners[corner_idx]);
        clip[corner_idx] = mul(m_VP, float4(place[corner_idx], 1.0f));
        clip[corner_idx].xy += m_taa_jitter.xy * clip[corner_idx].w;
    }

    float2 at = (float2(pixel) + 0.5f) / size * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f);
    resolve_weights weights = resolve_triangle(clip[0], clip[1], clip[2], at, size);

    // Something drawn after the id pass stands in front: a model, the HUD, the grass. A model has kept its own surface.
    // The rasteriser snaps the corners: where the triangle is seen edge-on its depth differs from this one by a part of
    // how much it changes a pixel across, far more than the depth's own precision
    float3 clip_z = float3(clip[0].z, clip[1].z, clip[2].z);
    float3 clip_w = float3(clip[0].w, clip[1].w, clip[2].w);
    float3 across = weights.share + weights.share_dx;
    float3 down = weights.share + weights.share_dy;
    float depth = dot(weights.share, clip_z) / dot(weights.share, clip_w);
    float slope = abs(dot(across, clip_z) / dot(across, clip_w) - depth) + abs(dot(down, clip_z) / dot(down, clip_w) - depth);

    if (seen_depth < depth - 2e-6f - 0.5f * slope) {
        return;
    }

    // All that is taken from the triangle, before the maps are read: those reads wait long, and a warp keeps registers
    // for the most its shader ever holds at once. Past this point the corners, their places and the shares are gone
    float2 uvs[3];
    float3 normals[3];
    float hemis[3];

    [unroll] for (uint vertex_idx = 0u; vertex_idx < 3u; vertex_idx++) {
        uvs[vertex_idx] = scene_page_uv(cluster.vertices, corners[vertex_idx]);
        normals[vertex_idx] = scene_page_normal(cluster.vertices, corners[vertex_idx]);
        hemis[vertex_idx] = scene_page_hemi(cluster.vertices, corners[vertex_idx]);
    }

    float2 uv = RESOLVE_AT(uvs, weights.share);
    float2 uv_dx = RESOLVE_AT(uvs, weights.share_dx);
    float2 uv_dy = RESOLVE_AT(uvs, weights.share_dy);
    float3 normal = RESOLVE_AT(normals, weights.share);
    float hemi = RESOLVE_AT(hemis, weights.share);
    // How much the normal turns across the pixel, as surface_roughness_spread takes it: a turn of the whole triangle
    // does not change it
    float3 normal_dx = RESOLVE_AT(normals, weights.share_dx);
    float3 normal_dy = RESOLVE_AT(normals, weights.share_dy);
    float turned = 0.25f * (dot(normal_dx, normal_dx) + dot(normal_dy, normal_dy));
    uint transform = (cluster.triangles >> SCENE_CLUSTER_TRANSFORM_SHIFT) & SCENE_CLUSTER_TRANSFORM_MASK;
    bool is_cut = (cluster.triangles & SCENE_CLUSTER_ALPHA) != 0u;

    [branch] if (transform != 0u) {
        float3x3 turn = (float3x3)g_transforms[transform].world;
        normal = mul(turn, normal);
    }

    normal = normalize(normal);
    // The frame of the normal map from the triangle itself, by how its UV runs across its corners' places: the
    // tangents of the three vertices, decoded and weighed, were a tenth of what a pixel ran. Flat over a triangle,
    // set square to the pixel's own normal
    float3 edge1 = place[1] - place[0];
    float3 edge2 = place[2] - place[0];
    float2 span1 = uvs[1] - uvs[0];
    float2 span2 = uvs[2] - uvs[0];
    float side = span1.x * span2.y - span2.x * span1.y < 0.0f ? -1.0f : 1.0f;
    float3 tangent = (edge1 * span2.y - edge2 * span1.y) * side;
    tangent -= normal * dot(normal, tangent);
    tangent *= rsqrt(max(dot(tangent, tangent), 1e-20f));
    float handed = dot(cross(normal, tangent), (edge2 * span1.x - edge1 * span2.x) * side) < 0.0f ? -1.0f : 1.0f;
    float3 drawn_at = RESOLVE_AT(place, weights.share);
    uint material_idx = g_instances[cluster.instance].material;
    uint kind = g_materials[material_idx].flags;
    float detail_scale = g_materials[material_idx].detail_scale;
    bool is_pbr = (kind & SCENE_MATERIAL_PBR) != 0u;
    // The flags do not tell what has a normal map: the maps the record holds do
    bool has_bump = g_materials[material_idx].bump != SCENE_TEXTURE_ABSENT && g_materials[material_idx].bump_x != SCENE_TEXTURE_ABSENT;
#if RESOLVE_CLASS != 1
    // The detail texture fades out with the distance: calc_detail of common\r1_static.hlsli
    float fade = distance(drawn_at, eye_position) * dt_params.w;
    fade = min(fade * fade, 1.0f);
#endif
#ifdef RESOLVE_PARALLAX
    [branch] if (has_bump && !is_cut) {
        float3 eye_ray = eye_position - drawn_at;
        float3 ray = normalize(float3(dot(tangent, eye_ray), dot(handed * cross(normal, tangent), eye_ray), dot(normal, eye_ray)));
        uint3 heights = is_pbr ? uint3(g_materials[material_idx].bump, g_materials[material_idx].bump_head, g_materials[material_idx].bump_info)
            : uint3(g_materials[material_idx].bump_x, g_materials[material_idx].bump_x_head, g_materials[material_idx].bump_x_info);
        // The detail texture follows the base texture; the lightmap stays where the surface is
        uv = resolve_shift_uv(heights, is_pbr ? 0u : 3u, is_pbr, uv, uv_dx, uv_dy, ray, mul(m_V, float4(drawn_at, 1.0f)).z);
    }
#endif
#if RESOLVE_CLASS == 0 || RESOLVE_CLASS == 3
    bool has_lightmap = scene_page_kind(cluster.vertices) == ScenePageBaked_Lightmap && g_materials[material_idx].hemi != SCENE_TEXTURE_ABSENT;
    float2 light_uv = 0.0f;
    float2 light_dx = 0.0f;
    float2 light_dy = 0.0f;

    [branch] if (has_lightmap) {
        float2 lightmaps[3];

        [unroll] for (uint baked_idx = 0u; baked_idx < 3u; baked_idx++) {
            lightmaps[baked_idx] = scene_page_lightmap(scene_page_baked(cluster.vertices, corners[baked_idx]));
        }

        light_uv = RESOLVE_AT(lightmaps, weights.share);
        light_dx = RESOLVE_AT(lightmaps, weights.share_dx);
        light_dy = RESOLVE_AT(lightmaps, weights.share_dy);
    }
#endif
#ifdef RESOLVE_SNOW
    float2 snow_dx = RESOLVE_AT(place, weights.share_dx).xz * 0.2f;
    float2 snow_dy = RESOLVE_AT(place, weights.share_dy).xz * 0.2f;
#endif
#ifdef RESOLVE_DECALS
    float3x3 to_view = (float3x3)m_V;
    float3 decals = decal_tint(mul(m_V, float4(drawn_at, 1.0f)), mul(to_view, RESOLVE_AT(place, weights.share_dx)), mul(to_view, RESOLVE_AT(place, weights.share_dy)), normalize(mul(to_view, normal)), pixel);
#endif
    float4 base = RESOLVE_MAP(material_idx, base, uv, uv_dx, uv_dy);
    float3 detail = 0.5f;
    float4 bump = 0.0f;
    float4 bump_x = 0.0f;
    // The detail texture's own normal and material maps, faded out with its colour
    float detail_fade = 0.0f;
    float4 detail_bump = 0.0f;
    float4 detail_bump_x = 0.0f;
    float detail_gloss = 0.0f;
    scene_surface surface;
    surface.material_class = SceneClass_Plain;
    surface.flags = 0u;
#if RESOLVE_CLASS == 1
    // Terrain has its share of the sky in the alpha of its base texture, and its maps in four layers a mask weighs.
    // Its blender tells the record nothing of the layers: the maps the record holds do
    bool has_layers = g_materials[material_idx].terrain[SCENE_TERRAIN_COLOUR].x != SCENE_TEXTURE_ABSENT;
    has_bump = has_layers ? g_materials[material_idx].terrain[SCENE_TERRAIN_BUMP].x != SCENE_TEXTURE_ABSENT : has_bump;
    hemi = base.a;
    surface.material_class = SceneClass_Terrain;

    [branch] if (has_layers) {
        float4 layers = resolve_layer(material_idx, 0u, uv, uv_dx, uv_dy);
        layers = layers / dot(layers, 1.0f);
        detail = 0.0f;

        [unroll] for (uint layer_idx = 0u; layer_idx < 4u; layer_idx++) {
            [branch] if (layers[layer_idx] > 0.0f) {
                detail += resolve_layer(material_idx, SCENE_TERRAIN_COLOUR + layer_idx, uv * detail_scale, uv_dx * detail_scale, uv_dy * detail_scale).rgb * layers[layer_idx];

                [branch] if (has_bump) {
                    bump += resolve_layer(material_idx, SCENE_TERRAIN_BUMP + layer_idx, uv * detail_scale, uv_dx * detail_scale, uv_dy * detail_scale) * layers[layer_idx];
                    bump_x += resolve_layer(material_idx, SCENE_TERRAIN_BUMP_X + layer_idx, uv * detail_scale, uv_dx * detail_scale, uv_dy * detail_scale) * layers[layer_idx];
                }
            }
        }
    } else {
        [branch] if (g_materials[material_idx].detail != SCENE_TEXTURE_ABSENT) {
            detail = RESOLVE_MAP(material_idx, detail, uv * detail_scale, uv_dx * detail_scale, uv_dy * detail_scale).rgb;
        }

        [branch] if (has_bump) {
            bump = RESOLVE_MAP(material_idx, bump, uv, uv_dx, uv_dy);
            bump_x = RESOLVE_MAP(material_idx, bump_x, uv, uv_dx, uv_dy);
        }
    }

#else
    // Past where it has faded out the detail texture is the grey it fades to: not read
    [branch] if (fade < 1.0f && g_materials[material_idx].detail != SCENE_TEXTURE_ABSENT) {
        float4 detail_map = RESOLVE_MAP(material_idx, detail, uv * detail_scale, uv_dx * detail_scale, uv_dy * detail_scale);
        detail = detail_map.rgb * (1.0f - fade) + 0.5f * fade;
#ifdef USE_SURFACE_HIGH
        [branch] if (has_bump && fade < 1.0f && (kind & SCENE_MATERIAL_DETAIL_BUMP) && g_materials[material_idx].detail_bump != SCENE_TEXTURE_ABSENT && g_materials[material_idx].detail_bump_x != SCENE_TEXTURE_ABSENT) {
            detail_fade = 1.0f - fade;
            detail_bump = RESOLVE_MAP(material_idx, detail_bump, uv * detail_scale, uv_dx * detail_scale, uv_dy * detail_scale);
            detail_bump_x = RESOLVE_MAP(material_idx, detail_bump_x, uv * detail_scale, uv_dx * detail_scale, uv_dy * detail_scale);
            detail_gloss = detail_map.a;
        }
#endif
    }

    [branch] if (has_bump) {
        bump = RESOLVE_MAP(material_idx, bump, uv, uv_dx, uv_dy);
        bump_x = RESOLVE_MAP(material_idx, bump_x, uv, uv_dx, uv_dy);
    }
#if RESOLVE_CLASS == 2
    // A tree's baked share of the sky is scaled and offset by its place, and weighed by how much it looks up
    SceneTreeLight tree_light = g_tree_lights[transform];
    // A tree's share of the sky as the deferred frame had it: a leaf's is held under what its side of the crown
    // can see, not scaled by it, which took a quarter off every leaf that looks sideways and half off one that looks down
    bool is_leaf = (kind & (SCENE_MATERIAL_PLACED | SCENE_MATERIAL_STILL)) == SCENE_MATERIAL_PLACED && is_cut;
    hemi = saturate((tree_light.scale.w * hemi + tree_light.bias.w) * 0.5f);
    hemi = is_leaf ? min(hemi, 0.5f + 0.25f * normal.y) : hemi;
    surface.material_class = is_leaf ? SceneClass_Foliage : SceneClass_Plain;
#else
    [branch] if (has_lightmap) {
        float4 hemi_map = RESOLVE_MAP(material_idx, hemi, light_uv, light_dx, light_dy);
#ifdef USE_SOC_LIGHTING
        hemi = dot(hemi_map.rgb, 1.0f / 3.0f);
#else
        hemi = hemi_map.a;
#endif
    }
#endif
#endif
    // Where the pixel is, by its depth: the triangle's own place for it was let go before the reads
    float3 position = scene_surface_position(pixel, seen_depth);
    float3 to_eye = normalize(eye_position - position);
    float view_depth = mul(m_V, float4(position, 1.0f)).z;
    float3 bitangent = handed * cross(normal, tangent);
    surface.albedo = base.rgb * detail;
    surface.normal = normal;
    surface.metalness = 0.0f;
    surface.roughness = 0.0f;
    surface.scatter = 0.0f;
    surface.flags |= SceneSurface_Gloss;
    // What the screen-space passes take the surface for: surface_kind of common\surface_forward.hlsli
    float old_kind = frac((g_materials[material_idx].gloss + 0.5f) * 0.25f);
    old_kind = max(1.0f - 4.0f * old_kind, (old_kind - 0.25f) / 0.75f);
    float thin_roughness = has_bump ? old_kind * old_kind * (3.0f - 2.0f * old_kind) * 0.25f + 0.15f : 1.0f;
    float sky_reach = hemi;
    float3 thin_normal = normal;

    float occlusion = 1.0f;

    [branch] if (has_bump) {
        float3 mapped;

        if (is_pbr) {
            mapped = float3(bump.wy * 2.0f - 1.0f, 0.0f);
            mapped.z = sqrt(1.0f - saturate(dot(mapped.xy, mapped.xy)));
            float2 detail_xy = detail_bump.wy * 2.0f - 1.0f;
            float3 detail_shift = float3(detail_xy.x, -detail_xy.y, sqrt(saturate(1.0f - dot(detail_xy, detail_xy)))) * detail_fade;
            bump_x = saturate(bump_x * lerp((float4)1.0f, detail_bump_x * float4(2.0f, 2.0f, 1.0f, 1.0f), detail_fade));
            // A PBR normal map has its green the other way up
            mapped.y = -mapped.y;
            mapped += detail_shift;
            surface.normal = normalize(tangent * mapped.x + bitangent * mapped.y + normal * mapped.z);
            surface.metalness = bump_x.x;
#ifdef USE_SURFACE_HIGH
            surface.scatter = bump_x.z;
#endif
            surface.flags &= ~SceneSurface_Gloss;
            occlusion = bump_x.w;
            // The roughness widened by how much the surface turns across the pixel. Its normal map is not read at the
            // neighbours for it: two reads a pixel were a quarter of the pass
            surface.roughness = surface_roughness_spread(turned, surface.normal, to_eye, max(0.02f, bump_x.y));
            thin_roughness = surface.roughness;
        } else {
            // An old normal map has half the height, which doubles its relief
            mapped = bump.wzy + bump_x.xyz - 1.0f + (detail_bump.wzy + detail_bump_x.xyz - 1.0f) * detail_fade;
            mapped.z = abs(mapped.z * 0.5f);
            surface.normal = normalize(tangent * mapped.x + bitangent * mapped.y + normal * mapped.z);
            surface.roughness = bump.x * bump.x * lerp(1.0f, detail_gloss * 2.0f, detail_fade);
        }

        // The sky lights what looks up more than what looks down, a third of the way, and the occlusion map dims it
        hemi *= lerp(1.0f, (0.5f + 0.5f * surface.normal.y) / max(0.5f + 0.5f * normal.y, 0.25f), 0.3f) * occlusion;
        thin_normal = surface.normal;
    }

#ifdef RESOLVE_SNOW
    // Where the level has snow (rain_params.w), what faces up under the open sky takes its colour, and with PBR maps
    // its roughness; the surface keeps its own relief
    [branch] if (rain_params.w > 0.0f && !is_cut) {
        float snow = smoothstep(0.2f, 0.3f, saturate(sky_reach)) * smoothstep(0.7f, 0.8f, surface.normal.y) * saturate(rain_params.w);

        [branch] if (snow > 0.0f) {
            float2 snow_uv = position.xz * 0.2f;
            surface.albedo = lerp(base.rgb, g_snow_base.SampleGrad(smp_base, snow_uv, snow_dx, snow_dy).rgb, snow) * lerp(detail, 0.5f, snow);

            if (surface.flags & SceneSurface_Gloss) {
                // Snow scatters, it does not shine
                surface.roughness *= 1.0f - snow;
            } else {
#ifdef SNOW_PBR
                float snow_roughness = max(0.02f, g_snow_bump_x.SampleGrad(smp_base, snow_uv, snow_dx, snow_dy).y);
#else
                float snow_roughness = 0.6f;
#endif
                surface.roughness = lerp(surface.roughness, snow_roughness, snow);
                surface.metalness *= 1.0f - snow;
                thin_roughness = surface.roughness;
            }
        }
    }
#endif
#ifdef RESOLVE_WETNESS
    // What the rain reaches is wet: darker unless it is a metal, smoother where it mirrors, with the rings and
    // streaks of the rain in what it mirrors
    [branch] if (rain_setup.w > 0.0f) {
        float rain_reached;
        float wet = rain_exposure(position, normal, saturate(sky_reach), view_depth, float2(pixel) + 0.5f, rain_reached);
        float puddle = 0.0f;
        thin_normal = normalize(surface.normal + rain_ripples(position, surface.normal) * wet);
#ifdef RESOLVE_PUDDLES
        // Water fills the terrain from its low places up as it stays wet, as far as the rain reaches: smooth, dark,
        // with its own ripples, and no wet ground under it. The layers' weighed heights are in bump.x
        [branch] if (is_pbr && has_layers) {
            float3 puddle_normal;
            float2 puddle_refraction;
            puddle = GetRainPuddle(position.xz * rain_level.zw - rain_level.xy, mul((float3x3)m_V, normal), mul(m_V, float4(position, 1.0f)), saturate(bump.x), true, puddle_normal, puddle_refraction);
            puddle *= rain_reached * step(1e-4f, wet);
            thin_normal = normalize(lerp(thin_normal, mul(puddle_normal, (float3x3)m_V), puddle));
            surface.albedo *= lerp(1.0f, 0.3f * occlusion, puddle);
            wet *= 1.0f - puddle;
        }
#endif

        if (surface.flags & SceneSurface_Gloss) {
            // The old maps get wet as their own frame wet them: darker by how bright the surface is, by a half at
            // most, and glossier by the same. The albedo here is half of it
            float soak = wet * dot(surface.albedo, 0.66f);
            surface.albedo *= max(1.0f - sqrt(soak), 0.5f);
            // The gloss is kept squared: that frame added 0.8 of the soak to the gloss itself
            float gloss = saturate(sqrt(surface.roughness) + 0.8f * soak);
            surface.roughness = gloss * gloss;
        } else {
            surface.albedo *= lerp(1.0f, lerp(0.66f, 1.0f, surface.metalness), wet);
        }

        thin_roughness = lerp(lerp(thin_roughness, min(0.2f, thin_roughness), wet), 0.03f, puddle);
    }
#endif

#ifdef RESOLVE_DECALS
    surface.albedo *= decals;
#endif
    // The share of the sky has six bits in the surface and four in the thin target: a pattern settles them
    float pattern = frac(52.9829189f * frac(dot(float2(pixel) + 0.5f, float2(0.06711056f, 0.00583715f))));
    surface.hemi = hemi;
    g_surface[pixel] = scene_surface_pack(surface, pattern - 0.5f);
    g_thin[pixel] = thin_pack(normalize(mul((float3x3)m_V, thin_normal)), sky_reach + (pattern - 0.5f) / 15.0f, thin_roughness, 0u, has_bump, false, pattern);
}

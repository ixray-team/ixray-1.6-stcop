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
// The grass's pixels from their ids (the tiles of list SCENE_SHADE_GRASS): the blade and the triangle an id names
// are put together again as the id pass drew them, and the pixel keeps the surface the grass's depth pass kept in a
// frame drawn forward. The thin target names it as moving, so the sway the id pass wrote stays its velocity
#include "common\fixed_cb.hlsli"
#include "common\common_samplers.hlsli"
#include "common\scene_tables.hlsli"
#include "common\scene_gbuffer.hlsli"
#include "common\thin_surface.hlsli"
#include "common\vertex.hlsli"

Texture2D<uint> g_ids : register(t0);
StructuredBuffer<SceneCluster> g_clusters : register(t2);
Buffer<uint> g_words : register(t5);
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
StructuredBuffer<uint> g_tiles : register(t24);
StructuredBuffer<SceneGrassItem> g_forward_items : register(t25);
Buffer<uint> g_forward_grass : register(t26); // the frame's forward list, see SCENE_GRASS_LIST
StructuredBuffer<SceneGrassBin> g_grass_bins : register(t27);
StructuredBuffer<SceneGrassShade> g_grass_shade : register(t28);
RWTexture2D<uint2> g_surface : register(u0);
RWTexture2D<uint> g_thin : register(u1);

// The heads are in the materials already: the buckets are read as they are
#define SCENE_BUCKETS_TAILS
#define smp_scene smp_base
#include "common\scene_buckets.hlsli"

// The sway of the grass as the forward frame's vertex stage has it (r1_static.hlsli)
float calc_cyclic(float x)
{
    float f = 1.4142f * sin(x * 3.14159f);
    return f * f - 1.0f;
}

float2 calc_xz_wave(float2 direction, float rigidity)
{
    return direction * rigidity;
}

#define GRASS_PULL_ONLY
#include "common\scene_forward_grass.hlsli"

// The shares of the corners at the pixel, and a pixel across and down (resolve_triangle of scene_resolve.cs.hlsl)
void grass_shares(float4 clip0, float4 clip1, float4 clip2, float2 at, float2 size, out float3 share, out float3 share_dx, out float3 share_dy)
{
    float3 edge0 = cross(clip1.xyw, clip2.xyw);
    float3 edge1 = cross(clip2.xyw, clip0.xyw);
    float3 edge2 = cross(clip0.xyw, clip1.xyw);
    float3 here = float3(dot(edge0.xy, at) + edge0.z, dot(edge1.xy, at) + edge1.z, dot(edge2.xy, at) + edge2.z);
    float3 across = here + float3(edge0.x, edge1.x, edge2.x) * (2.0f / size.x);
    float3 down = here - float3(edge0.y, edge1.y, edge2.y) * (2.0f / size.y);
    share = here / dot(here, 1.0f);
    share_dx = across / dot(across, 1.0f) - share;
    share_dy = down / dot(down, 1.0f) - share;
}

#define GRASS_AT(name, shares) (name[0] * shares.x + name[1] * shares.y + name[2] * shares.z)

[numthreads(SCENE_SHADE_TILE, SCENE_SHADE_TILE, 1)]
void main(uint3 group_id : SV_GroupID, uint3 inside : SV_GroupThreadID)
{
    uint tile = g_tiles[SCENE_SHADE_GRASS * SCENE_SHADE_TILES + group_id.x];
    uint2 pixel = uint2(tile & 0xFFFFu, tile >> 16u) * SCENE_SHADE_TILE + inside.xy;
    uint id = g_ids[pixel];

    if ((id & SCENE_ID_GRASS_BIT) == 0u) {
        return;
    }

    uint slot = (id & ~SCENE_ID_GRASS_BIT) >> SCENE_ID_GRASS_SLOT_SHIFT;
    uint triangle_idx = id & SCENE_ID_GRASS_TRIANGLE_MASK;
    SceneGrassShade shade = g_grass_shade[0];

    // The draw the slot is in: the last whose place in the list is at or before it
    uint low = 0u;
    uint high = shade.counts.x;

    [loop] while (high - low > 1u) {
        uint middle = (low + high) >> 1u;
        bool is_before = g_forward_grass[middle] <= slot;
        low = is_before ? middle : low;
        high = is_before ? high : middle;
    }

    SceneGrassBin bin = g_grass_bins[low >> 1u];
    uint entry = g_forward_grass[SCENE_GRASS_LIST + slot];
    // The model's clusters: its far mesh, then the whole one, a corner a word
    SceneCluster cluster = g_clusters[bin.cluster + ((low & 1u) != 0u ? 0u : bin.far_clusters) + triangle_idx / SCENE_CLUSTER_TRIANGLES];
    uint first = cluster.index_start + (triangle_idx % SCENE_CLUSTER_TRIANGLES) * 3u;
    uint wave_idx = (max(bin.variant & 0xFFFFu, 1u) - 1u) * 4u;
    float4 waves[4] = { shade.waves[wave_idx], shade.waves[wave_idx + 1u], shade.waves[wave_idx + 2u], shade.waves[wave_idx + 3u] };

    float4 clip[3];
    float3 place[3];
    float3 normal[3];
    float2 uv[3];
    float3 light[3];
    float scatter = 0.0f;

    [unroll] for (uint corner_idx = 0u; corner_idx < 3u; corner_idx++) {
        grass_vertex pulled = grass_pull(bin, entry, g_words[first + corner_idx], shade.wind, waves, shade.shade.xy);
        place[corner_idx] = pulled.position_world;
        clip[corner_idx] = mul(m_VP, float4(pulled.position_world, 1.0f));
        clip[corner_idx].xy += m_taa_jitter.xy * clip[corner_idx].w;
        normal[corner_idx] = normalize(pulled.normal);
        uv[corner_idx] = pulled.vertex.UV;
        // The light of the vertex stage: the sky reaches a blade no more than its side turned to the eye looks up
        float3 to_eye = normal[corner_idx] * (dot(normal[corner_idx], eye_position - pulled.position_world) < 0.0f ? -1.0f : 1.0f);
        light[corner_idx] = saturate((L_ambient.rgb + L_hemi_color.rgb * min(abs(pulled.hemi), to_eye.y * 0.25f + 0.5f)) * pulled.shade);
        scatter = pulled.scatter;
    }

    float2 size = scaled_screen_res.xy;
    float2 at = (float2(pixel) + 0.5f) / size * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f);
    float3 share;
    float3 share_dx;
    float3 share_dy;
    grass_shares(clip[0], clip[1], clip[2], at, size, share, share_dx, share_dy);

    float2 tex = GRASS_AT(uv, share);
    float2 tex_dx = GRASS_AT(uv, share_dx);
    float2 tex_dy = GRASS_AT(uv, share_dy);
    float3 position = GRASS_AT(place, share);
    SceneMaterial material = g_materials[bin.material];
    // The base as the forward frame reads it: its head where one is loaded, else its tail
    float2 repeat = asfloat((127u - uint2(material.base_info & 15u, (material.base_info >> 4) & 15u)) << 23u);
    float4 base = sample_slice(material.base_head != SCENE_TEXTURE_ABSENT ? material.base_head : material.base, tex * repeat, tex_dx * repeat, tex_dy * repeat);

    // HACK: a BC1 blade's holes are black and the filter bleeds them into its edges: the holes' share is divided out
    [branch] if (material.flags & SCENE_MATERIAL_BASE_BC1) {
        base.rgb /= max(base.a, 1.0f / 32.0f);
    }

    float sky_reach = saturate(dot(GRASS_AT(light, share), 1.0f / 3.0f) / max(dot(L_hemi_color.rgb + L_ambient.rgb, 1.0f / 3.0f), 1e-3f));
#ifdef USE_SURFACE_HIGH
    // The rain wets the grass as the forward frame's default_matter does
    [branch] if (rain_setup.w > 0.0f) {
        float grass_open = smoothstep(0.1f, 0.6f, sky_reach);
        float grass_far = 1.0f - smoothstep(min(rain_setup.y - 15.0f, rain_setup.x), rain_setup.y, dot(share, float3(clip[0].w, clip[1].w, clip[2].w)));
        float grass_soak = grass_open * grass_far * grass_far * saturate(rain_params.x) * dot(base.rgb, 0.33f);
        base.rgb *= max(1.0f - sqrt(grass_soak), 0.5f);
    }
#endif

    // Grass is seen from both sides: its normal faces the eye
    float3 facing = GRASS_AT(normal, share);
    facing *= dot(facing, eye_position - position) < 0.0f ? -1.0f : 1.0f;

    scene_surface kept;
    kept.albedo = base.rgb * 0.5f;
    kept.normal = normalize(facing);
    kept.roughness = 0.0f;
    kept.scatter = 0.0f;
    kept.flags = SceneSurface_Gloss;
    kept.material_class = SceneClass_Grass;
    // A blade that sways lets the sun through: the metalness tells one that stands
    kept.metalness = scatter > 0.0f ? 0.0f : 1.0f;
    kept.hemi = sky_reach;
    float pattern = frac(52.9829189f * frac(dot(float2(pixel) + 0.5f, float2(0.06711056f, 0.00583715f))));
    g_surface[pixel] = scene_surface_pack(kept, pattern - 0.5f);
    g_thin[pixel] = thin_pack(normalize(mul((float3x3)m_V, kept.normal)), 1.0f, 1.0f, THIN_ID_MOVING, false, false, 0.5f);
}

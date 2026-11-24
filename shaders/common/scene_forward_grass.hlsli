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
// The grass of a forward frame drawn from the scene's grass cull: a draw a model, indexed by the model's own indices,
// an instance a visible blade. Included where calc_cyclic and calc_xz_wave are known

#include "common\detail_sway.hlsli"

struct grass_vertex {
    PulledVertex vertex;
    float3 position_world;
    float3 position_old; // where the sway had it a frame ago
    float3 normal; // of the model, turned as the blade is
    float hemi; // of the blade: its sign says whether the sun reaches the place
    float shade; // of its sway on its light
    float scatter; // 1 for a blade that sways, 0 for what stands still
    uint material;
};

// vertex_idx: an index of the model's own mesh; slot: the blade among the draw's visible ones
grass_vertex pull_grass(uint vertex_idx, uint slot)
{
    grass_vertex pulled = (grass_vertex)0;
    SceneGrassBin bin = grass.bins[forward_draw.y];
    uint entry = g_forward_grass[forward_draw.x + slot];
    pulled.vertex = Decode(LoadPage(bin.vertices >> 24, (bin.vertices & 0xFFFFFFu) + vertex_idx));
    SceneGrassItem item = g_forward_items[entry & (SCENE_GRASS_ITEMS - 1u)];
    float scale = item.scale * float(entry >> 24) * (1.0f / 127.0f);
    uint variant = bin.variant;
    uint wave_idx = (max(variant, 1u) - 1u) * 4u;
    float rigidity = DecodeS16x2(pulled.vertex.X).x * (1.0f / 2048.0f);

    float3x3 turn = QuaternionToMatrix(float4(item.quat, sqrt(max(0.0f, 1.0f - dot(item.quat, item.quat)))));
    // A copy of a far blade's clump stands beside the blade, turned its own way, on the ground the blade is turned to
    uint copy = (entry >> 20u) & 15u;
    float3 beside = 0.0f;

    [branch] if (copy != 0u) {
        float2 lot = frac(sin(dot(item.pos.xz, float2(12.9898f, 78.233f)) + float2(1.7f, 4.3f) * float(copy)) * 43758.5453f);
        float2 way;
        float2 spin;
        sincos(lot.x * 6.2831853f, way.y, way.x);
        sincos(lot.y * 6.2831853f, spin.y, spin.x);
        beside = float3(way.x, 0.0f, way.y) * (0.3f + 0.7f * lot.y);
        pulled.vertex.P.xz = float2(pulled.vertex.P.x * spin.x - pulled.vertex.P.z * spin.y, pulled.vertex.P.x * spin.y + pulled.vertex.P.z * spin.x);
        pulled.vertex.N.xz = float2(pulled.vertex.N.x * spin.x - pulled.vertex.N.z * spin.y, pulled.vertex.N.x * spin.y + pulled.vertex.N.z * spin.x);
    }

    // What does not sway lies on the ground (leaves, twigs): a centimetre off it, or it shares the ground's depth
    // and comes and goes as the eye moves
    float3 world = mul(turn, pulled.vertex.P * scale + beside + float3(0.0f, variant == 0u ? 0.01f : 0.0f, 0.0f)) + item.pos;
    float3 position = world;
    float3 position_old = world;
    DetailSway(grass.wind, grass.waves[wave_idx], grass.waves[wave_idx + 1u], grass.waves[wave_idx + 2u], grass.waves[wave_idx + 3u], variant == 0u, pulled.vertex.P, rigidity, scale, item.pos, item.trample_visual, item.trample_dir, world, position, position_old);

    float sway = calc_cyclic(dot(world, grass.waves[wave_idx].xyz) + grass.waves[wave_idx].w);
    pulled.position_world = position;
    pulled.position_old = position_old;
    pulled.normal = mul(turn, pulled.vertex.N);
    pulled.hemi = item.hemi;
    pulled.shade = variant == 0u ? 1.0f : forward_grass.y + forward_grass.x * max(0.0f, sway) * rigidity;
    pulled.material = bin.material;
    pulled.scatter = variant == 0u ? 0.0f : 1.0f;
    return pulled;
}

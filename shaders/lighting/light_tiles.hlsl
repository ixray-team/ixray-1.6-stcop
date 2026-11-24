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
#include "common\scene_tables.hlsli"
#include "common\fixed_cb.hlsli"

StructuredBuffer<SceneLight> g_forward_lights : register(t46);
StructuredBuffer<SceneDecal> g_forward_decals : register(t52);
RWTexture2D<uint> u_light_tiles : register(u0);

// Which of the frame's local lights and decals reach a 16 px tile of the screen, for s_light_tiles of
// light_lists.hlsli. A thread is a tile; a light is the sphere of its range and a decal the sphere around its
// box, tested against the four sides of the tile, which are planes through the eye
[numthreads(8, 8, 1)]
void cs_main(uint3 tile : SV_DispatchThreadID)
{
    uint2 tiles = (uint2(pos_decompression_params2.xy) + SCENE_LIGHT_TILE - 1u) / SCENE_LIGHT_TILE;

    if (any(tile.xy >= tiles)) {
        return;
    }

    // The tile a pixel wider on every side. x grows to the right and y downwards here, both from -1 to 1; a point
    // of view space is at x * m_P._11 / z across and y * m_P._22 / z up
    float2 low = (float2(tile.xy * SCENE_LIGHT_TILE) - 1.0f) * pos_decompression_params2.zw * 2.0f - 1.0f;
    float2 high = (float2((tile.xy + 1u) * SCENE_LIGHT_TILE) + 1.0f) * pos_decompression_params2.zw * 2.0f - 1.0f;
    float3 sides[4] = {
        normalize(float3(m_P._11, 0.0f, -low.x)),
        normalize(float3(-m_P._11, 0.0f, high.x)),
        normalize(float3(0.0f, m_P._22, high.y)),
        normalize(float3(0.0f, -m_P._22, -low.y)),
    };

    uint words[SCENE_TILE_WORDS];

    for (uint word_idx = 0u; word_idx < SCENE_TILE_WORDS; ++word_idx) {
        words[word_idx] = 0u;
    }

    uint count = min(g_forward_lights[0].flags, SCENE_LIGHTS);

    [loop]
    for (uint light_idx = 0u; light_idx < count; ++light_idx) {
        float4 place = g_forward_lights[light_idx + 1u].position;
        float reach = rsqrt(place.w);

        if (dot(sides[0], place.xyz) >= -reach && dot(sides[1], place.xyz) >= -reach && dot(sides[2], place.xyz) >= -reach && dot(sides[3], place.xyz) >= -reach) {
            words[light_idx >> 5] |= 1u << (light_idx & 31u);
        }
    }

    uint decals = min(uint(g_forward_decals[0].sphere.x), SCENE_DECALS);

    [loop]
    for (uint decal_idx = 0u; decal_idx < decals; ++decal_idx) {
        float4 round = g_forward_decals[decal_idx + 1u].sphere;

        if (dot(sides[0], round.xyz) >= -round.w && dot(sides[1], round.xyz) >= -round.w && dot(sides[2], round.xyz) >= -round.w && dot(sides[3], round.xyz) >= -round.w) {
            words[SCENE_LIGHTS / 32u + (decal_idx >> 5)] |= 1u << (decal_idx & 31u);
        }
    }

    for (uint out_idx = 0u; out_idx < SCENE_TILE_WORDS; ++out_idx) {
        u_light_tiles[uint2(tile.x * SCENE_TILE_WORDS + out_idx, tile.y)] = words[out_idx];
    }
}

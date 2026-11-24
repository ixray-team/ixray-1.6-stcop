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
#ifndef LIGHT_LISTS_HLSLI
#define LIGHT_LISTS_HLSLI

#include "common\scene_tables.hlsli"

// The frame's local lights as a forward pixel shader finds them. g_forward_lights: record 0 is a header (flags -
// the number of lights, shadow - the number of shadow entries), the lights follow it; positions and directions are
// in view space. s_light_tiles: per 16 px tile of the screen SCENE_TILE_WORDS texels side by side, 32 bits each:
// first the lights (bit n is set where light n reaches the tile), then the decals of g_forward_decals the same
// way. A pixel looks its tile up by its place on the screen alone and scans the bits
StructuredBuffer<SceneLight> g_forward_lights : register(t46);
StructuredBuffer<SceneLightShadow> g_forward_shadows : register(t47);
Texture2D<uint> s_light_tiles : register(t48);
StructuredBuffer<SceneDecal> g_forward_decals : register(t52);

// What the frame's spot lights project, in the slots SceneLight::projector names
#ifndef FORWARD_PROJECTORS
#define FORWARD_PROJECTORS
Texture2D s_forward_projector0 : register(t36);
Texture2D s_forward_projector1 : register(t38);
Texture2D s_forward_projector2 : register(t39);
Texture2D s_forward_projector3 : register(t41);
Texture2D s_forward_projector4 : register(t42);
Texture2D s_forward_projector5 : register(t43);
Texture2D s_forward_projector6 : register(t44);
Texture2D s_forward_projector7 : register(t45);
#endif

#define LIGHT_TILE_WORDS (SCENE_LIGHTS / 32u)

uint light_count()
{
    return g_forward_lights[0].flags;
}

uint light_tile_word(uint2 pixel, uint word_idx)
{
    uint2 tile = pixel / SCENE_LIGHT_TILE;
    return s_light_tiles.Load(int3(tile.x * SCENE_TILE_WORDS + word_idx, tile.y, 0));
}

uint decal_count()
{
    return uint(g_forward_decals[0].sphere.x);
}

uint decal_tile_word(uint2 pixel, uint word_idx)
{
    uint2 tile = pixel / SCENE_LIGHT_TILE;
    return s_light_tiles.Load(int3(tile.x * SCENE_TILE_WORDS + LIGHT_TILE_WORDS + word_idx, tile.y, 0));
}

// returns what the projector in a slot lets through at uv; past the edge it repeats its border, as
// accum_base.hlsl samples it
float3 light_projector(uint slot, float2 uv)
{
    switch (slot) {
    case 1u:
        return s_forward_projector1.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 2u:
        return s_forward_projector2.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 3u:
        return s_forward_projector3.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 4u:
        return s_forward_projector4.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 5u:
        return s_forward_projector5.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 6u:
        return s_forward_projector6.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 7u:
        return s_forward_projector7.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    default:
        return s_forward_projector0.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    }
}

#endif

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
// What a pixel keeps of its surface for the pass that lights it, two words:
// .x: bits 0-23 half the albedo, a gamma value (the base texture times its detail), 24-31 the roughness, or the
//     gloss of a surface with the old maps
// .y: bits 0-17 the normal in the world (octahedral, 9 + 9, the first of them plus one: a written pixel is never 0),
//     18-21 the metalness in twelfths, dithered, or above 12 how much of the sun scatters through the surface, in
//     thirds (what scatters is no metal), 22-27 the square root of the share of the sky's light (a linear step rings
//     in the dark), 28-29 the class, 30-31 SceneSurface_
#ifndef SCENE_GBUFFER_HLSLI
#define SCENE_GBUFFER_HLSLI

#define SceneClass_Plain 0u
#define SceneClass_Terrain 1u
// Of an id: a tree, placed by the scene's table. Of a surface: what lets the sun through, a leaf
#define SceneClass_Foliage 2u
// Of a surface: a blade of grass, which lets the sun through unless it stands still; its metalness is 1 where it does.
// Its depth pass keeps the surface alone, and the tiles' pass tells the screen-space passes of it (scene_tiles)
#define SceneClass_Grass 3u

#define SceneSurface_Hud 1u
#define SceneSurface_Gloss 2u               // The old maps: a gloss in the place of the roughness, no metalness

struct scene_surface {
    float3 albedo;                          // half of it, a gamma value
    float roughness;
    float3 normal;
    float metalness;
    float hemi;
    uint material_class;
    uint flags;
    float scatter;
};

// dither: the pixel's own offset in -0.5 to 0.5, by which the hemi and the metalness round
uint2 scene_surface_pack(scene_surface surface, float dither)
{
    float3 folded = surface.normal / (abs(surface.normal.x) + abs(surface.normal.y) + abs(surface.normal.z));
    float2 wrapped = (1.0f - abs(folded.yx)) * float2(folded.x >= 0.0f ? 1.0f : -1.0f, folded.y >= 0.0f ? 1.0f : -1.0f);
    uint2 code = uint2(saturate((folded.z >= 0.0f ? folded.xy : wrapped) * 0.5f + 0.5f) * 510.0f + 0.5f);
    uint scatter = uint(saturate(surface.scatter) * 3.0f + 0.5f);
    uint metal = scatter > 0u ? 0u : min(uint(max(saturate(surface.metalness) * 12.0f + 0.5f + dither, 0.0f)), 12u);
    // Below a full metal the albedo holds the share that scatters, by the metalness as stored: the colour a metal
    // mirrors is told back from it exactly
    uint3 albedo = uint3(saturate(surface.albedo * (metal < 12u ? 1.0f - float(metal) / 12.0f : 1.0f)) * 255.0f + 0.5f);
    uint2 packed;
    packed.x = albedo.x | albedo.y << 8u | albedo.z << 16u | uint(saturate(surface.roughness) * 255.0f + 0.5f) << 24u;
    packed.y = (code.x + 1u) | code.y << 9u | (scatter > 0u ? 12u + scatter : metal) << 18u;
    packed.y |= uint(saturate(sqrt(saturate(surface.hemi)) + dither / 63.0f) * 63.0f + 0.5f) << 22u | surface.material_class << 28u | surface.flags << 30u;
    return packed;
}

scene_surface scene_surface_unpack(uint2 packed)
{
    scene_surface surface;
    surface.albedo = float3(packed.x & 255u, (packed.x >> 8u) & 255u, (packed.x >> 16u) & 255u) * (1.0f / 255.0f);
    surface.roughness = float(packed.x >> 24u) * (1.0f / 255.0f);
    float2 folded = (float2((packed.y & 511u) - 1u, (packed.y >> 9u) & 511u) - 255.0f) * (1.0f / 255.0f);
    float3 raw = float3(folded, 1.0f - abs(folded.x) - abs(folded.y));
    float under = saturate(-raw.z);
    raw.xy += raw.xy >= 0.0f ? -under : under;
    surface.normal = normalize(raw);
    uint metal = (packed.y >> 18u) & 15u;
    surface.metalness = metal > 12u ? 0.0f : float(metal) * (1.0f / 12.0f);
    surface.scatter = metal > 12u ? float(metal - 12u) * (1.0f / 3.0f) : 0.0f;
    surface.hemi = float((packed.y >> 22u) & 63u) * (1.0f / 63.0f);
    surface.hemi *= surface.hemi;
    surface.material_class = (packed.y >> 28u) & 3u;
    surface.flags = packed.y >> 30u;
    return surface;
}

// returns where the surface of a pixel is in the world, by its depth. The includer has CBView
#ifdef FIXED_CB_HLSLI
float3 scene_surface_position(uint2 pixel, float depth)
{
    float2 at = (float2(pixel) + 0.5f) / scaled_screen_res.xy * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f) - m_taa_jitter.xy;
    float4 seen = mul(m_invP, float4(at, depth, 1.0f));
    return mul(m_invV, float4(seen.xyz / seen.w, 1.0f));
}
#endif

#endif

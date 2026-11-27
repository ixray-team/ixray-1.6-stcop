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
#ifndef THIN_SURFACE_HLSLI
#define THIN_SURFACE_HLSLI

#include "common\material.hlsli"

// What the screen-space passes that run before lighting know of a surface beside its depth, in one word a pixel:
// bits 0-15 the normal in view space (octahedral, 8 + 8), 16-19 the hemi, 20-27 the roughness, 28-29 the material
// id, 30 whether the surface reflects at all, 31 whether it is grass
Texture2D<uint> s_thin : register(t39);

// The id of a model of the forward level frame: what may move by itself
#define THIN_ID_MOVING 1u

struct thin_surface {
    float3 normal;
    float hemi;
    float roughness;
    uint id;
    bool reflects;
    bool grass;
};

// dither: where in its step the normal rounds up, 0.5 to round to the nearest. A flat mirror's normal is one value
// across it: rounded alike, the whole reflection jumped a step (about two degrees of the mirrored ray) as the eye
// turned, and a dither per pixel leaves the reflection's neighbours to average it
uint thin_pack(float3 normal, float hemi, float roughness, uint id, bool reflects, bool grass, float dither)
{
    uint2 octahedral = uint2(saturate(NormalEncode(normal) * 0.5f + 0.5f) * 255.0f + dither);
    uint packed = octahedral.x | (octahedral.y << 8);
    packed |= uint(saturate(hemi) * 15.0f + 0.5f) << 16;
    packed |= uint(saturate(roughness) * 255.0f + 0.5f) << 20;
    packed |= (id & 3u) << 28;
    packed |= (reflects ? 1u : 0u) << 30;
    packed |= (grass ? 1u : 0u) << 31;
    return packed;
}

thin_surface thin_unpack(uint packed)
{
    thin_surface surface;
    surface.normal = NormalDecode(float2(packed & 255u, (packed >> 8) & 255u) / 255.0f * 2.0f - 1.0f);
    surface.hemi = float((packed >> 16) & 15u) / 15.0f;
    surface.roughness = float((packed >> 20) & 255u) / 255.0f;
    surface.id = (packed >> 28) & 3u;
    surface.reflects = (packed >> 30) & 1u;
    surface.grass = packed >> 31;
    return surface;
}

#endif

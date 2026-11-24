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
#define SURFACE_NO_WETNESS
#include "common\surface_forward.hlsli"
#define SURFACE_NO_DECALS
#define SURFACE_NO_DEBUG_VIEW
#define USE_VERTEX_ENTRY

// returns the vertex lifted off its wall and towards the eye, more the further away it is: the mark is not buried
// in the wall it lies on. Precise: the depth pass of a mark drawn from the scene's clusters and its colour pass meet
// where the depth is equal
surface_vertex vtx_entry(surface_vertex output, float4 position, float3 normal)
{
    precise float3 at = output.position_world;
    float3 to_eye = eye_position - at;
    float dist = length(to_eye);
    at += normal * 0.007f - normalize(eye_direction - to_eye / dist) * lerp(0.003f, 0.011f, dist);
    precise float4 projected = mul(m_VP, float4(at, 1.0f));
    output.position_proj = projected;
    output.position_world = at;
    output.light.w = r1_fog(at);
    return output;
}

// returns the surface under the pixel
surface_data fetch_entry(surface_vertex input)
{
    return default_fetch(input);
}

// returns what one local light adds to the surface's light
float3 light_entry(surface_vertex input, surface_data surface, light_shading_data light)
{
    return default_light(input, surface, light);
}

// returns final output color: the fog thins a mark out, and brings one that multiplies the wall to no change
float4 shade_entry(surface_vertex input, surface_data surface)
{
#ifdef WALLMARK_MULTIPLY
    // One half leaves the wall as it is, in gamma and in linear
    float4 mark = lerp(0.5f, surface.base, input.light.w);
    return float4(surface_scene(mark.rgb * 2.0f) * 0.5f, mark.a);
#else
    return float4(surface_plain(surface.base.rgb, surface), surface.base.a * input.light.w);
#endif
}

#include "common\surface_forward_entry.hlsli"

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

#ifdef LIGHTPLANE_NOISE
Texture2D s_arrow_lmap : register(t40);
#endif

// returns the vertex with how much of its face the eye sees: a plane of light fades out when seen edge-on. One of the
// level stands in the world, as a cluster of the scene has no object matrix
surface_vertex vtx_entry(surface_vertex output, float4 position, float3 normal)
{
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Static
    output.tint.a = abs(dot(normalize(mul(m_V, float4(output.position_world, 1.0f)).xyz), normalize(mul((float3x3)m_V, normal))));
#elif defined(SURFACE_CLUSTER)
    // A model of the scene has its place and normal in the world, not in m_WV: the angle is the same
    output.tint.a = abs(dot(normalize(output.position_world - eye_position), output.normal));
#else
    output.tint.a = abs(dot(normalize(mul(m_WV, position).xyz), normalize(mul((float3x3)m_WV, normal))));
#endif
    return output;
}

// returns the surface under the pixel
surface_data fetch_entry(surface_vertex input)
{
    surface_data surface = default_fetch(input);
#ifdef LIGHTPLANE_NOISE
    surface.base.rgb += s_arrow_lmap.Sample(smp_base, input.uv.xy).rgb * m_affects.x * 2.0f;
#endif
    return surface;
}

// returns what one local light adds to the surface's light
float3 light_entry(surface_vertex input, surface_data surface, light_shading_data light)
{
    return default_light(input, surface, light);
}

// returns final output color, no fog: it is added to the picture
float4 shade_entry(surface_vertex input, surface_data surface)
{
#ifdef LIGHTPLANE_PLAIN
    return float4(surface_scene(surface.base.rgb), surface.base.a);
#else
    return float4(surface_scene(surface.base.rgb), surface.base.a * input.tint.a);
#endif
}

#include "common\surface_forward_entry.hlsli"

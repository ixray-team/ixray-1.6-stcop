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
#define SURFACE_NO_SUN_SHADOWS
#define SURFACE_NO_LAMP_SHADOWS
#define SURFACE_CARD
#include "common\surface_forward.hlsli"
#define SURFACE_NO_DECALS

#ifndef SURFACE_CLUSTER
Texture2D s_base0 : register(t49);
Texture2D s_base1 : register(t50);
Texture2D s_hemi0 : register(t51);
Texture2D s_hemi1 : register(t52);
#endif

// returns the surface under the pixel: two views of the tree, blended by how far the eye is between them
surface_data fetch_entry(surface_vertex input)
{
    surface_data surface;
    surface.local = 0.0f;
    surface.sun = 0.0f;
#ifdef SURFACE_CLUSTER
    // A card the scene draws: both views are of its material's maps in the buckets
    surface_material_idx = input.material & DEBUG_MATERIAL_MASK;
    float4 base0 = surface_map(surface_material.base, surface_material.base_head, surface_material.base_info, input.uv.xy);
    float4 base1 = surface_map(surface_material.base, surface_material.base_head, surface_material.base_info, input.uv.zw);
    float hemi0 = surface_map(surface_material.hemi, surface_material.hemi_head, surface_material.hemi_info, input.uv.xy).w;
    float hemi1 = surface_map(surface_material.hemi, surface_material.hemi_head, surface_material.hemi_info, input.uv.zw).w;
#else
    float4 base0 = s_base0.Sample(smp_base, input.uv.xy);
    float4 base1 = s_base1.Sample(smp_base, input.uv.zw);
    float hemi0 = s_hemi0.Sample(smp_base, input.uv.xy).w;
    float hemi1 = s_hemi1.Sample(smp_base, input.uv.zw).w;
#endif
    surface.base = lerp(base0, base1, input.detail.z);
    surface.base.a *= input.tint.a;
#ifdef USE_ALPHA_COVERAGE
    // r_msaa: a card's alpha is its coverage as it is, so the texels of its leaves blend instead of standing apart
    clip(surface.base.a - 0.5f / 255.0f);
#else
    clip(surface.base.a - m_AlphaRef);
#endif
    float hemi = lerp(hemi0, hemi1, input.detail.z);
    surface.light = input.light.rgb * (0.5f + 0.5f * hemi);
    surface.detail = 0.5f;
    surface.specular = 0.0f;
    surface.mirror = 0.0f;
    surface.mirrored = 0.0f;
    surface.reflect = 0.0f;
    surface.f0 = 0.0f;
    surface.roughness = 1.0f;
    surface.gloss = 0.0f;
    surface.normal = input.normal;
    return surface;
}

// returns what one local light adds to the surface's light
float3 light_entry(surface_vertex input, surface_data surface, light_shading_data light)
{
    return default_light(input, surface, light);
}

// returns final output color
float4 shade_entry(surface_vertex input, surface_data surface)
{
    return default_shade(input, surface);
}

#include "common\surface_forward_entry.hlsli"

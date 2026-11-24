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
#ifndef DECALS_HLSLI
#define DECALS_HLSLI

#include "common\light_lists.hlsli"

// The decals' textures, a layer each; SceneDecal::layer names one
Texture2DArray s_decals : register(t53);

// A face steeper to a decal's box than this takes none of it: the mark would be a streak
#define DECAL_FACING 0.3f

// returns what the decals of a pixel's tile multiply its base colour by: 1 where none lies. A decal is a
// texture that leaves the colour alone at 0.5, darkens below and brightens above; it fades to that as it ages.
// view_pos and view_normal are the surface in view space; across, down: how view_pos changes a pixel across and down
float3 decal_tint(float3 view_pos, float3 across, float3 down, float3 view_normal, uint2 pixel)
{
    float3 tint = 1.0f;

    if (decal_count() == 0u) {
        return tint;
    }

    for (uint word_idx = 0u; word_idx < (decal_count() + 31u) / 32u; ++word_idx) {
        uint bits = decal_tile_word(pixel, word_idx);

        while (bits != 0u) {
            SceneDecal decal = g_forward_decals[word_idx * 32u + firstbitlow(bits) + 1u];
            bits &= bits - 1u;

            float3 at = float3(dot(decal.to_box[0], float4(view_pos, 1.0f)), dot(decal.to_box[1], float4(view_pos, 1.0f)), dot(decal.to_box[2], float4(view_pos, 1.0f)));

            if (any(abs(at) >= 1.0f) || abs(dot(normalize(decal.to_box[2].xyz), view_normal)) < DECAL_FACING) {
                continue;
            }

            float2 uv = float2(1.0f + at.x, 1.0f - at.y) * 0.5f;
            float2 uv_across = float2(dot(decal.to_box[0].xyz, across), -dot(decal.to_box[1].xyz, across)) * 0.5f;
            float2 uv_down = float2(dot(decal.to_box[0].xyz, down), -dot(decal.to_box[1].xyz, down)) * 0.5f;
            float3 mark = s_decals.SampleGrad(smp_linear, float3(uv, decal.layer), uv_across, uv_down).rgb;
            tint *= lerp(1.0f, mark * 2.0f, decal.opacity);
        }
    }

    return tint;
}

// A stage with no derivatives of its own hands the slopes in (DECAL_NO_DERIVATIVES)
#ifndef DECAL_NO_DERIVATIVES
float3 decal_tint(float3 view_pos, float3 view_normal, uint2 pixel)
{
    return decal_tint(view_pos, ddx(view_pos), ddy(view_pos), view_normal, pixel);
}
#endif

#endif

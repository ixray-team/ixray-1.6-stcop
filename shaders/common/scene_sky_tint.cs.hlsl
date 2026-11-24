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
// The colour of the sky along each axis, both ways, once a frame: the weather's two skies at their smallest, blended
// as the weather blends them. scene_light tints the sky's light by it; read from the two cubes by every pixel that
// mirrors, it was a seventh of that pass
#include "common\fixed_cb.hlsli"
#include "common\common_samplers.hlsli"

TextureCube g_sky0 : register(t0);
TextureCube g_sky1 : register(t1);
RWStructuredBuffer<float4> g_sky_tint : register(u0);

[numthreads(6, 1, 1)]
void main(uint3 thread_id : SV_DispatchThreadID)
{
    float3 way = float3(uint3(0u, 1u, 2u) == (thread_id.x >> 1u)) * ((thread_id.x & 1u) ? -1.0f : 1.0f);
    float3 tint = lerp(g_sky0.SampleLevel(smp_rtlinear, way, 16.0f).rgb, g_sky1.SampleLevel(smp_rtlinear, way, 16.0f).rgb, L_hemi_color.w);
    g_sky_tint[thread_id.x] = float4(tint, 1.0f);
}

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
// r_debug_view velocity: how far each pixel has moved since the frame before. The direction is the hue: right red,
// up yellow-green, left cyan, down violet. The length is the brightness on a log scale: a quarter of a pixel shows,
// sixteen pixels are full colour
#include "common\common.hlsli"

float4 ps_main(PSInputFullscreen input) : SV_Target
{
    float2 motion = s_velocity.Load(int3(input.hpos.xy, 0)).xy * 0.5f * scaled_screen_res.xy;
    float hue = atan2(motion.y, motion.x) * (0.5f / PI);
    float3 color = saturate(abs(frac(hue + float3(1.0f, 2.0f / 3.0f, 1.0f / 3.0f)) * 6.0f - 3.0f) - 1.0f);
    return float4(color * saturate(log2(1.0f + length(motion) * 4.0f) / 6.0f), 1.0f);
}

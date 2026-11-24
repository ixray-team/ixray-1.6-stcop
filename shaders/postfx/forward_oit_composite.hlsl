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
// The blended surfaces the forward level frame weighed order-independently, laid over its scene by the pass's blend:
// their weighed mean colour, covering the scene by what they took of it. The sums are what the surfaces' ps_main of
// common\surface_forward_entry.hlsli wrote under USE_SURFACE_WBOIT
Texture2D s_oit_accum : register(t0);
Texture2D<float> s_oit_revealage : register(t1);

float4 ps_main(float4 position : SV_Position) : SV_Target
{
    int3 pixel = int3(position.xy, 0);
    float revealage = s_oit_revealage.Load(pixel);

    [branch] if (revealage >= 1.0f) {
        discard;
    }

    float4 accum = s_oit_accum.Load(pixel);
    return float4(accum.rgb / max(accum.a, 1e-5f), 1.0f - revealage);
}

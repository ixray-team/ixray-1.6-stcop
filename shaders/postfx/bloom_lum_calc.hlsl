/*
Made by Papa Doenitz for IX-ray engine 2026-03-05
CC BY-NC-SA 4.0 Lisence https://creativecommons.org/licenses/by-nc-sa/4.0/

Credits goes to:
Bruno Opsenica https://bruop.github.io/exposure/
Krzysztof Narkowicz https://knarkowicz.wordpress.com/2016/01/09/automatic-exposure/
The Real MJP https://mynameismjp.wordpress.com/2011/08/10/average-luminance-compute-shader/
Epic Games, "How Epic Games is handling Auto Exposure in 4.25"
Unreal Engine Documentation, "Auto Exposure / Eye Adaptation"
*/

#include "common\common.hlsli"

uniform Texture2D p_image : register(t25);

#include "common\post_cb.hlsli"

// The average log2 luminance of the 16x16 level, blended over the stored one by adapt_params.z
float4 ps_main(PSInputFullscreen I) : SV_Target
{
    float LumaCurr = 0.f;
    [unroll]
    for (int y = 0; y < 16; y++)
    {
        for (int x = 0; x < 16; x++)
        {
            LumaCurr += s_image.SampleLevel(smp_rtlinear, (float2(x, y) + 0.5f) / 16.f, 0).r;
        }
    }

    return float2(LumaCurr * rcp(256.f), adapt_params.z).xxxy;
}

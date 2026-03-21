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

#include "common.hlsli"

uniform Texture2D b_image;
float4 adapt_params;

static const float2 offsets2[16] =
{
    float2(-0.375f, -0.375f), float2(-0.125f, -0.375f), float2(0.125f, -0.375f), float2(0.375f, -0.375f),
    float2(-0.375f, -0.125f), float2(-0.125f, -0.125f), float2(0.125f, -0.125f), float2(0.375f, -0.125f),
    float2(-0.375f, 0.125f),  float2(-0.125f, 0.125f),  float2(0.125f, 0.125f),  float2(0.375f, 0.125f),
    float2(-0.375f, 0.375f),  float2(-0.125f, 0.375f),  float2(0.125f, 0.375f),  float2(0.375f, 0.375f)
};

float main(PSInputFullscreen I) : SV_Target 
{
    float res = 0.f;
    float2 center = I.texcoord.xy ;
    float x = adapt_params.z;
    float y = adapt_params.w;

    for (int i = 0; i < 16; i++)
    {
        res += b_image.Sample(smp_rtlinear, center + offsets2[i] * float2(x,y)).r;
    }
    res *= 0.0625;

    return res;
}

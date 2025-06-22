#include "common.hlsli"

Texture2D s_patched_normal;

float4 main(float2 tc : TEXCOORD0, float2 tcJ : TEXCOORD1) : SV_Target
{
    float Gloss = s_patched_normal.Sample(smp_nofilter, tc).w;

    float ColorIntencity = 1.0f - sqrt(Gloss);
    ColorIntencity = max(ColorIntencity, 0.5f);

    return float4(ColorIntencity, ColorIntencity, ColorIntencity, 0.0f);
}

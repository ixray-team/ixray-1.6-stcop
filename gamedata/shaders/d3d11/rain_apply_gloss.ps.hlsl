#include "common.hlsli"

Texture2D s_patched_normal;

float4 main(float2 tc : TEXCOORD0, float2 tcJ : TEXCOORD1) : SV_Target
{
    float Gloss = s_patched_normal.Sample(smp_nofilter, tc).w;

    float ColorIntencity = 1.0f - sqrt(Gloss);
    ColorIntencity = max(ColorIntencity, 0.5f);
    Gloss *= 0.8f;

    // TODO (Hozar to ???): Disable on PBS pipeline
#ifndef USE_LEGACY_LIGHT
    Gloss *= 0.0f;
#endif

    return float4(ColorIntencity, ColorIntencity, ColorIntencity, Gloss);
}

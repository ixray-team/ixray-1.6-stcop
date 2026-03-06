#include "common.hlsli"
#include "sload.hlsli"
#include "shadow.hlsli"

Texture2D s_depth  : register(t1);
SamplerState smp_depth : register(s1);

// p_TL обычно содержит SV_Position и Tex0 + Color
float4 main(p_TL I) : SV_Target
{
    float4 res = 1.f;
    float4 t = s_base.Sample(smp_base, I.Tex0) * I.Color;
    return GammaToLinear(t);
}
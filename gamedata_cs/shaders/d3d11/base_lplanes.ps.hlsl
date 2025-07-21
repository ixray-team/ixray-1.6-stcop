#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float4 c0 : COLOR0; // sun
};

// Pixel
float4 main(v2p I) : SV_Target
{
    float4 t_base = s_base.Sample(smp_base, I.tc0);
    return PushGamma(float4(t_base.xyz, t_base.w * I.c0.w));
}


#include "common.hlsli"

struct v2p
{
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;

    float3 tctexgen : TEXCOORD1;
    float4 hpos : SV_POSITION;
    float fog : FOG;
};

// Pixel
float4 main(v2p I) : SV_Target
{
    return GammaToLinear(I.c.a * s_base.Sample(smp_base, I.tc).a);
}


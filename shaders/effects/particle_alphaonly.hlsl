#include "common\common.hlsli"

struct v2p
{
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;
};

// Pixel
float4 ps_main(v2p I) : SV_Target
{
    return GammaToLinear(I.c.a * s_base.Sample(smp_base, I.tc).a);
}

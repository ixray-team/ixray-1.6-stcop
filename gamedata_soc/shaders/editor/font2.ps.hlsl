#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
};

// Pixel
float4 main(v2p I) : SV_Target
{
    float4 r = s_base.SampleLevel(smp_base, I.tc0, 0); //No mips
    r.w = 1 - r.w;
    return r;
}

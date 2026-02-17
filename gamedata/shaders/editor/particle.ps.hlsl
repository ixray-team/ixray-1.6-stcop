#include "common.hlsli"

struct vf
{
    float4 hpos : SV_POSITION;
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;
    float fog : FOG;
};

// Pixel
float4 main(vf I) : SV_Target
{
    return I.c * s_base.Sample(smp_base, I.tc);
}

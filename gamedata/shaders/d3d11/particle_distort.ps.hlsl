#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0;
    float4 c : COLOR0;

    float3 tctexgen : TEXCOORD1;

    float4 hpos : SV_POSITION;
    float fog : FOG;
};

// Pixel
Texture2D s_distort;
float4 main(v2p I) : SV_Target
{
    float4 distort = s_distort.Sample(smp_linear, I.tc0);
    float factor = distort.w * dot(I.c.xyz, 0.3333f);
	
    return float4(distort.xyz, factor);
}


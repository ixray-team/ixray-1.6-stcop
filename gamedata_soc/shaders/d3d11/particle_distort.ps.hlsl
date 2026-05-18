#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float4 c : COLOR0; // diffuse

#ifdef USE_SOFT_PARTICLES
    //	Igor: for additional depth dest
    float4 tctexgen : TEXCOORD1;
#endif //	USE_SOFT_PARTICLES

    float4 hpos : SV_POSITION;
};

// Pixel
Texture2D s_distort;
float4 main(v2p I) : SV_Target
{
    float4 distort = s_distort.Sample(smp_linear, I.tc0);
    float factor = distort.w * dot(I.c.xyz, 0.3333f);
	
	// #ifdef USE_SOFT_PARTICLES
    // float4 Point = GbufferGetPoint(I.hpos.xy);
	
    // float spaceDepth = Point.z - I.tctexgen.z;
    // factor *= Contrast(saturate(spaceDepth * 1.3f), 2.0f);
	// #endif //	USE_SOFT_PARTICLES
	
    return float4(distort.xyz, factor);
}


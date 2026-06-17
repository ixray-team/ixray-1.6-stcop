#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0;
    float4 c : COLOR0;

    float4 tctexgen : TEXCOORD1;

    float4 hpos : SV_POSITION;
    float fog : FOG;
};

//	Must be less than view near
#define DEPTH_EPSILON 0.1h

void main(v2p I, out IXRayForward O)
{
    float4 result = I.c * s_base.Sample(smp_base, I.tc0);

#if defined(USE_SOFT_PARTICLES) && !defined(DISABLE_SOFT_PARTICLES)
    float3 Point = GbufferGetPoint(I.hpos.xy);
    float spaceDepth = Point.z - I.tctexgen.z;
    result *= Contrast(saturate(spaceDepth * 1.3f), 2.0f);
#endif

    clip(result.a - (0.01f / 255.0f));

    O.Color.xyz = GammaToLinear(result.xyz * I.fog);
    O.Color.w = result.w * I.fog;
	
	O.Velocity = 0.0f;
}


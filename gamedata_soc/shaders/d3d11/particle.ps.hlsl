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
void main(v2p I, out IXRayForward O)
{
    float4 result = I.c * s_base.Sample(smp_base, I.tc);

#if defined(USE_SOFT_PARTICLES) && !defined(DISABLE_SOFT_PARTICLES)
    float3 Point = GbufferGetPoint(I.hpos.xy);
    float spaceDepth = Point.z - I.tctexgen.z;
    result *= Contrast(saturate(spaceDepth * 1.3f), 2.0f);
#endif

   // clip(result.a - (0.01f / 255.0f));
	result = lerp(fog_color, result, I.fog);
	
    O.Color.xyz = GammaToLinear(result.xyz);
	O.Color.w = result.w;
	
#ifndef USE_LEGACY_LIGHT
	O.Color.xyz *= 0.8f;
#endif
	
	clip(O.Color.w  - EPS);
	
	O.Velocity = 0.0f;
}


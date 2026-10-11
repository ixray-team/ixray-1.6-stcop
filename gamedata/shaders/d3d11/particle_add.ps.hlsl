#include "common.hlsli"

struct v2p
{
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;

    float3 tctexgen : TEXCOORD1;
    float4 hpos : SV_POSITION;
    float fog : FOG;
};

#ifdef EDITOR_MODE
// x: 1 in the unlit editor view: no scene depth (soft particles) and no weather (fog)
uniform float4 editor_params;
#define EDITOR_LIT (editor_params.x < 0.5f)
#else
#define EDITOR_LIT true
#endif

void main(v2p I, out IXRayForward O)
{
    O = (IXRayForward)0;
    float4 result = I.c * s_base.Sample(smp_base, I.tc);

#if defined(USE_SOFT_PARTICLES) && !defined(DISABLE_SOFT_PARTICLES)
    if (EDITOR_LIT)
    {
        float3 Point = GbufferGetPoint(I.hpos.xy).xyz;
        float spaceDepth = Point.z - I.tctexgen.z;
        result *= Contrast(saturate(spaceDepth * 1.3f), 2.0f);
    }
#endif

    clip(result.w - EPS);

    float Fog = EDITOR_LIT ? I.fog : 1.0f;
    O.Color.xyz = GammaToLinear(result.xyz * Fog);
    O.Color.w = result.w * Fog;

#ifndef USE_PBR
	O.Color.w = GammaToLinear(O.Color.w);
#endif
	
#ifndef DISABLE_MOTION_VECTORS
	O.Velocity = 0.0f;
#endif
}


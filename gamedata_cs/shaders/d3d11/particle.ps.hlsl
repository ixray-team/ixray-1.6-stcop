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

// Pixel
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

   // clip(result.a - (0.01f / 255.0f));
	result = EDITOR_LIT ? lerp(fog_color, result, I.fog) : result;
    O.Color.xyz = GammaToLinear(result.xyz);
	
#ifdef USE_PBR
	O.Color.w = result.w;
#else
	O.Color.w = GammaToLinear(result.w);
#endif
	
	clip(O.Color.w - EPS);
	
#ifndef DISABLE_MOTION_VECTORS
	O.Velocity = 0.0f;
#endif
}


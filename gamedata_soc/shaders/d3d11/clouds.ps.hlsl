#include "common.hlsli"

struct v2p
{
    float4 color : COLOR0;
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
};

Texture2D s_clouds0 : register(t0);
Texture2D s_clouds1 : register(t1);

// Pixel
float4 main(v2p I) : SV_Target
{
    float4 s0 = s_clouds0.Sample(smp_base, I.tc0);
    float4 s1 = s_clouds1.Sample(smp_base, I.tc1);
    float4 mix = I.color * (s0 + s1);
	
#ifdef USE_LEGACY_SKY_TONEMAP
	return float4(detonemap(mix.xyz), mix.w);
#else
	return float4(GammaToLinear(mix.xyz), mix.w);
#endif
}


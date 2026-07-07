#include "common.hlsli"

struct v2p
{
    float4 factor : COLOR0;
    float3 p : TEXCOORD1;

    float4 hpos : SV_POSITION;
};

TextureCube s_sky0 : register(t0);
TextureCube s_sky1 : register(t1);

void main(in v2p I, out float4 Color : SV_Target0)
{
	float3 TexCoord = I.p;
	
#ifndef USE_FULL_SKY_SPHERE
    RemapVector(TexCoord);
#endif

	float3 s0 = s_sky0.SampleLevel(smp_rtlinear, TexCoord, 0.0f).xyz;
	float3 s1 = s_sky1.SampleLevel(smp_rtlinear, TexCoord, 0.0f).xyz;
	float3 sky = lerp(s0, s1, I.factor.w);
    
#ifdef USE_BGRA_SKYCOLOR
    sky *= L_sky_color.zyx;
#else
    sky *= L_sky_color.xyz;
#endif

	Color = float4(sky * 0.66f, 0.0f);
}


#include "common.hlsli"

float3 main(p_TL I) : SV_Target
{
	float4 Color = s_base.Sample(smp_rtlinear, I.Tex0);

	Color.w *= GetBorderAtten(I.Tex0);
	Color.w *= 1.0f - I.Color.w;

	Color.xyz = lerp(I.Color.xyz, Color.xyz, Color.w);

	clip(Color.w - EPS_S);
	return Color.xyz;
}


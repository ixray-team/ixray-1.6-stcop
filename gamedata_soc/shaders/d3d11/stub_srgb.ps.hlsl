#include "common.hlsli"

// Pixel
float4 main(p_TL I) : SV_Target
{
	float4 final = s_base.Sample(smp_base, I.Tex0) * I.Color;
	return PushGamma(final);
}


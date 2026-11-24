#include "common\common.hlsli"

// Pixel
float4 ps_main(p_TL I) : SV_Target
{
	float4 final = s_base.Sample(smp_base, I.Tex0) * I.Color;
	final.xyz = SceneFromGamma(final.xyz * 0.6f);
	
	return final;
}

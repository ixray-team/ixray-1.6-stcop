#include "common.hlsli"

uniform float4 ui_emissive_tint;

float4 main(p_bumped_new I) : SV_Target
{
	float3 Color = s_base.Sample(smp_base, I.tcdh.xy).xyz;
    return float4(GammaToLinear(Color) * ui_emissive_tint.xyz, 1.0f);
}


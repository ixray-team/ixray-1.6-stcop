#include "common.hlsli"

float4 main(p_bumped_new I) : SV_Target
{
	float3 Color = s_base.Sample(smp_base, I.tcdh.xy).xyz;
    return float4(GammaToLinear(Color), 0.0f);
}


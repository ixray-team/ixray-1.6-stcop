#include "common.hlsli"

float4 main(p_bumped_new I) : SV_Target
{
	float3 Color = s_base.Sample(smp_base, I.tcdh.xy).xyz * 9.0f;
    return float4(Color, 0.0f);
}


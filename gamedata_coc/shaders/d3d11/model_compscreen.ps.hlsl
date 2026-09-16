#include "common.hlsli"

uniform	float4 		m_affects;

float4 main(p_bumped_new I) : SV_Target
{
	float3 Color = s_base.Sample(smp_base, I.tcdh.xy).xyz;

    float noise = get_noise(I.tcdh.xy * timers.z) * m_affects.x * m_affects.x * 30;
    Color.r += noise + 0.1;
    Color.g += noise + 0.1;
    Color.b += noise + 0.1;

    return float4(GammaToLinear(Color), 0.0f);
}


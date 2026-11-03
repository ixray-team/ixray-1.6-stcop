#include "common.hlsli"
#include "scope_ui.hlsli"
#include "scope_lens.hlsli"

float ScopePostprocessActive;

struct v2p
{
    float2 tc0: TEXCOORD0;
    float3 tc1: TEXCOORD1;
    float4 c0: COLOR0;
};

float4 main(v2p I, float4 pos2d : SV_POSITION) : SV_Target
{
    if (m_hud_params.y * m_hud_params.a < 0.0001f) {
		return 0.0f;
	}
	
    float4 t_base = s_base.Sample(smp_base, I.tc0);
    float4 t_vp2 = SampleScopeScene(I.tc0, pos2d.xy);
	bool MaterialNightVision = ScopePostprocessActive < 0.5f && m_zoom_deviation.z > 0.0f;
	if (MaterialNightVision || m_affects.x > 0.0f)
	{
		float Exposure = max(s_tonemap.Load(uint3(0, 0, 0)).x, EPS_S);
		float3 DisplayColor = tonemap(max(t_vp2.xyz, 0.0f), Exposure);
		if (MaterialNightVision)
		{
			float Noise = (0.5f + get_noise(I.tc0 * timers.z)) * m_zoom_deviation.w;
			float Gray = dot(DisplayColor, float3(1.0f, 1.0f, 1.0f)) * m_zoom_deviation.z * 7.0f;
			DisplayColor = DisplayColor * (1.0f - m_zoom_deviation.z) + Gray;
			DisplayColor.b += (0.4f + Noise) * m_zoom_deviation.z;
		}
		DisplayColor += get_noise(I.tc0 * timers.z) * m_affects.x * m_affects.x * 30.0f;
		t_vp2.xyz = detonemap(saturate(DisplayColor)) / Exposure;
	}
	t_base.xyz = detonemap(t_base.xyz);
	
	
	t_vp2.xyz = ApplyScopeUI(t_vp2.xyz, pos2d.xy);

	float alpha = m_hud_params.y * m_hud_params.a;
	t_vp2 *= saturate(alpha * 2.0f - 1.0f);

    float3 final = lerp(t_vp2.xyz, t_base.xyz, t_base.a);
    return float4(final.xyz, saturate(alpha * 2.0f));
}


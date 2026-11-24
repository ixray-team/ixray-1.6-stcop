#include "common\common.hlsli"
#include "screens\scope_ui.hlsli"
#include "screens\scope_lens.hlsli"

#include "common\extra_cb.hlsli"
#include "common\post_cb.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0;
    float4 c0 : COLOR0;
    float4 hpos : SV_POSITION;
    float fog : FOG;
};

float4 ps_main(v2p I) : SV_Target
{
    if (m_hud_params.y * m_hud_params.a < 0.0001f) {
		return 0.0f;
	}
	
    float4 t_base = s_base.Sample(smp_base, I.tc0);
    float4 t_vp2 = SampleScopeScene(I.tc0, I.hpos.xy);
	bool MaterialNightVision = ScopePostprocessActive < 0.5f && m_zoom_deviation.z > 0.0f;
	if (MaterialNightVision || m_affects.x > 0.0f)
	{
		float Exposure = SceneExposure(s_tonemap.Load(uint3(0, 0, 0)).x, tonemap_params.w, bloom_params.w);
		float3 DisplayColor = ToneToDisplay(tonemap(max(t_vp2.xyz, 0.0f), Exposure));
		if (MaterialNightVision)
		{
			float Noise = (0.5f + get_noise(I.tc0 * timers.z)) * m_zoom_deviation.w;
			float Gray = dot(DisplayColor, float3(1.0f, 1.0f, 1.0f)) * m_zoom_deviation.z * 7.0f;
			DisplayColor = DisplayColor * (1.0f - m_zoom_deviation.z) + Gray;
			DisplayColor.b += (0.4f + Noise) * m_zoom_deviation.z;
		}
		DisplayColor += get_noise(I.tc0 * timers.z) * m_affects.x * m_affects.x * 30.0f;
		t_vp2.xyz = detonemap(DisplayToTone(saturate(DisplayColor))) / Exposure;
	}
	t_base.xyz = detonemap(DisplayToTone(t_base.xyz));
	
	
	t_vp2.xyz = ApplyScopeUI(t_vp2.xyz, I.hpos.xy);

	float alpha = m_hud_params.y * m_hud_params.a;
	t_vp2 *= saturate(alpha * 2.0f - 1.0f);

    float3 final = lerp(t_vp2.xyz, t_base.xyz, t_base.a);
    return float4(final.xyz, saturate(alpha * 2.0f));
}

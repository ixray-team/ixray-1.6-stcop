#include "common.hlsli"

#include "scope_lens.hlsli"

struct v2p
{
	float2 tc0 : TEXCOORD0; // base
	float4 tc1 : TEXCOORD1;
};

float Random(float2 co)
{
	return 0.5f + frac(sin(dot(co.xy, float2(12.9898f, 78.233f))) * 43758.5453f) * 0.5f;
}

float4 main(v2p I) : COLOR
{
	float4 t_base = tex2D(s_base, I.tc0);
	float2 SceneUV = GetScopeSceneUV(I.tc1);
	float4 t_vp2 = SampleScopeScene(I.tc0, SceneUV);

	float noise = Random(SceneUV * timers.z) * m_zoom_deviation.w;
	float gray = (t_vp2.r + t_vp2.g + t_vp2.b) * m_zoom_deviation.z * 7.0f;
	t_vp2.rgb = t_vp2.rgb * (1.0f - m_zoom_deviation.z) + gray;
	t_vp2.b += (0.4f + noise) * m_zoom_deviation.z;

	float Interference = m_affects.x * m_affects.x;
	t_vp2.rgb += (Random(SceneUV * timers.y) - 0.5f) * Interference * 30.0f;

	float3 final = lerp(t_vp2.rgb, t_base.rgb, t_base.a);
	return float4(final, GetScopeLensAlpha());
}

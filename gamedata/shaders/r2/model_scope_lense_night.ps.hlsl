#include "common.hlsli"
uniform float4 m_hud_params;
#include "pnv.hlsli"
#include "scope_lens.hlsli"

struct v2p
{
	float2 tc0 : TEXCOORD0; // base
	float4 tc1 : TEXCOORD1;
};

float4 main(v2p I) : COLOR
{
	float4 t_base = tex2D(s_base, I.tc0);
	float2 SceneUV = GetScopeSceneUV(I.tc1);
	float4 t_vp2 = SampleScopeScene(I.tc0, SceneUV);

	t_vp2.rgb = calc_night_vision_effect(SceneUV, t_vp2, float3(1.0f, 2.0f, 1.0f)).rgb;

	float3 final = lerp(t_vp2.rgb, t_base.rgb, t_base.a);
	return float4(final, GetScopeLensAlpha());
}

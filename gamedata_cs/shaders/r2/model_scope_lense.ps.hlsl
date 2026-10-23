#include "common.hlsli"
uniform float4 m_hud_params;
uniform float4 m_zoom_deviation;
#include "scope_lens.hlsli"

struct v2p
{
	float2 tc0 : TEXCOORD0; // base
	float4 tc1 : TEXCOORD1;
};

float4 main(v2p I) : COLOR
{
	float4 t_base = tex2D(s_base, I.tc0);
	float4 t_vp2 = SampleScopeScene(I.tc0, GetScopeSceneUV(I.tc1));

	float3 final = lerp(t_vp2.rgb, t_base.rgb, t_base.a);
	return float4(final, GetScopeLensAlpha());
}

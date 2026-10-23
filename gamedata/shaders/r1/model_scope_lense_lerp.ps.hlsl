#include "common.hlsli"
#include "scope_lens.hlsli"

struct v2p
{
	float2 tc0 : TEXCOORD0; // base
	float4 tc1 : TEXCOORD1;
};

float4 main(v2p I) : COLOR
{
	float2 coords = I.tc0;
	coords.x *= 0.5f;
	float4 base2 = tex2D(s_base, coords);
	coords.x += 0.5f;
	float4 t_base = tex2D(s_base, coords);
	t_base = lerp(base2, t_base, m_zoom_deviation.z);

	float4 t_vp2 = SampleScopeScene(I.tc0, GetScopeSceneUV(I.tc1));

	float3 final = lerp(t_vp2.rgb, t_base.rgb, t_base.a);
	return float4(final, GetScopeLensAlpha());
}

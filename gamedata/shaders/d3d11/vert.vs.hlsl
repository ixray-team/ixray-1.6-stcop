#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
float2 tc0 : TEXCOORD0;
	float3 c0 : COLOR0;
	float fog : TEXCOORD1;
	float4 hpos : SV_POSITION;
};

void main(PULL_INPUT, out vf o)
{
	PulledVertex V = PullVertex(VertexId);
	float3 N = V.N;
	o.hpos = mul(m_WVP, float4(V.P, 1.0f));
	o.tc0 = V.UV;

	float3 L_rgb = DecodeU8x4(V.X).xyz;
	float3 L_hemi = r1_v_hemi(N) * DecodeU8x4(V.C).x;
	float3 L_sun = r1_v_sun(N) * DecodeU8x4(V.C).y;
	o.c0 = L_rgb + L_hemi + L_sun + L_ambient.xyz;
	o.fog = r1_fog(mul(m_W, float4(V.P, 1.0f)));
}

#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
float2 tc0 : TEXCOORD0;
	float2 tc1 : TEXCOORD1;
	float2 tch : TEXCOORD2;
	float3 c0 : COLOR0;
	float3 c1 : COLOR1;
	float fog : TEXCOORD3;
	float4 hpos : SV_POSITION;
};

void main(PULL_INPUT, out vf o)
{
	PulledVertex V = PullVertex(VertexId);
	float3 N = V.N;
	o.hpos = mul(m_WVP, float4(V.P, 1.0f));
	o.tc0 = V.UV;
	o.tc1 = unpack_tc_lmap(DecodeS16x2(V.X));
	o.tch = o.tc1;
	o.c0 = r1_v_hemi(N);
	o.c1 = r1_v_sun(N);
	o.fog = r1_fog(mul(m_W, float4(V.P, 1.0f)));
}

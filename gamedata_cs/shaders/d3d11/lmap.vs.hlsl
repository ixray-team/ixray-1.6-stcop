#define USE_LM_HEMI
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

void main(in v_static v, out vf o)
{
	float3 N = unpack_normal(v.Nh.xyz);
	o.hpos = mul(m_WVP, v.P);
	o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w);
	o.tc1 = unpack_tc_lmap(v.lmh);
	o.tch = o.tc1;
	o.c0 = r1_v_hemi(N);
	o.c1 = r1_v_sun(N);
	o.fog = r1_fog(mul(m_W, v.P));
}

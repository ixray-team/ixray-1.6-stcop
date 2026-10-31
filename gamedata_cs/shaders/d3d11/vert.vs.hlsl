#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
float2 tc0 : TEXCOORD0;
	float3 c0 : COLOR0;
	float fog : TEXCOORD1;
	float4 hpos : SV_POSITION;
};

void main(in v_static_color v, out vf o)
{
	float3 N = unpack_normal(v.Nh.xyz);
	o.hpos = mul(m_WVP, v.P);
	o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w);

	float3 L_rgb = unpack_D3DCOLOR(v.color).xyz;
	float3 L_hemi = r1_v_hemi(N) * unpack_D3DCOLOR(v.Nh).w;
	float3 L_sun = r1_v_sun(N) * unpack_D3DCOLOR(v.color).w;
	o.c0 = L_rgb + L_hemi + L_sun + L_ambient.xyz;
	o.fog = r1_fog(mul(m_W, v.P));
}

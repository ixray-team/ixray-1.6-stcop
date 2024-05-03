#include "common.h"

cbuffer LodConstants {
	float3x4 m_xform;
	float3x4 m_xform_v;

	float4 consts;
	float4 wind;
	float4 wave;
	
	float4 consts_old;
	float4 wave_old;
	float4 wind_old;
	
	float4 c_scale;
	float4 c_bias;

	float2 c_sun;
}

void main(in v_tree I, out p_bumped_new O)
{	
	float4 pos = float4(mul(m_xform, I.P).xyz, 1.0);
	float4 pos_old = pos;
	
	float2 tc = I.tc.xy * consts.xy;
	float sun = I.Nh.w * c_sun.x + c_sun.y;
	float hemi = I.Nh.w * c_scale.w + c_bias.w;
	
#ifdef USE_TREEWAVE
	float base = m_xform._24;
	float H = pos.y - base;
	
	float dp = calc_cyclic(wave.w + dot(pos, wave.xyz));
	float frac = I.tc.z * consts.x;
	float inten = H * dp;
	
	pos.xz += calc_xz_wave(wind.xz * inten, frac);
	
	float dp_old = calc_cyclic(wave_old.w + dot(pos, wave_old.xyz));
	float frac_old = I.tc.z * consts_old.x;
	float inten_old = H * dp_old;
	
	pos_old.xz += calc_xz_wave(wind_old.xz * inten_old, frac_old);
#endif
	
	float3 Pe = mul(m_V, pos);

	O.tcdh = float4(tc.xy, hemi, sun);
	O.position = float4(Pe, 1.0f);

	float3 N = unpack_bx4(unpack_D3DCOLOR(I.Nh));
	float3 T = unpack_bx4(unpack_D3DCOLOR(I.T));
	float3 B = unpack_bx4(unpack_D3DCOLOR(I.B));
	
	float3x3 xform = mul((float3x3)m_xform_v, float3x3(
		T.x, B.x, N.x,
		T.y, B.y, N.y,
		T.z, B.z, N.z
	));
	
	O.M1 = xform[0]; 
	O.M2 = xform[1]; 
	O.M3 = xform[2];
	
	O.hpos = mul(m_VP, pos);
	
	O.hpos_curr = mul(m_VP, pos);
	O.hpos_old = mul(m_VP_old, pos_old);
}
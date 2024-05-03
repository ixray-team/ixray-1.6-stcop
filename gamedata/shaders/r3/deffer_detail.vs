#include "common.h"

cbuffer DetailConstants {
float4 consts;

float4 wave;
float4 wave_old;

float4 dir2D;
float4 dir2D_old; 

float4 array[61*4];
}

void main(in v_detail I, out p_bumped_new O)
{
	int i = I.misc.w;
	float4 m0 = array[i + 0];
	float4 m1 = array[i + 1];
	float4 m2 = array[i + 2];
	float4 c0 = array[i + 3];

	float4 	pos, pos_old;
 	pos.x = dot	(m0, I.pos);
 	pos.y = dot	(m1, I.pos);
 	pos.z = dot	(m2, I.pos);
	pos.w = 1.0f;
	pos_old = pos;

	float base = m1.w;
	float H = pos.y - base;
	float frac = I.misc.z * consts.x;
	
	float dp = calc_cyclic(dot(pos, wave));
	float inten = H * dp;
	float2 result = calc_xz_wave(dir2D.xz * inten, frac);
	
	pos.xz += result;
	
	float dp_old = calc_cyclic(dot(pos, wave_old));
	float inten_old = H * dp_old;
	float2 result_old = calc_xz_wave(dir2D_old.xz * inten_old, frac);
	
	pos_old.xz += result;

	float3 Pe = mul(m_WV, pos);
	float2 tc = I.misc.xy * consts.xy;
	
	float3 N;
	N.x = pos.x - m0.w;
	N.y = pos.y - m1.w + 0.75f;
	N.z = pos.z - m2.w;

	O.tcdh = float4(tc.xy, c0.w, c0.x);
	O.position = float4(Pe, 1.0f);
	
	float3x3 xform = mul((float3x3)m_WV, float3x3(
		0.0f, 0.0f, N.x,
		0.0f, 0.0f, N.y,
		0.0f, 0.0f, N.z
	));
	
	O.M1 = xform[0]; 
	O.M2 = xform[1]; 
	O.M3 = xform[2];
	
	O.hpos = mul(m_WVP, pos);
	
	O.hpos_curr = mul(m_VP, pos);
	O.hpos_old = mul(m_VP_old, pos_old);
}


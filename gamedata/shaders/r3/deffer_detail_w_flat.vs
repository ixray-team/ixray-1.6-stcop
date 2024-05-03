#include "common.h"

uniform float4 		consts; // {1/quant,1/quant,diffusescale,ambient}

uniform float4 		wave, wave_old; 	// cx,cy,cz,tm
uniform float4 		dir2D, dir2D_old; 
//uniform float4 		array	[200] : register(c12);
//tbuffer DetailsData
//{
	uniform float4 		array[61*4];
//}

v2p_flat 	main (v_detail v)
{
	v2p_flat 		O;
	// index
	int 	i 	= v.misc.w;
	float4  m0 	= array[i+0];
	float4  m1 	= array[i+1];
	float4  m2 	= array[i+2];
	float4  c0 	= array[i+3];

	// Transform pos to world coords
	float4 	pos;
 	pos.x 		= dot	(m0, v.pos);
 	pos.y 		= dot	(m1, v.pos);
 	pos.z 		= dot	(m2, v.pos);
	pos.w 		= 1;

	float 	base = m1.w;
	float 	H = pos.y - base; // height of vertex (scaled)
	float 	frac = v.misc.z * consts.x; // fractional
	
	float 	dp	= calc_cyclic   (dot(pos,wave));
	float 	inten 	= H * dp;
	float2 	result	= calc_xz_wave	(dir2D.xz * inten, frac);
	
	float 	dp_old	= calc_cyclic (dot(pos,wave_old));
	float 	inten_old 	= H * dp_old;
	float2 	result_old	= calc_xz_wave	(dir2D_old.xz * inten_old, frac);
	
	float4 o_pos = float4(pos.x+result_old.x, pos.y, pos.z+result_old.y, 1);
	float4 f_pos = float4(pos.x+result.x, pos.y, pos.z+result.y, 1);

	// Normal in world coords
	float3 	norm;	//	= float3(0,1,0);
		norm.x 	= f_pos.x - m0.w;
		norm.y 	= f_pos.y - m1.w + .75f;	// avoid zero
		norm.z	= f_pos.z - m2.w;

	// Final out
	O.hpos 		= mul		(m_WVP,	f_pos);
	float3	Pe	= mul		(m_WV,  f_pos);
	O.N 		= mul		(m_WV,  normalize(norm));
	
	O.tcdh 		= float4	((v.misc * consts).xyyy );

# if defined(USE_R2_STATIC_SUN)
	O.tcdh.w	= c0.x;								// (,,,dir-occlusion)
# endif
	O.hpos_curr = mul (m_VP, f_pos);
	O.hpos_old = mul (m_VP_old, o_pos);
	
	O.position	= float4	(Pe, 		c0.w		);

	return O;
}
FXVS;

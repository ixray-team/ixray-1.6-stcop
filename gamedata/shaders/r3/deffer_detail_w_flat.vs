#include "common.h"

uniform float4 		consts; // {1/quant,1/quant,diffusescale,ambient}
uniform float4 		wave; 	// cx,cy,cz,tm
uniform float4 		prev_wave; 	// cx,cy,cz,tm
uniform float4 		dir2D; 
uniform float4 		prev_dir2D; 

uniform float4 		array[61*4];

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
	float4 prev_pos;
 	pos.x 		= dot	(m0, v.pos);
 	pos.y 		= dot	(m1, v.pos);
 	pos.z 		= dot	(m2, v.pos);
	pos.w 		= 1;

	// 
	float 	base 		= m1.w;
	float 	dp			= calc_cyclic   (dot(pos,wave));
	float 	H 			= pos.y - base;			// height of vertex (scaled)
	float 	inten 		= H * dp;
	float 	frac 		= v.misc.z*consts.x;		// fractional
	float2 	result 		= calc_xz_wave	(dir2D.xz*inten,frac);
	pos					= float4(pos.x+result.x, pos.y, pos.z+result.y, 1);

	float 	prev_dp		= calc_cyclic   (dot(pos,prev_wave));
	float 	prev_H 		= pos.y - base;			// height of vertex (scaled)
	float 	prev_inten 	= prev_H * prev_dp;
	float 	prev_frac 	= v.misc.z*consts.x;		// fractional
	float2 	prev_result = calc_xz_wave	(prev_dir2D.xz*prev_inten,prev_frac);
	prev_pos			= float4(pos.x+prev_result.x, pos.y, pos.z+prev_result.y, 1);

	// Normal in world coords
	float3 	norm;	//	= float3(0,1,0);
		norm.x 	= pos.x - m0.w	;
		norm.y 	= pos.y - m1.w	+ .75f;	// avoid zero
		norm.z	= pos.z - m2.w	;

	// Final out
	O.hpos 		= mul		(m_VP,	pos				);	
	O.cur_hpos	= mul		(m_VP_Unjittered,		float4(pos.x, pos.y, pos.z, 1.0f)			);	
	O.prev_hpos	= mul 		(m_prevVP_Unjittered, 	float4(prev_pos.x, prev_pos.y, prev_pos.z, 1.0f) 	);
	O.N 		= mul		(m_WV,  normalize(norm)	);
	float3	Pe	= mul		(m_WV,  pos				);
//	O.tcdh 		= float4	((v.misc * consts).xy	);
	O.tcdh 		= float4	((v.misc * consts).xyyy );

# if defined(USE_R2_STATIC_SUN)
	O.tcdh.w	= c0.x;								// (,,,dir-occlusion)
# endif
	O.position	= float4	(Pe, 		c0.w		);

	return O;
}
FXVS;

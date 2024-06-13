#include "common.h"

struct	v_vert
{
	float4 	pos	: POSITION;	// (float,float,float,1)
	float4	color	: COLOR0;	// (r,g,b,dir-occlusion)
};
struct 	v2p
{
	float4 c	: COLOR0;
	float  fog	: FOG;
	float4 hpos	: SV_Position;
};

v2p main (v_vert v)
{
	v2p o;

	o.hpos = mul(m_VP, v.pos);				// xform, input in world coords
	o.c = v.color;
	o.fog = calc_fogging(v.pos);			// fog, input in world coords
	o.c.rgb = lerp(o.c, fog_color, o.fog);
	o.c.a *= 1.0f - o.fog * o.fog;
	o.fog = 1.0f - o.fog;
	
	o.hpos.xy += m_taa_jitter.xy * o.hpos.w;

	return o;
}
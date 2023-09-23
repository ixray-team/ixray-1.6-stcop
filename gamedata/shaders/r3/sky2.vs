#include "common.h"

struct vi
{
	float4	p		: POSITION;
	float4	c		: COLOR0;
	float3	tc0		: TEXCOORD0;
	float3	tc1		: TEXCOORD1;
};

struct v2p
{
	float4	c		: COLOR0;
	float3	tc0		: TEXCOORD0;
	float3	tc1		: TEXCOORD1;
	float4	hpos	: SV_Position;
  	float4  prev_hpos : TEXCOORD2;
  	float4  cur_hpos  : TEXCOORD3;
};

v2p main (vi v)
{
	v2p		o;

	float4	tpos	= mul(1000, v.p);
     o.hpos         = mul(m_WVP, tpos);						// xform, input in world coords, 1000 - magic number
	o.hpos.z	    = o.hpos.w;
	o.cur_hpos		= mul(m_VP_Unjittered, tpos);
	o.prev_hpos		= mul(m_prevVP_Unjittered, tpos);
	o.cur_hpos.z 	= o.cur_hpos.w;
	o.prev_hpos.z 	= o.prev_hpos.w;
	o.tc0			= v.tc0;                        					// copy tc
	o.tc1			= v.tc1;                        					// copy tc
	float	scale	= s_tonemap.Load( int3(0,0,0) ).x;
    o.c				= float4	( v.c.rgb, v.c.a );      		// copy color, pre-scale by tonemap //float4 ( v.c.rgb*scale*2, v.c.a );

	return	o;
}
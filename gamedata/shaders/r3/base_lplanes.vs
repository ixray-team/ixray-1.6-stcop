#include "common.h"

struct vf
{
	float2 tc0	: TEXCOORD0;		// base
	float4 c0	: COLOR0;		// color
	float4 hpos	: SV_Position;
	float4 prev_hpos : POSITION0;
	float4 cur_hpos : POSITION1;
};

vf main (v_static v)
{
	vf 		o;

	float4 pos = v.P;;
	o.hpos 		= mul			(m_WVP, pos);		// xform, input in world coords
	o.cur_hpos	= mul			(m_WVP_Unjittered, float4(pos.x, pos.y, pos.z, 1.0f));
	o.prev_hpos = mul			(m_prevWVP_Unjittered, float4(pos.x, pos.y, pos.z, 1.0f));	
	o.tc0		= unpack_tc_base(v.tc,v.T.w,v.B.w);	// copy tc

	// calculate fade
	float3  dir_v 	= normalize	(mul(m_WV,v.P));
	float3 	norm_v 	= normalize (mul(m_WV,unpack_normal(v.Nh).zyx));
	float 	fade 	= abs		(dot(dir_v,norm_v));
	o.c0		= fade;

	return o;
}

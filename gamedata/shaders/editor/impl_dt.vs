#include "common.h"

struct vf
{
	float4 hpos	: POSITION;
	float2 tc0	: TEXCOORD0;
	float2 tc1	: TEXCOORD1;
	float2 tc2	: TEXCOORD2;
	float  fog	: FOG;
};
struct vv
{
	float4 P	: POSITION;
	float2 tc	: TEXCOORD0;
	float3 N	: NORMAL;
};
vf main (vv v)
{
	vf 		o;
	float2 	dt 	= calc_detail		(v.P);
	float3	N 	= unpack_normal		(v.N);
	o.hpos 		= mul			(m_WVP, float4(v.P.xyz,1));			// xform, input in world coords
	o.tc0		= v.tc;
	o.tc1		= o.tc0;						// copy tc 
	o.tc2		= o.tc0*dt_params;					// dt tc
	o.fog 		= calc_fogging 		(v.P);			// fog, input in world coords

	return o;
}

#include "common.h"
#include "skin.h"

// #define SKIN_2

struct vf
{
	float4 hpos	: POSITION;
	float2 tc0	: TEXCOORD0;		// base
	float  fog	: FOG;
};

vf 	_main (v_model v)
{
	vf 		o;
	o.hpos 		= mul			(m_WVP,v.pos);		// xform, input in world coords
	o.tc0		= v.tc.xy;					// copy tc
	o.fog 		= calc_fogging (v.pos);
	return o;
}

/////////////////////////////////////////////////////////////////////////
#define SKIN_VF vf
#include "skin_main.h"
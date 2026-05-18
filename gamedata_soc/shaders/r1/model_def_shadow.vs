#include "common.h"
#include "skin.h"

struct vf
{
	float4	hpos	: POSITION;
	float4 	c0	: COLOR0;		// color
};

vf 	_main 	(v_model v)
{
	vf 		o;

	o.hpos 		= mul	(m_WVP, v.pos);	// xform, input in world coords
	o.c0 		= 0;
	return o;
}

/////////////////////////////////////////////////////////////////////////
#ifdef 	SKIN_NONE
vf	main(v_model v) 		{ return _main(v); 		}
#endif

#ifdef 	SKIN_0
vf	main(v_model_skinned_0 v) 	{ return _main(skinning_0(v)); }
#endif

#ifdef	SKIN_1
vf	main(v_model_skinned_1 v) 	{ return _main(skinning_1(v)); }
#endif

#ifdef	SKIN_2
vf	main(v_model_skinned_2 v) 	{ return _main(skinning_2lq(v)); }
#endif

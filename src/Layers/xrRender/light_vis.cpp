#include "StdAfx.h"
#include "light.h"

const	u32	delay_small_min			= 1;
const	u32	delay_small_max			= 3;
const	u32	delay_large_min			= 10;
const	u32	delay_large_max			= 20;
const	u32	cullfragments			= 4;

void	light::vis_prepare			()
{
	//	. test is sheduled for future	= keep old result
	//	. test time comes :)
	//		. camera inside light volume	= visible,	shedule for 'small' interval
	//		. perform testing				= ???,		pending

	u32	frame	= Device.dwFrame;
	if (frame	<	vis.frame2test)		return;
	PROF_EVENT("light::vis_prepare")
	bool	skiptest	= vis.camerainbounds;
	if (ps_r2_ls_flags.test(R2FLAG_EXP_DONT_TEST_UNSHADOWED) && !flags.bShadow)	skiptest=true;
	if (ps_r2_ls_flags.test(R2FLAG_EXP_DONT_TEST_SHADOWED) && flags.bShadow)	skiptest=true;

	if (skiptest)	{	// small error
		vis.visible		=	true;
		vis.pending		=	false;
		vis.frame2test	=	frame	+ ::Random.randI(delay_small_min,delay_small_max);
		return;
	}

	// testing
	vis.pending										= true;
	RCache.set_xform_world							(m_xform);
	vis.query_order	= RImplementation.occq_begin	(vis.query_id);
	//	Hack: Igor. Light is visible if it's frutum is visible. (Only for volumetric)
	//	Hope it won't slow down too much since there's not too much volumetric lights
	//	TODO: sort for performance improvement if this technique hurts
	if ( (flags.type==IRender_Light::SPOT||flags.type==IRender_Light::OMNIPART) && flags.bShadow && flags.bVolumetric )
		RCache.set_Stencil			(FALSE);
	else
		RCache.set_Stencil			(TRUE,D3DCMP_LESSEQUAL,0x01,0xff,0x00);
	RImplementation.Target->draw_volume				(this);
	RImplementation.occq_end						(vis.query_id);
}

void	light::vis_update			()
{
	//	. not pending	->>> return (early out)
	//	. test-result:	visible:
	//		. shedule for 'large' interval
	//	. test-result:	invisible:
	//		. shedule for 'next-frame' interval

	if (!vis.pending)	return;
	PROF_EVENT("light::vis_update")
	u32	frame			= Device.dwFrame;

	R_occlusion::occq_result fragments = RImplementation.occq_get	(vis.query_id);

	vis.visible			= (fragments > cullfragments);
	vis.pending			= false;
	if (vis.visible)	
		vis.frame2test	=	frame	+ ::Random.randI(delay_large_min,delay_large_max);
	else 
		vis.frame2test	=	frame	+ 1; 
}

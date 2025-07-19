#include "StdAfx.h"
#include "light.h"
#include "QueryHelper.h"
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
	if (!flags.bActive)
	{
		vis.visible = false;
		vis.pending = false;
		return;
	}
	if (!flags.bOccq || flags.bHudMode)
	{
		vis.visible = true;
		vis.pending = false;
		return;
	}

	u32	frame	= Device.dwFrame;
	if (frame	<	vis.frame2test)		return;

	float safe_area = Device.fViewportNear;
	{
		float a0 = deg2rad(Device.fFOV*Device.fASPECT*0.5f);
		float a1 = deg2rad(Device.fFOV*0.5f);
		float x0 = Device.fViewportNear/_cos(a0);
		float x1 = Device.fViewportNear/_cos(a1);
		float c	= _sqrt(x0*x0 + x1*x1);
		safe_area = _max(_max(Device.fViewportNear,_max(x0,x1)),c);
	}

	bool	skiptest	= false;
	if (ps_r2_ls_flags.test(R2FLAG_EXP_DONT_TEST_UNSHADOWED) && !flags.bShadow)	skiptest=true;
	if (ps_r2_ls_flags.test(R2FLAG_EXP_DONT_TEST_SHADOWED) && flags.bShadow)	skiptest=true;

	if (skiptest || Device.vCameraPosition.distance_to(SpatialComponent->spatial.sphere.P)<=(SpatialComponent->spatial.sphere.R*1.01f+safe_area + (SpatialComponent->spatial.sphere.R * 0.1f)))	
	{	
		// small error
		vis.visible		=	true;
		vis.pending		=	false;
		vis.frame2test	=	frame	+ ::Random.randI(delay_small_min,delay_small_max);
		return;
	}

	// testing
	vis.pending										= true;
	RCache.set_xform_world							(m_xform);
	CHK_DX(BeginQuery(vis.Q));
	//	Hack: Igor. Light is visible if it's frutum is visible. (Only for volumetric)
	//	Hope it won't slow down too much since there's not too much volumetric lights
	//	TODO: sort for performance improvement if this technique hurts

	if ( (flags.type==IRender_Light::SPOT) && flags.bShadow && flags.bVolumetric )
		RCache.set_Stencil			(FALSE);
	else
		RCache.set_Stencil			(TRUE,D3DCMP_LESSEQUAL,0x01,0xff,0x00);
	RImplementation.Target->draw_volume				(this);
	CHK_DX(EndQuery(vis.Q));
}

void	light::vis_update			()
{
	PROF_EVENT("vis_update");
	//	. not pending	->>> return (early out)
	//	. test-result:	visible:
	//		. shedule for 'large' interval
	//	. test-result:	invisible:
	//		. shedule for 'next-frame' interval

	if (!vis.pending)	return;
	u32	frame			= Device.dwFrame;
	CTimer T;
	T.Start();
	R_occlusion::occq_result fragments = 0;
	HRESULT hr;
	while (hr = GetData(vis.Q , &fragments, sizeof(fragments), 0x1 /*D3D11_ASYNC_GETDATA_DONOTFLUSH*/) == S_FALSE)
	{
		if (hr == D3DERR_DEVICELOST || T.GetElapsed_ms_f() > 0.5f)
		{
			fragments = R_occlusion::occq_result(-1);
			break;
		}
	}

	vis.visible = (fragments > cullfragments);
	vis.pending = false;
	if (vis.visible)	
		vis.frame2test	=	frame + ::Random.randI(delay_large_min,delay_large_max);
	else 
		vis.frame2test	=	frame + 1; 
}

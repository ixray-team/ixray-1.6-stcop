#include "stdafx.h"

static ClearData Data;

void	CRenderTarget::phase_accumulator()
{
	// Targets
	if (dwAccumulatorClearMark==Device.dwFrame)	{
		// normal operation - setup
		if (RImplementation.o.fp16_blend)
			u_setrt(rt_Accumulator, nullptr, nullptr, RDepth);
		else
			u_setrt(rt_Accumulator_temp, nullptr, nullptr, RDepth);
	} else {
		// initial setup
		dwAccumulatorClearMark				= Device.dwFrame;

		// clear
		u_setrt(rt_Accumulator, nullptr, nullptr, RDepth);
		//dwLightMarkerID						= 5;					// start from 5, increment in 2 units
		reset_light_marker();

		Data.Color.set(0.0f, 0.0f, 0.0f, 0.0f);
		g_RenderRHI->Clear(ERHIClearStage::eClearTarget, rt_Accumulator->pRT, Data);

		// Stencil	- draw only where stencil >= 0x1
		RCache.set_Stencil					(TRUE,D3DCMP_LESSEQUAL,0x01,0xff,0x00);
		RCache.set_CullMode					(CULL_NONE);
		RCache.set_ColorWriteEnable			();
		
	}

	//	Restore viewport after shadow map rendering
	RImplementation.rmNormal();
}

void	CRenderTarget::phase_vol_accumulator()
{
	if (!m_bHasActiveVolumetric)
	{
		m_bHasActiveVolumetric = true;
		u_setrt(rt_Generic_2, nullptr, nullptr, RDepth);
		Data.Color.set(0.0f, 0.0f, 0.0f, 0.0f);
		g_RenderRHI->Clear(ERHIClearStage::eClearTarget, rt_Generic_2->pRT, Data);
	}
	else
	{
		u_setrt(rt_Generic_2, nullptr, nullptr, RDepth);
	}

	RCache.set_Stencil							(FALSE);
	RCache.set_CullMode							(CULL_NONE);
	RCache.set_ColorWriteEnable					();
}
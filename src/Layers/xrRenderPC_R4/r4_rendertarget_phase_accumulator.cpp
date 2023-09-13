#include "stdafx.h"

void	CRenderTarget::phase_accumulator()
{
	// Targets
	if (dwAccumulatorClearMark==Device.dwFrame)	{
		// normal operation - setup
		if (RImplementation.o.fp16_blend)	u_setrt	(rt_Accumulator,		NULL,NULL, rt_HWDepth->pZRT);
		else								u_setrt	(rt_Accumulator_temp,	NULL,NULL, rt_HWDepth->pZRT);
	} else {
		// initial setup
		dwAccumulatorClearMark				= Device.dwFrame;

		// clear
   		u_setrt								(rt_Accumulator,		NULL,NULL, rt_HWDepth->pZRT);

		reset_light_marker();

		FLOAT ColorRGBA[4] = {0.0f, 0.0f, 0.0f, 0.0f};
		HW.pContext->ClearRenderTargetView( rt_Accumulator->pRT, ColorRGBA);
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
		u_setrt								(rt_Generic_2,		NULL,NULL, rt_HWDepth->pZRT);
		//u32		clr4clearVol				= color_rgba(0,0,0,0);	// 0x00
		//CHK_DX	(HW.pDevice->Clear			( 0L, NULL, D3DCLEAR_TARGET, clr4clearVol, 1.0f, 0L));
		FLOAT ColorRGBA[4] = {0.0f, 0.0f, 0.0f, 0.0f};
		HW.pContext->ClearRenderTargetView( rt_Generic_2->pRT, ColorRGBA);
	}
	else
	{
		u_setrt								(rt_Generic_2,		NULL,NULL, rt_HWDepth->pZRT);
	}

	RCache.set_Stencil							(FALSE);
	RCache.set_CullMode							(CULL_NONE);
	RCache.set_ColorWriteEnable					();
}
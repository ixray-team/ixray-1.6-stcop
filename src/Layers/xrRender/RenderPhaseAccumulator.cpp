#include "stdafx.h"

void CRenderTarget::phase_accumulator()
{
	// Targets
	if (dwAccumulatorClearMark == Device.dwFrame)
	{
		// normal operation - setup
		u_setrt(rt_Accumulator, nullptr, nullptr, RDepth);
	}
	else
	{
		// initial setup
		dwAccumulatorClearMark = Device.dwFrame;

		// clear
		u_setrt(rt_Accumulator, nullptr, nullptr, RDepth);
		reset_light_marker();
		GRHI->ClearTarget(rt_Accumulator->pRT);

		// Stencil	- draw only where stencil >= 0x1
		RCache.set_Stencil(TRUE, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);
		RCache.set_CullMode(CULL_NONE);
		RCache.set_ColorWriteEnable();
	}

#ifdef USE_DX11
	// Restore viewport after shadow map rendering
	RImplementation.rmNormal();
#endif
}

void CRenderTarget::phase_vol_accumulator()
{
	if (!m_bHasActiveVolumetric)
	{
		m_bHasActiveVolumetric = true;
		u_setrt(rt_Generic_2, nullptr, nullptr, RDepth);
		GRHI->ClearTarget(rt_Generic_2->pRT);
	}
	else
	{
		u_setrt(rt_Generic_2, nullptr, nullptr, RDepth);
	}

	RCache.set_Stencil(FALSE);
	RCache.set_CullMode(CULL_NONE);
	RCache.set_ColorWriteEnable();
}
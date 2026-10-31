#include "stdafx.h"

void CRenderTarget::phase_accumulator(GfxCmdList& Cmd)
{
	// Targets
	if (dwAccumulatorClearMark == Device.dwFrame)
	{
		// normal operation - setup
		u_setrt(Cmd, rt_Accumulator, nullptr, nullptr, RDepth);
	}
	else
	{
		// initial setup
		dwAccumulatorClearMark = Device.dwFrame;

		// clear
		u_setrt(Cmd, rt_Accumulator, nullptr, nullptr, RDepth);
		reset_light_marker(Cmd);
		Cmd.ClearTarget(rt_Accumulator->pRT);

		// Stencil	- draw only where stencil >= 0x1
		Cmd.SetStencil(true, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
		Cmd.SetColorWriteEnable();
	}

	// Restore viewport after shadow map rendering
	RImplementation.rmNormal();
}

void CRenderTarget::phase_vol_accumulator(GfxCmdList& Cmd)
{
	if (!m_bHasActiveVolumetric)
	{
		m_bHasActiveVolumetric = true;
		u_setrt(Cmd, rt_Generic_2, nullptr, nullptr, RDepth);
		Cmd.ClearTarget(rt_Generic_2->pRT);
	}
	else
	{
		u_setrt(Cmd, rt_Generic_2, nullptr, nullptr, RDepth);
	}

	Cmd.SetStencil(false);
	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetColorWriteEnable();
}
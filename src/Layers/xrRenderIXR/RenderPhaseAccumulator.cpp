#include "stdafx.h"

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
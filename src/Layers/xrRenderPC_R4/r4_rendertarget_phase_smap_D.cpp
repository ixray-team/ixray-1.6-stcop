#include "stdafx.h"

void CRenderTarget::phase_smap_direct(GfxCmdList& Cmd, light* L, u32 sub_phase)
{
	u_setrt(Cmd, nullptr, nullptr, nullptr, rt_smap_depth->pZRT);
	Cmd.ClearDepthStencil(rt_smap_depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
	
	//	Prepare viewport for shadow map rendering
	if(sub_phase != SE_SUN_RAIN_SMAP)
	{
		RImplementation.rmNormal();
	}
	else
	{
		RHIViewport VP = 
		{
			(float)L->X.D.minX,(float)L->X.D.minY,
			(float)(L->X.D.maxX - L->X.D.minX),
			(float)(L->X.D.maxY - L->X.D.minY),
			0.0f, 1.0f
		};

		Cmd.SetViewport(VP);
	}

	// Stencil	- disable
	Cmd.SetStencil(false);
}

void CRenderTarget::phase_smap_direct_tsh(GfxCmdList& Cmd, light* L, u32 sub_phase)
{
	Cmd.SetColorWriteEnable();
	RImplementation.rmNormal();

	Cmd.ClearTarget(Cmd.GetRenderTarget(), ERTColor::Black);
}

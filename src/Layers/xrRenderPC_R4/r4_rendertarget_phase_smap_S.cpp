#include "stdafx.h"

void CRenderTarget::phase_smap_spot_clear()
{
	RImplementation.Imm().ClearTarget(rt_smap_surf->pRT, ERTColor::Black);
	RImplementation.Imm().ClearDepthStencil(rt_smap_depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
}

void CRenderTarget::phase_smap_spot(GfxCmdList& Cmd, light* L)
{
	// Targets + viewport
	u_setrt(Cmd, rt_smap_surf, nullptr, nullptr, rt_smap_depth->pZRT);

	RHIViewport VP = {(float)L->X.S.posX, (float)L->X.S.posY, (float)L->X.S.size, (float)L->X.S.size, 0, 1};
	Cmd.SetViewport(VP);

	// Misc - draw only front-faces //back-faces
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
	Cmd.SetStencil(false);

	// no transparency
	#pragma todo("can optimize for multi-lights covering more than say 50%...")

	Cmd.SetColorWriteEnable(false);
}

void CRenderTarget::phase_smap_spot_tsh(GfxCmdList& Cmd, light* L)
{
	Cmd.SetColorWriteEnable();
}

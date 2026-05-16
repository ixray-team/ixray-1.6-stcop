#include "stdafx.h"

void CRenderTarget::phase_smap_spot_clear()
{
	GRHI->ClearTarget(rt_smap_surf->pRT, ERTColor::Black);
	GRHI->ClearDepthStencil(rt_smap_depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
}

void CRenderTarget::phase_smap_spot(light* L)
{
	// Targets + viewport
	u_setrt(rt_smap_surf, nullptr, nullptr, rt_smap_depth->pZRT);

	RHIViewport VP = {(float)L->X.S.posX, (float)L->X.S.posY, (float)L->X.S.size, (float)L->X.S.size, 0, 1};
	GRHI->SetViewport(VP);

	// Misc - draw only front-faces //back-faces
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	RCache.set_Stencil(false);

	// no transparency
	#pragma todo("can optimize for multi-lights covering more than say 50%...")

	RCache.set_ColorWriteEnable(false);
}

void CRenderTarget::phase_smap_spot_tsh(light* L)
{
	RCache.set_ColorWriteEnable();
}

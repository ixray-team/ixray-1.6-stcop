#include "stdafx.h"

void CRenderTarget::phase_smap_spot_clear()
{
	GRHI->ClearTarget(rt_smap_surf->pRT, ERTColor::Black);
	GRHI->ClearDepthStencil(rt_smap_depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
}

void CRenderTarget::phase_smap_spot(light* L)
{
	RHIViewport VP = {(float)L->X.S.posX, (float)L->X.S.posY, (float)L->X.S.size, (float)L->X.S.size, 0, 1};
	SetupSpotShadowTarget(rt_smap_depth->pZRT, rt_smap_depth->dwWidth, rt_smap_depth->dwHeight, VP, rt_smap_surf->pRT);
}

void CRenderTarget::SetupSpotShadowTarget(IRHIDepthStencilView* Depth, u32 Width, u32 Height, RHIViewport Viewport, IRHIRenderTargetView* Color)
{
	u_setrt(Width, Height, Color, nullptr, nullptr, Depth);
	GRHI->SetViewport(Viewport);
	RCache.set_Scissor(nullptr);
	GRHI->StateManager->SetDepthEnable(TRUE);
	GRHI->StateManager->SetRenderState(D3DRS_ZWRITEENABLE, TRUE);
	GRHI->StateManager->SetDepthFunc(D3DCMP_LESSEQUAL);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	RCache.set_Stencil(false);
	RCache.set_ColorWriteEnable(false);
}

void CRenderTarget::phase_smap_spot_tsh(light* L)
{
	RCache.set_ColorWriteEnable();
}

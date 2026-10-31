#include "stdafx.h"

void CRenderTarget::phase_smap_spot_clear()
{
	RImplementation.Imm().ClearTarget(rt_smap_surf->pRT, ERTColor::Black);
	RImplementation.Imm().ClearDepthStencil(rt_smap_depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
}

void CRenderTarget::phase_smap_spot(GfxCmdList& Cmd, light* L)
{
	RHIViewport VP = {(float)L->X.S.posX, (float)L->X.S.posY, (float)L->X.S.size, (float)L->X.S.size, 0, 1};
	SetupSpotShadowTarget(Cmd, rt_smap_depth->pZRT, rt_smap_depth->dwWidth, rt_smap_depth->dwHeight, VP, rt_smap_surf->pRT);
}

void CRenderTarget::SetupSpotShadowTarget(GfxCmdList& Cmd, IRHIDepthStencilView* Depth, u32 Width, u32 Height, RHIViewport Viewport, IRHIRenderTargetView* Color)
{
	u_setrt(Cmd, Width, Height, Color, nullptr, nullptr, Depth);
	Cmd.SetViewport(Viewport);
	Cmd.SetScissor(nullptr);
	Cmd.SetDepthEnable(TRUE);
	Cmd.SetDepthWrite(true);
	Cmd.SetDepthFunc(D3DCMP_LESSEQUAL);
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
	Cmd.SetStencil(false);
	Cmd.SetColorWriteEnable(false);
}

void CRenderTarget::phase_smap_spot_tsh(GfxCmdList& Cmd, light* L)
{
	Cmd.SetColorWriteEnable();
}

#include "stdafx.h"

void CRenderTarget::phase_smap_spot_clear(u32 Page)
{
	RImplementation.Imm().ClearDepthStencil(SmapPage(Page), ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
}

void CRenderTarget::phase_smap_spot(GfxCmdList& Cmd, light* L)
{
	RHIViewport VP = {(float)L->X.S.posX, (float)L->X.S.posY, (float)L->X.S.size, (float)L->X.S.size, 0, 1};
	SetupSpotShadowTarget(Cmd, SmapPage(L->vis.smap_ID), RImplementation.o.smapsize, RImplementation.o.smapsize, VP);
}

void CRenderTarget::SetupSpotShadowTarget(GfxCmdList& Cmd, IRHIDepthStencilView* Depth, u32 Width, u32 Height, RHIViewport Viewport)
{
	u_setrt(Cmd, Width, Height, nullptr, nullptr, nullptr, Depth);
	Cmd.SetViewport(Viewport);
	Cmd.SetScissor(nullptr);
	Cmd.SetDepthEnable(TRUE);
	Cmd.SetDepthWrite(true);
	Cmd.SetDepthFunc(D3DCMP_LESSEQUAL);
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
	Cmd.SetStencil(false);
	Cmd.SetColorWriteEnable(false);
}

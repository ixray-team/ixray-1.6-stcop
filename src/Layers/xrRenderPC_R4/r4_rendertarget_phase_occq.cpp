#include "stdafx.h"

void CRenderTarget::phase_occq(GfxCmdList& Cmd)
{
	u_setrt(Cmd, (u32)Cmd.GetWidth(), (u32)Cmd.GetHeight(), nullptr, nullptr, nullptr, RDepth);
	Cmd.SetPass(s_occq);
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
	Cmd.SetStencil(true, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);
	Cmd.SetColorWriteEnable(false);
}
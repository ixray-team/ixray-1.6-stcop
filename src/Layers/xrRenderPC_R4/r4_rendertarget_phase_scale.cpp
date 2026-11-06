#include "stdafx.h"

void CRenderTarget::phase_scale(GfxCmdList& Cmd)
{
	DrawSQ(Cmd, s_scale, rt_Generic, std::min<u32>(ps_r_scale_mode, 1), []
	{
		RImplementation.rmNormal();
	});
}

void CRenderTarget::PhaseEditorScale(GfxCmdList& Cmd)
{
	u_setrt(Cmd, get_target_width(), get_target_height(), rt_BackbufferLUT->pRT, nullptr, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetStencil(FALSE);
	Cmd.SetColorWriteEnable();
	Cmd.SetPass(s_scale->E[1]);
	Cmd.DrawNoInput(3);
}

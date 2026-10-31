#include "stdafx.h"

void CRenderTarget::phase_ssao(GfxCmdList& Cmd)
{
	DrawSQ(Cmd, s_ssao, rt_ssao_temp, 0, [&]
	{
		Cmd.SetStencil(false);
		RImplementation.rmNormal();
	});

	Cmd.SetStencil(false);
}

void CRenderTarget::phase_downsamp(GfxCmdList& Cmd)
{
	u_setrt(Cmd, rt_half_depth, 0, 0, 0);
	Cmd.ClearTarget(rt_half_depth->pRT);

	RImplementation.rmNormal();
	Cmd.SetStencil(false);
	Cmd.SetPass(s_ssao->E[1]);
	Cmd.DrawNoInput(3);
}

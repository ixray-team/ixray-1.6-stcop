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

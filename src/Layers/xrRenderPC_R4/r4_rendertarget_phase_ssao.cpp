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

	u32 w = (u32)Cmd.GetWidth();
	u32 h = (u32)Cmd.GetHeight();

	RImplementation.rmNormal();
	Cmd.SetStencil(false);

	u32 Offset = 0;

	float scale_X = float(w) / float(TEX_jitter);
	float scale_Y = float(h) / float(TEX_jitter);

	FVF::TL* pv = (FVF::TL*)Cmd.Vertex.Lock(4, g_combine->vb_stride, Offset);
	pv->set(-1, 1, 0, 1, 0, 0, scale_Y);	pv++;
	pv->set(-1, -1, 0, 0, 0, 0, 0);	pv++;
	pv->set(1, 1, 1, 1, 0, scale_X, scale_Y);	pv++;
	pv->set(1, -1, 1, 0, 0, scale_X, 0);	pv++;
	Cmd.Vertex.Unlock(4, g_combine->vb_stride);

	Cmd.SetPass(s_ssao->E[1]);
	Cmd.SetGeometry(g_combine);

	Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 6, 0, Offset);
}

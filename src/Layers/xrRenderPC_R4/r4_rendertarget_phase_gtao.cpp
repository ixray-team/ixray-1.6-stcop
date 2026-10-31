#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_gtao(GfxCmdList& Cmd)
{
	GPU_EVENT(phase_gtao);

	float p_scale = Cmd.GetHeight() / (tan(deg2rad(Device.fFOV) * 0.5f) * 2.0f);
	p_scale *= 0.5;

	{
		GPU_EVENT(gtao_render);

		RHIViewport Viewport(0.0f, 0.0f, (float)rt_gtao_0->dwWidth, (float)rt_gtao_0->dwHeight);
		Cmd.SetViewport(Viewport);

		DrawSQ(Cmd, s_gtao, rt_gtao_0, 0, [&]
		{
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.SetStencil(false);
			Cmd.Constants.Write<CBPost>().gtao_parameters = p_scale;
		});

		Viewport.Width = (float)Cmd.GetWidth();
		Viewport.Height = (float)Cmd.GetHeight();
		Cmd.SetViewport(Viewport);
	}

	{
		GPU_EVENT(gtao_filter);

		// D3D11 drops the SRV of a resource still bound as a render target
		u_setrt(Cmd, (u32)Cmd.GetWidth(), (u32)Cmd.GetHeight(), nullptr, nullptr, nullptr, nullptr);

		SPass& P = *(s_gtao->E[1]->passes[0]);
		Cmd.SetStates(P.state);
		Cmd.SetConstantTable(P.constants);
		Cmd.SetTextures(P.T);
		Cmd.SetCS(P.cs);

		IRHIUnorderedAccessView* UAV = rt_ssao_temp->pUAView;
		Cmd.SetComputeUAVs(0, 1, &UAV);

		Cmd.Dispatch((rt_gtao_0->dwWidth + 7u) / 8u, (rt_gtao_0->dwHeight + 7u) / 8u, 1);

		UAV = nullptr;
		Cmd.SetComputeUAVs(0, 1, &UAV);
		Cmd.UnbindComputeTextures();
	}
}

void CRenderTarget::pharse_velocity(GfxCmdList& Cmd)
{
	GPU_EVENT(pharse_velocity);

	DrawSQ(Cmd, s_gtao, rt_Velocity, 2, [&]
	{
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	});
}

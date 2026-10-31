#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_gtao(GfxCmdList& Cmd)
{
	GPU_EVENT(phase_gtao);

	float p_scale = Cmd.GetHeight() / (tan(deg2rad(Device.fFOV) * 0.5f) * 2.0f);
	p_scale *= 0.5;

	{
		GPU_EVENT(gtao_render);
		DrawSQ(Cmd, s_gtao, rt_gtao_0, 0, [&]
		{
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.SetStencil(false);
			Cmd.Constants.Write<CBPost>().gtao_parameters = p_scale;
		});
	}

	{
		GPU_EVENT(gtao_filter);
		DrawSQ(Cmd, s_gtao, rt_ssao_temp, 1, [&]
		{
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.SetStencil(false);
		});
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

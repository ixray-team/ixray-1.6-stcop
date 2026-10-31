#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_gtao()
{
	GPU_EVENT(phase_gtao);

	float p_scale = RCache.get_height() / (tan(deg2rad(Device.fFOV) * 0.5f) * 2.0f);
	p_scale *= 0.5;

	{
		GPU_EVENT(gtao_render);
		DrawSQ(s_gtao, rt_gtao_0, 0, [&]
		{
			RCache.set_CullMode(ERHI_CULLMODE::NONE);
			RCache.set_Stencil(false);
			RCache.Constants.Write<CBPost>().gtao_parameters = p_scale;
		});
	}

	{
		GPU_EVENT(gtao_filter);
		DrawSQ(s_gtao, rt_ssao_temp, 1, []
		{
			RCache.set_CullMode(ERHI_CULLMODE::NONE);
			RCache.set_Stencil(false);
		});
	}
}

void CRenderTarget::pharse_velocity()
{
	GPU_EVENT(pharse_velocity);

	DrawSQ(s_gtao, rt_Velocity, 2, []
	{
		RCache.set_CullMode(ERHI_CULLMODE::NONE);
	});
}

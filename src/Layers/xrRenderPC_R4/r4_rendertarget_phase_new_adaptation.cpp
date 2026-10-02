#include "stdafx.h"

#include "r4_rendertarget.h"

RHIViewport VP_NL = {
	0.0f,
	0.0f,
	1024.f,
	1024.f,
	0.0f,
	1.0f
};

void CRenderTarget::phase_new_luminance()
{
	GPU_EVENT(phase_new_luminance);

	ref_rt* targets[] = { &rt_LUM_A, &rt_LUM_B, &rt_LUM_C };
	for (u32 i = 0; i < std::size(targets); ++i)
	{
		DrawSQ(s_lum_copy, *targets[i], i, [&]
		{
			RImplementation.rmNormal();
			GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
			RCache.set_Stencil(false);
			RCache.set_c("adapt_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));
		});
	}

	DrawSQ(s_lum_copy, rt_LUM_D, 3, [&]
	{
		RImplementation.rmNormal();
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		RCache.set_Stencil(false);

		RCache.set_c("adapt_params", ps_r2_autoexposure_min_weight, ps_r2_autoexposure_gaussian, 1.0f - exp(-Device.fTimeDeltaSmoothing / ps_r2_autoexposure_speed), 0.f);
		RCache.set_c("adapt_params2", ps_r2_autoexposure_soft_log_k, ps_r2_autoexposure_soft_limiter, ps_r2_autoexposure_sensitivity, 0.f);

		f_luminance_adapt = 0.9f * f_luminance_adapt + 0.1f * Device.fTimeDelta * ps_r2_tonemap_adaptation;

		Fvector3 Current, Result;
		Result.set(1, 0, 1);
		Current.set(ps_r2_tonemap_middlegray, 1.f, ps_r2_tonemap_low_lum);
		Result.lerp(Result, Current, ps_r2_tonemap_amount);

		RCache.set_c("MiddleGray", Result.x, Result.y, Result.z, f_luminance_adapt);
	});
}

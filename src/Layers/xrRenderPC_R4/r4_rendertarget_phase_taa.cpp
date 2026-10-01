#include "stdafx.h"
#include "r4_rendertarget.h"

void CRenderTarget::phase_taa()
{
	DrawSQ(s_taa, rt_Generic_2, 0, []
	{
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	});

	ResolveSurface(rt_Generic_0_prev, rt_Generic_2);
	ResolveSurface(rt_Generic_0, rt_Generic_2);
}

void CRenderTarget::phase_mblur()
{
	if (ps_r4_mblur_power < EPS || !ps_r4_mblur_quality)
		return;

	GPU_EVENT(PhaseMBlur);

	for (u32 i = 1; i <= ps_r4_mblur_quality; ++i)
	{
		DrawSQ(s_taa, rt_Generic_2, 1, [&]
		{
			GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
			RCache.set_c("mblur_params", ps_r4_mblur_power / i, i, 1.0f / dwWidth, 1.0f / dwHeight);
		});
		ResolveSurface(rt_Generic_0, rt_Generic_2);
	}
}

void CRenderTarget::phase_depth_upscale()
{
	Fmatrix invVP_old;
	invVP_old.invert44(Device.mFullTransform_old);

	DrawSQ(s_taa, rt_upscaled_depth, 2, [&]
	{
		RCache.set_c("m_invVP_old", invVP_old);
	});

	ResolveSurface(rt_upscaled_depth_old, rt_upscaled_depth);
}

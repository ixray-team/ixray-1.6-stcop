#include "stdafx.h"
#include "r4_rendertarget.h"

void CRenderTarget::phase_taa()
{
    u_setrt(rt_Generic_2, nullptr, nullptr, nullptr);
    GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);

    RCache.set_Element(s_taa->E[0]);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
	
	//Save previous frame
	GRHI->CopySurface(rt_Generic_0_prev->pSurface, rt_Generic_2->pSurface);
	GRHI->CopySurface(rt_Generic_0->pSurface, rt_Generic_2->pSurface);
}

void CRenderTarget::phase_mblur()
{
	if (ps_r4_mblur_power < EPS || !ps_r4_mblur_quality)
	{
		return;
	}

	GPU_EVENT(PhaseMBlur);

	for (u32 i = 1; i <= ps_r4_mblur_quality; ++i) 
	{
		u_setrt(rt_Generic_2, nullptr, nullptr, nullptr);
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);

		RCache.set_Element(s_taa->E[1]);
		RCache.set_c("mblur_params", ps_r4_mblur_power / i, i, 1.0f / dwWidth, 1.0f / dwHeight);
		RCache.set_Geometry(FSTriangleGeom);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
		GRHI->CopySurface(rt_Generic_0->pSurface, rt_Generic_2->pSurface);
	}
}

void CRenderTarget::phase_depth_upscale()
{
	u_setrt(rt_upscaled_depth, nullptr, nullptr, nullptr);
	Fmatrix invVP_old; invVP_old.invert44(Device.mFullTransform_old);

	RCache.set_Element(s_taa->E[2]);
	RCache.set_Geometry(FSTriangleGeom);

	RCache.set_c("m_invVP_old", invVP_old);

	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
	GRHI->CopySurface(rt_upscaled_depth_old->pSurface, rt_upscaled_depth->pSurface);
}
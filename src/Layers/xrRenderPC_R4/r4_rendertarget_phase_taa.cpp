#include "stdafx.h"
#include "r4_rendertarget.h"

void CRenderTarget::phase_taa()
{
	//Output directly to rt_Generic_0 hence we didn't call CopyResource before
    u_setrt(rt_Generic_2, nullptr, nullptr, nullptr);
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(FALSE);

    RCache.set_Element(s_taa->E[0]);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
	
	//Save previous frame
	GRHI->CopySurface(rt_Generic_0_prev->pSurface, rt_Generic_2->pSurface);

	//Copy to rt_Generic_0... waste more perf...
	GRHI->CopySurface(rt_Generic_0->pSurface, rt_Generic_2->pSurface);
}

void CRenderTarget::phase_mblur()
{
	if (ps_r4_mblur_power < EPS || !ps_r4_mblur_quality)
	{
		return;
	}

	auto mblur_power = 0.016f / Device.fTimeDelta;
	mblur_power *= ps_r4_mblur_power;

	GPU_EVENT(PhaseMBlur);

	for (u32 i = 1; i <= ps_r4_mblur_quality; ++i) 
	{
		u_setrt(rt_Generic_2, nullptr, nullptr, nullptr);

		RCache.set_CullMode(CULL_NONE);
		RCache.set_Stencil(FALSE);

		RCache.set_Element(s_taa->E[1]);
		RCache.set_c("mblur_params", mblur_power / i, i, 1.0f / dwWidth, 1.0f / dwHeight);
		RCache.set_Geometry(FSTriangleGeom);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
		GRHI->CopySurface(rt_Generic_0->pSurface, rt_Generic_2->pSurface);
	}
}
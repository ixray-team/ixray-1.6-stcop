#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_gtao()
{
	GPU_EVENT(phase_gtao);

	//Calculate projection factor, to transform world radius to screen space
	float p_scale = RCache.get_height() / (tan(deg2rad(Device.fFOV) * 0.5f) * 2.0f);
	p_scale *= 0.5;
	FVF::TL* pv = nullptr;

	{
		GPU_EVENT(gtao_render);
		//Render the AO and view-z into new rendertarget
		u_setrt(rt_gtao_0, nullptr, nullptr, nullptr);
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		RCache.set_Stencil(false);

		//Go go power rangers
		RCache.set_Element(s_gtao->E[0]);
		RCache.set_c("gtao_parameters", p_scale);
		RCache.set_Geometry(FSTriangleGeom);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
	}

	{
		GPU_EVENT(gtao_filter);
		//Blur...
		u_setrt(rt_ssao_temp, nullptr, nullptr, nullptr);
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		RCache.set_Stencil(false);

		//Go go power rangers
		RCache.set_Element(s_gtao->E[1]);
		RCache.set_Geometry(FSTriangleGeom);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
	}
}
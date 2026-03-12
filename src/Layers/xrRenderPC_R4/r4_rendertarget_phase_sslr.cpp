#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_sslr()
{
	GPU_EVENT(phase_sslr);

	{
		GPU_EVENT(sslr_render);
		//Render the AO and view-z into new rendertarget
		u_setrt(rt_sslr, rt_sslr_data, nullptr, nullptr);
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);

		//Go go power rangers
		RCache.set_Element(s_sslr->E0]);
		RCache.set_Geometry(FSTriangleGeom);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
	}

	{
		GPU_EVENT(sslr_filter);
		u_setrt(rt_sslr_temp, nullptr, nullptr, nullptr);
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);

		//Go go power rangers
		RCache.set_Element(s_sslr->E[1]);
		RCache.set_Geometry(FSTriangleGeom);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
	}

	{
		GPU_EVENT(sslr_temporal);
		u_setrt(rt_sslr, nullptr, nullptr, nullptr);
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);

		//Go go power rangers
		RCache.set_Element(s_sslr->E[2]);
		RCache.set_Geometry(FSTriangleGeom);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
	}

	GRHI->CopySurface(rt_sslr_old->pSurface, rt_sslr->pSurface);
}
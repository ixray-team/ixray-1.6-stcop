#include "stdafx.h"
#include "r4_rendertarget.h"

void CRenderTarget::phase_taa()
{
	u32 Offset = 0;
    constexpr u32 vertex_color = color_rgba(0, 0, 0, 255);

	//Output directly to rt_Generic_0 hence we didn't call CopyResource before
    u_setrt(rt_Generic_2, nullptr, nullptr, nullptr);
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(FALSE);

	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(3, g_combine->vb_stride, Offset);
	pv->set(-1.0, 1.0, 1.0, 1.0, vertex_color, 0.0, 0.0);
	pv++;
	pv->set(3.0, 1.0, 1.0, 1.0, vertex_color, 2.0, 0.0);
	pv++;
	pv->set(-1.0, -3.0, 1.0, 1.0, vertex_color, 0.0, 2.0);
	pv++;
	RCache.Vertex.Unlock(3, g_combine->vb_stride);

    RCache.set_Element(s_taa->E[0]);
	RCache.set_Geometry(g_combine);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Offset, 0, 3, 0, 1);
	
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
	u32 Offset = 0;

	for (u32 i = 1; i <= ps_r4_mblur_quality; ++i) 
	{
		u_setrt(rt_Generic_2, nullptr, nullptr, nullptr);

		RCache.set_CullMode(CULL_NONE);
		RCache.set_Stencil(FALSE);

		FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(3, g_combine->vb_stride, Offset);

		pv[2].set(-1.0, -3.0, 1.0, 1.0, 0x0, 0.0, 2.0);
		pv[0].set(-1.0, 1.0, 1.0, 1.0, 0x0, 0.0, 0.0);
		pv[1].set(3.0, 1.0, 1.0, 1.0, 0x0, 2.0, 0.0);

		RCache.Vertex.Unlock(3, g_combine->vb_stride);

		RCache.set_Element(s_taa->E[1]); RCache.set_Geometry(g_combine);
		RCache.set_c("mblur_params", mblur_power / i, i, 1.0f / dwWidth, 1.0f / dwHeight);

		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Offset, 0, 3, 0, 1);
		GRHI->CopySurface(rt_Generic_0->pSurface, rt_Generic_2->pSurface);
	}
}
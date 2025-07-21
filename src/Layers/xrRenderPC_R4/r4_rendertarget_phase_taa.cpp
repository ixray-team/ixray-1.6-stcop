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
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 3, 0, 1);
	
	//Save previous frame
	RContext->CopyResource(rt_Generic_0_prev->pSurface, rt_Generic_2->pSurface);

	//Copy to rt_Generic_0... waste more perf...
	RContext->CopyResource(rt_Generic_0->pSurface, rt_Generic_2->pSurface);
}
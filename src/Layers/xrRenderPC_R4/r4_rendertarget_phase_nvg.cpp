#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_nvg()
{
	u32 Offset = 0;
    constexpr u32 vertex_color = color_rgba(0, 0, 0, 255);

    u_setrt(rt_Back_Buffer_AA, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
    RCache.set_Stencil(FALSE);

	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(3, g_combine->vb_stride, Offset);
	pv->set(-1.0, 1.0, 1.0, 1.0, vertex_color, 0.0, 0.0);
	pv++;
	pv->set(3.0, 1.0, 1.0, 1.0, vertex_color, 2.0, 0.0);
	pv++;
	pv->set(-1.0, -3.0, 1.0, 1.0, vertex_color, 0.0, 2.0);
	pv++;
	RCache.Vertex.Unlock(3, g_combine->vb_stride);

    RCache.set_Element(s_nvg->E[0]);
	RCache.set_Geometry(g_combine);

	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Offset, 0, 3, 0, 1);
	GRHI->CopySurface(rt_Back_Buffer->pSurface, rt_Back_Buffer_AA->pSurface);
}
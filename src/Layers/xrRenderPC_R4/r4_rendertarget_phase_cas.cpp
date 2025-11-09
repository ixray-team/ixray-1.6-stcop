#include "stdafx.h"

void CRenderTarget::phase_cas()
{
    u_setrt(rt_Back_Buffer_AA, nullptr, nullptr, nullptr);
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(FALSE);

	// Draw COLOR
    RCache.set_Element(s_cas->E[0]);
	RCache.set_c("sharpening_intensity", ps_r4_cas_sharpening);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	//Resolve back to rt_Back_Buffer
	GRHI->CopySurface(rt_Back_Buffer->pSurface, rt_Back_Buffer_AA->pSurface);
}
#include "stdafx.h"

void CRenderTarget::phase_fxaa()
{
    u_setrt(rt_Generic_2, nullptr, nullptr, nullptr);

    // Set pass
    RCache.set_Element(s_fxaa->E[0]);

    // Set geometry
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

    // Resolve RT
    GRHI->CopySurface(rt_Generic_0->pSurface, rt_Generic_2->pSurface);
}

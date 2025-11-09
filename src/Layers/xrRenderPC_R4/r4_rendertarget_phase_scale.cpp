#include "stdafx.h"

void CRenderTarget::phase_scale() {
	u_setrt(rt_Generic, 0, 0, 0);
	RImplementation.rmNormal();

	// Draw COLOR
	RCache.set_Element(s_scale->E[ps_r_scale_mode]);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
}
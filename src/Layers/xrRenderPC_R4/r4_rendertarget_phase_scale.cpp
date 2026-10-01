#include "stdafx.h"

void CRenderTarget::phase_scale()
{
	DrawSQ(s_scale, rt_Generic, ps_r_scale_mode, []
	{
		RImplementation.rmNormal();
	});
}

void CRenderTarget::PhaseEditorScale()
{
	u_setrt(get_target_width(), get_target_height(), rt_BackbufferLUT->pRT, nullptr, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);
	RCache.set_ColorWriteEnable();
	RCache.set_Element(s_scale->E[1]);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
}

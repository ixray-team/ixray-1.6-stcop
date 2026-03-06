#include "stdafx.h"

void CRenderTarget::phase_ssao()
{
	u_setrt(rt_ssao_temp, 0, 0, 0);
	RCache.set_Stencil(false);

	RImplementation.rmNormal();

	RCache.set_Element(s_ssao->E[0]);
	RCache.set_Geometry(FSTriangleGeom);

	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
	
	RCache.set_Stencil(false);
}


void CRenderTarget::phase_downsamp()
{
	u_setrt(rt_half_depth, 0, 0, 0);
	GRHI->ClearTarget(rt_half_depth->pRT);

	u32 w = (u32)RCache.get_width();
	u32 h = (u32)RCache.get_height();

	RImplementation.rmNormal();
	RCache.set_Stencil(false);

	Fmatrix m_v2w;
	m_v2w.invert(Device.mView);

	u32 Offset = 0;

	// Fill VB
	float	scale_X = float(w) / float(TEX_jitter);
	float	scale_Y = float(h) / float(TEX_jitter);

	// Fill vertex buffer
	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
	pv->set(-1, 1, 0, 1, 0, 0, scale_Y);	pv++;
	pv->set(-1, -1, 0, 0, 0, 0, 0);	pv++;
	pv->set(1, 1, 1, 1, 0, scale_X, scale_Y);	pv++;
	pv->set(1, -1, 1, 0, 0, scale_X, 0);	pv++;
	RCache.Vertex.Unlock(4, g_combine->vb_stride);

	// Draw
	RCache.set_Element(s_ssao->E[1]);
	RCache.set_Geometry(g_combine);
	RCache.set_c("m_v2w", m_v2w);

	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Offset, 0, 4, 0, 2);
}
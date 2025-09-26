#include "stdafx.h"

void set_viewport(ID3DDeviceContext* dev, float w, float h)
{
	D3D_VIEWPORT viewport[1] =
	{
		0, 0, w, h, 0.f, 1.f
	};
	dev->RSSetViewports(1, viewport);
}

void CRenderTarget::phase_ssao()
{
	GRHI->ClearTarget(rt_ssao_temp->pRT);

	// low/hi RTs
	u_setrt(rt_ssao_temp, 0, 0, 0/*RDepth*/);

	RCache.set_Stencil(FALSE);

	// Fill VB
	float	scale_X = RCache.get_width() * 0.5f / float(TEX_jitter);
	float	scale_Y = RCache.get_height() * 0.5f / float(TEX_jitter);

	float _w = RCache.get_width();
	float _h = RCache.get_height();

	set_viewport(RContext, _w, _h);
	u32	Offset = 0;

	// Fill vertex buffer
	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
	pv->set(-1, 1, 0, 1, 0, 0, scale_Y);	pv++;
	pv->set(-1, -1, 0, 0, 0, 0, 0);	pv++;
	pv->set(1, 1, 1, 1, 0, scale_X, scale_Y);	pv++;
	pv->set(1, -1, 1, 0, 0, scale_X, 0);	pv++;
	RCache.Vertex.Unlock(4, g_combine->vb_stride);

	// Draw
	RCache.set_Element(s_ssao->E[0]);
	RCache.set_Geometry(g_combine);

	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
	set_viewport(RContext, RCache.get_width(), RCache.get_height());

	RCache.set_Stencil(FALSE);
}


void CRenderTarget::phase_downsamp()
{
	u_setrt(rt_half_depth, 0, 0, 0);
	GRHI->ClearTarget(rt_half_depth->pRT);

	u32 w = (u32)RCache.get_width();
	u32 h = (u32)RCache.get_height();

	RImplementation.rmNormal();
	RCache.set_Stencil(FALSE);

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

	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
}
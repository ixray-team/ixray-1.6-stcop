#include "stdafx.h"

#include "../xrRender/dxRenderDeviceRender.h"

void CRenderTarget::PhaseGammaGenerateLUT()
{
	u32 Offset = 0;
	float _w = float(Device.TargetWidth);
	float _h = float(Device.TargetHeight);

	Fvector2 p0, p1;
#if defined(USE_DX11)
	p0.set(0.0f, 0.0f);
	p1.set(1.0f, 1.0f);
#else
	p0.set(0.5f / _w, 0.5f / _h);
	p1.set((_w + 0.5f) / _w, (_h + 0.5f) / _h);
#endif

	/////// Create gamma LUT //////////////////
	u_setrt(rt_GammaLUT, nullptr, nullptr, nullptr);
	RCache.set_Stencil(FALSE);

	// Constants
	float brightness = dxRenderDeviceRender::Instance().m_Gamma.fBrightness;
	float gamma = dxRenderDeviceRender::Instance().m_Gamma.fGamma;
	float contrast = dxRenderDeviceRender::Instance().m_Gamma.fContrast;
	Fcolor color_grading = dxRenderDeviceRender::Instance().m_Gamma.cBalance;

	// Fill vertex buffer
	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
	pv->set(0.0f, 1.0f, 0.0f, 1.0f, 0, p0.x, p1.y); pv++;
	pv->set(0.0f, 0.0f, 0.0f, 1.0f, 0, p0.x, p0.y); pv++;
	pv->set(1024, 1.0f, 0.0f, 1.0f, 0, p1.x, p1.y); pv++;
	pv->set(1024, 0.0f, 0.0f, 1.0f, 0, p1.x, p0.y); pv++;
	RCache.Vertex.Unlock(4, g_combine->vb_stride);

	// Draw COLOR
	RCache.set_Element(s_gamma->E[0]);
	RCache.set_c("color_params", brightness, gamma, contrast, 0.0f);
	RCache.set_c("color_grading", color_grading.r, color_grading.g, color_grading.b, 0.0f);
	RCache.set_Geometry(g_combine);
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
}

void CRenderTarget::PhaseGammaApply()
{
	float w = float(Device.TargetWidth);
	float h = float(Device.TargetHeight);
	RCache.set_Z(FALSE);

	u32 Offset = 0;
	float d_Z = EPS_S;
	float d_W = 1.0f;
	u32	C = color_rgba(0, 0, 0, 255);

#if defined(USE_DX11)
	u_setrt(w, h, RTarget, nullptr, nullptr, nullptr);
#endif

	RCache.set_CullMode(CULL_NONE);
	RCache.set_Stencil(false);

	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
	pv->set(0, h, d_Z, d_W, C, 0, 1); pv++;
	pv->set(0, 0, d_Z, d_W, C, 0, 0); pv++;
	pv->set(w, h, d_Z, d_W, C, 1, 1); pv++;
	pv->set(w, 0, d_Z, d_W, C, 1, 0); pv++;
	RCache.Vertex.Unlock(4, g_combine->vb_stride);

	RCache.set_Element(s_gamma->E[1]);
	RCache.set_Geometry(g_combine);
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
}
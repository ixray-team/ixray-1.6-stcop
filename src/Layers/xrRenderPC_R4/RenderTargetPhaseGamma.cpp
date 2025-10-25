#include "stdafx.h"

#include "../xrRender/dxRenderDeviceRender.h"

void CRenderTarget::PhaseGammaApply()
{
	float w = float(Device.TargetWidth);
	float h = float(Device.TargetHeight);

	auto color_grading = dxRenderDeviceRender::Instance().m_Gamma.cBalance;

	float brightness = dxRenderDeviceRender::Instance().m_Gamma.fBrightness;
	float contrast = dxRenderDeviceRender::Instance().m_Gamma.fContrast;
	float gamma = dxRenderDeviceRender::Instance().m_Gamma.fGamma;

	brightness = (brightness - contrast) * 0.25f;
	contrast = contrast * 0.5f + 0.5f;
	gamma = 1.0f / _max(EPS_S, gamma);

#if defined(USE_DX11)
	u_setrt(w, h, RTarget, nullptr, nullptr, nullptr);
#endif

	RCache.set_CullMode(CULL_NONE);
 	RImplementation.rmNormal();

	u32 Offset = 0;
	auto pv = (FVF::TL*)RCache.Vertex.Lock(3, g_combine->vb_stride, Offset);

	pv[2].set(-1.0, -3.0, 1.0, 1.0, 0x0, 0.0, 2.0);
	pv[0].set(-1.0, 1.0, 1.0, 1.0, 0x0, 0.0, 0.0);
	pv[1].set(3.0, 1.0, 1.0, 1.0, 0x0, 2.0, 0.0);

	RCache.Vertex.Unlock(3, g_combine->vb_stride);
	RCache.set_Element(s_gamma->E[0]);

	RCache.set_c("color_params", contrast, gamma, brightness, 0.0f);
	RCache.set_c("color_grading", color_grading.r, color_grading.g, color_grading.b, 0.0f);

	RCache.set_Geometry(g_combine);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Offset, 0, 3, 0, 1);
}
#include "stdafx.h"

#include "dxRenderDeviceRender.h"

void CRenderTarget::PhaseGammaApply(GfxCmdList& Cmd, IRHIRenderTargetView* Destination)
{
	float w = float(Device.TargetWidth);
	float h = float(Device.TargetHeight);

	auto color_grading = dxRenderDeviceRender::Instance().m_Gamma.cBalance;

	float brightness = dxRenderDeviceRender::Instance().m_Gamma.fBrightness;
	float contrast = dxRenderDeviceRender::Instance().m_Gamma.fContrast;
	float gamma = dxRenderDeviceRender::Instance().m_Gamma.fGamma;

	brightness = (brightness - contrast) * 0.25f;
	contrast = contrast * 0.5f + 0.5f;
	gamma = 1.0f / std::max(EPS_S, gamma);

	u_setrt(Cmd, w, h, Destination ? Destination : RTarget, nullptr, nullptr, nullptr);

	DrawPassSQ(Cmd, s_gamma, 0, [&]
	{
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
		RImplementation.rmNormal();
		Cmd.Constants.Write<CBFrame>().color_params.set(contrast, gamma, brightness, 0.0f);
		Cmd.Constants.Write<CBFrame>().color_grading.set(color_grading.r, color_grading.g, color_grading.b, 0.0f);
	});
}
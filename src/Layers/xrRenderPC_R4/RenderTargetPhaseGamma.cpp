#include "stdafx.h"

#include "dxRenderDeviceRender.h"

void CRenderTarget::PhaseGammaApply(IRHIRenderTargetView* Destination)
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

	u_setrt(w, h, Destination ? Destination : RTarget, nullptr, nullptr, nullptr);
	constexpr float opaqueBlack[4] = { 0.f, 0.f, 0.f, 1.f };
	GRHI->ClearTarget(Destination ? Destination : RTarget, opaqueBlack);
	const u32 colorWriteMask = GRHI->StateManager->GetColorWriteEnable();

	DrawPassSQ(s_gamma, 0, [&]
	{
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		RImplementation.rmNormal();
		RCache.set_ColorWriteEnable(colorWriteMask & ~D3DCOLORWRITEENABLE_ALPHA);
		RCache.set_c("color_params", contrast, gamma, brightness, 0.0f);
		RCache.set_c("color_grading", color_grading.r, color_grading.g, color_grading.b, 0.0f);
	});
	RCache.set_ColorWriteEnable(colorWriteMask);
}

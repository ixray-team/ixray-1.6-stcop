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

	// Straight into the window the view is fitted into it and the rest of the window is black; into a target it
	// takes the target's corner
	IRHIRenderTargetView* Target = Destination ? Destination : RTarget;
	const bool IsWindow = Target == GRHI->DevicePtr->SwapChainRTV;
	const Irect Shown = IsWindow ? Device.PresentRect : Irect{ 0, 0, int(w), int(h) };
	const bool IsCovered = Shown.x1 <= 0 && Shown.y1 <= 0 && u32(Shown.x2) >= GRHI->SwapChain.Width && u32(Shown.y2) >= GRHI->SwapChain.Height;

	if (IsWindow && !IsCovered)
	{
		constexpr float Black[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
		Cmd.ClearTarget(Target, Black);
	}

	u_setrt(Cmd, w, h, Target, nullptr, nullptr, nullptr);

	DrawPassSQ(Cmd, s_gamma, 0, [&]
	{
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
		RImplementation.rmNormal();
		RHIViewport Area(float(Shown.x1), float(Shown.y1), float(Shown.width()), float(Shown.height()));
		Cmd.SetViewport(Area);
		Cmd.Constants.Write<CBFrame>().color_params.set(contrast, gamma, brightness, 0.0f);
		Cmd.Constants.Write<CBFrame>().color_grading.set(color_grading.r, color_grading.g, color_grading.b, 0.0f);
		Cmd.Constants.Write<CBPass>().c_scale.set(float(Shown.x1), float(Shown.y1), w / float(std::max(Shown.width(), 1)), h / float(std::max(Shown.height(), 1)));
	});
}

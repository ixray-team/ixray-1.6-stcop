#include "stdafx.h"

#include "EditorDrawContext.h"

#include "OverlayAPI/FSR3Wrapper.h"

void CRenderTarget::init_fsr()
{
	g_Fsr3Wrapper.Destroy();

	Fsr3Wrapper::ContextParameters initParams = {};

	initParams.displaySize.width = (u32)GfxCmdList::GetTargetWidth();
	initParams.displaySize.height = (u32)GfxCmdList::GetTargetHeight();

	initParams.maxRenderSize.width = (u32)GfxCmdList::GetWidth();
	initParams.maxRenderSize.height = (u32)GfxCmdList::GetHeight();

	g_Fsr3Wrapper.Create(initParams);
}

bool CRenderTarget::phase_fsr()
{
	GPU_EVENT(FSR);

	Fsr3Wrapper::DrawParameters fsr3Params = {};

	fsr3Params.unresolvedColorResource = rt_Generic_0->pSurface;
	fsr3Params.motionvectorResource = rt_Velocity->pSurface;
	fsr3Params.depthbufferResource = rt_Position->pSurface;

	fsr3Params.reactiveMapResource = ReactiveMask->pSurface;
	fsr3Params.transparencyAndCompositionResource = nullptr;

	fsr3Params.resolvedColorResource = rt_Generic->pSurface;

	fsr3Params.renderWidth = (u32)GfxCmdList::GetWidth();
	fsr3Params.renderHeight = (u32)GfxCmdList::GetHeight();
	fsr3Params.displayWidth = (u32)GfxCmdList::GetTargetWidth();
	fsr3Params.displayHeight = (u32)GfxCmdList::GetTargetHeight();

	// The context is made for the largest sizes and takes the view's as they are; what it kept is of the size before
	static u32 LastWidth = 0;
	static u32 LastHeight = 0;
	const bool IsResized = LastWidth != fsr3Params.displayWidth || LastHeight != fsr3Params.displayHeight;
	LastWidth = fsr3Params.displayWidth;
	LastHeight = fsr3Params.displayHeight;

	fsr3Params.cameraReset = IsResized || (Device.IsEditorMode() && GEditorDraw.UpscalerReset);

	fsr3Params.cameraJitterX = ps_r_taa_jitter_full.x;
	fsr3Params.cameraJitterY = ps_r_taa_jitter_full.y;

	fsr3Params.enableSharpening = false;
	fsr3Params.sharpness = 0.0f;

	fsr3Params.frameTimeDelta = std::max(1.0f + EPS_L, float(RFrame.dwTimeDelta));

	fsr3Params.farPlane = RFrame.Env().far_plane;
	fsr3Params.nearPlane = RFrame.fViewportNear;
	fsr3Params.fovH = deg2rad(RFrame.fFOV);

	return g_Fsr3Wrapper.Draw(fsr3Params);
}

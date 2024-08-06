#include "stdafx.h"

#include "FSR2Wrapper.h"

extern Fvector3 ps_r_taa_jitter_full;

void CRenderTarget::init_fsr() 
{
	g_Fsr2Wrapper.Destroy();

	Fsr2Wrapper::ContextParameters initParams = {};

	initParams.displaySize.width = (u32)RCache.get_target_width();
	initParams.displaySize.height = (u32)RCache.get_target_height();

	initParams.maxRenderSize.width = (u32)RCache.get_width();
	initParams.maxRenderSize.height = (u32)RCache.get_height();

	initParams.device = RDevice;

#ifdef DEBUG_DRAW
	initParams.fpMessage = [](FfxFsr2MsgType type, const wchar_t* message) {
		xr_string error_msg = Platform::TCHAR_TO_ANSI_U8(message);
		Msg("[FSR]: %s", error_msg.c_str());
	};
#endif

	g_Fsr2Wrapper.Create(initParams);
}

bool CRenderTarget::phase_fsr() {
	PIX_EVENT(FSR);

	// #TODO: RHI - FSR
#if 0
	Fsr2Wrapper::DrawParameters fsr2Params = {};
	fsr2Params.deviceContext = RContext;

	fsr2Params.unresolvedColorResource = rt_Generic_0->pSurface;
	fsr2Params.motionvectorResource = rt_Velocity->pSurface;
	fsr2Params.depthbufferResource = rt_Position->pSurface;

	fsr2Params.reactiveMapResource = nullptr;
	fsr2Params.transparencyAndCompositionResource = nullptr;

	fsr2Params.resolvedColorResource = rt_Generic->pSurface;

	fsr2Params.renderWidth = (u32)RCache.get_width();
	fsr2Params.renderHeight = (u32)RCache.get_height();

	fsr2Params.cameraReset = false;

	fsr2Params.cameraJitterX = ps_r_taa_jitter_full.x;
	fsr2Params.cameraJitterY = ps_r_taa_jitter_full.y;

	fsr2Params.enableSharpening = false;
	fsr2Params.sharpness = 0.f;

	fsr2Params.frameTimeDelta = std::max(1.0f + EPS_L, float(Device.dwTimeDelta));

	fsr2Params.farPlane = g_pGamePersistent->Environment().CurrentEnv->far_plane;
	fsr2Params.nearPlane = VIEWPORT_NEAR;
	fsr2Params.fovH = deg2rad(Device.fFOV);

	return g_Fsr2Wrapper.Draw(fsr2Params);
#else
	return false;
#endif
}


#include "stdafx.h"

#include "OverlayAPI/FSR3Wrapper.h"

extern Fvector3 ps_r_taa_jitter_full;

void CRenderTarget::init_fsr()
{
	g_Fsr3Wrapper.Destroy();

	Fsr3Wrapper::ContextParameters initParams = {};

	initParams.displaySize.width = (u32)RCache.get_target_width();
	initParams.displaySize.height = (u32)RCache.get_target_height();

	initParams.maxRenderSize.width = (u32)RCache.get_width();
	initParams.maxRenderSize.height = (u32)RCache.get_height();

	initParams.device = RDevice;

	g_Fsr3Wrapper.Create(initParams);
}

bool CRenderTarget::phase_fsr()
{
	GPU_EVENT(FSR);

	Fsr3Wrapper::DrawParameters fsr3Params = {};
	fsr3Params.deviceContext = RContext;

	fsr3Params.unresolvedColorResource = (ID3D11Resource*)rt_Generic_0->pSurface->GetRawTexture();
	fsr3Params.motionvectorResource = (ID3D11Resource*)rt_Velocity->pSurface->GetRawTexture();
	fsr3Params.depthbufferResource = (ID3D11Resource*)rt_Position->pSurface->GetRawTexture();

	fsr3Params.reactiveMapResource = nullptr;
	fsr3Params.transparencyAndCompositionResource = nullptr;

	fsr3Params.resolvedColorResource = (ID3D11Resource*)rt_Generic->pSurface->GetRawTexture();

	fsr3Params.renderWidth = (u32)RCache.get_width();
	fsr3Params.renderHeight = (u32)RCache.get_height();
	fsr3Params.displayWidth = (u32)RCache.get_target_width();
	fsr3Params.displayHeight = (u32)RCache.get_target_height();

	fsr3Params.cameraReset = false;

	fsr3Params.cameraJitterX = ps_r_taa_jitter_full.x;
	fsr3Params.cameraJitterY = ps_r_taa_jitter_full.y;

	fsr3Params.enableSharpening = false;
	fsr3Params.sharpness = 0.0f;

	fsr3Params.frameTimeDelta = std::max(1.0f + EPS_L, float(Device.dwTimeDelta));

	fsr3Params.farPlane = g_pGamePersistent->Environment().CurrentEnv->far_plane;
	fsr3Params.nearPlane = Device.fViewportNear;
	fsr3Params.fovH = deg2rad(Device.fFOV);

	return g_Fsr3Wrapper.Draw(fsr3Params);
}

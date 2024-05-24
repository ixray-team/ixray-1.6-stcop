#include "stdafx.h"

#include "DLSSWrapper.h"

extern Fvector3 ps_r_taa_jitter_full;

void CRenderTarget::init_dlss() {
	g_DLSSWrapper.Destroy();

	DLSSWrapper::ContextParameters initParams;
	initParams.device = RDevice;
	initParams.displaySize = { (int)RCache.get_target_width(), (int)RCache.get_target_height() };
	initParams.renderSize = { (int)RCache.get_width(), (int)RCache.get_height() };
	g_DLSSWrapper.Create(initParams);
}

bool CRenderTarget::phase_dlss() {
	PIX_EVENT(DLSS);

	DLSSWrapper::DrawParameters dlssParams = {};
	dlssParams.deviceContext = RContext;

	dlssParams.unresolvedColorResource = rt_Generic_0->pSurface;
	dlssParams.motionvectorResource = rt_Velocity->pSurface;
	dlssParams.depthbufferResource = rt_Position->pSurface;

	dlssParams.exposureResource = nullptr;
	dlssParams.reactiveMapResource = nullptr;
	dlssParams.transparencyAndCompositionResource = nullptr;

	dlssParams.resolvedColorResource = rt_Generic->pSurface;

	dlssParams.renderWidth = RCache.get_width();
	dlssParams.renderHeight = RCache.get_height();

	dlssParams.cameraReset = false;

	dlssParams.cameraJitterX = ps_r_taa_jitter_full.x;
	dlssParams.cameraJitterY = ps_r_taa_jitter_full.y;

	dlssParams.sharpness = ps_r4_cas_sharpening;

	dlssParams.frameTimeDelta = std::max(1.0f + EPS_L, float(Device.dwTimeDelta));

	dlssParams.nearPlane = VIEWPORT_NEAR;
	dlssParams.farPlane = g_pGamePersistent->Environment().CurrentEnv->far_plane;
	dlssParams.fovH = deg2rad(Device.fFOV);

	return g_DLSSWrapper.Draw(dlssParams);
}

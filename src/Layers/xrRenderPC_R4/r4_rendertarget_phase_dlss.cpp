#include "stdafx.h"

#include "OverlayAPI/DLSSWrapper.h"

extern Fvector3 ps_r_taa_jitter_full;

void CRenderTarget::init_dlss()
{
	g_DLSSWrapper.Destroy();

	DLSSWrapper::ContextParameters initParams;
	initParams.device = RDevice;
	initParams.displaySize = { (int)RCache.get_target_width(), (int)RCache.get_target_height() };
	initParams.renderSize = { (int)RCache.get_width(), (int)RCache.get_height() };
	g_DLSSWrapper.Create(initParams);
}

bool CRenderTarget::phase_dlss()
{
	GPU_EVENT(DLSS);

	DLSSWrapper::DrawParameters dlssParams = {};
	dlssParams.deviceContext = RContext;

	dlssParams.unresolvedColorResource = (ID3D11Texture2D*)rt_Generic_0->pSurface->GetRawTexture();
	dlssParams.motionvectorResource = (ID3D11Texture2D*)rt_Velocity->pSurface->GetRawTexture();
	dlssParams.depthbufferResource = (ID3D11Texture2D*)rt_Position->pSurface->GetRawTexture();

	dlssParams.exposureResource = nullptr;
	dlssParams.reactiveMapResource = nullptr;
	dlssParams.transparencyAndCompositionResource = nullptr;

	dlssParams.resolvedColorResource = (ID3D11Texture2D*)rt_Generic->pSurface->GetRawTexture();

	dlssParams.renderWidth = (int)RCache.get_width();
	dlssParams.renderHeight = (int)RCache.get_height();

	dlssParams.cameraReset = false;

	dlssParams.cameraJitterX = ps_r_taa_jitter_full.x;
	dlssParams.cameraJitterY = ps_r_taa_jitter_full.y;

	dlssParams.sharpness = 0.0f;

	dlssParams.frameTimeDelta = std::max(1.0f + EPS_L, float(Device.dwTimeDelta));

	dlssParams.nearPlane = Device.fViewportNear;
	dlssParams.farPlane = g_pGamePersistent->Environment().CurrentEnv->far_plane;
	dlssParams.fovH = deg2rad(Device.fFOV);

	return g_DLSSWrapper.Draw(dlssParams);
}

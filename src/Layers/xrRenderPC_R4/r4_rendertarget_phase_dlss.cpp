#include "stdafx.h"

#include "EditorDrawContext.h"

#include "OverlayAPI/DLSSWrapper.h"
extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;
extern ENGINE_API u32 ps_r_dlss_model;

void CRenderTarget::init_dlss()
{
	DLSSWrapper::ContextParameters initParams;

	initParams.displaySize = { 
		(int)RCache.get_target_width(),
		(int)RCache.get_target_height() 
	};

	initParams.renderSize = { 
		(int)RCache.get_width(),
		(int)RCache.get_height() 
	};

	initParams.preset = ps_render_scale_preset;
	initParams.scale = ps_render_scale;
	initParams.model = ps_r_dlss_model;
	g_DLSSWrapper.Resize(initParams);
}

bool CRenderTarget::phase_dlss()
{
	GPU_EVENT(DLSS);

	DLSSWrapper::DrawParameters dlssParams = {};

	dlssParams.unresolvedColorResource = rt_Generic_0->pSurface;
	dlssParams.motionvectorResource = rt_Velocity->pSurface;
	dlssParams.depthbufferResource = rt_Position->pSurface;

	dlssParams.exposureResource = nullptr;
	dlssParams.reactiveMapResource = ReactiveMask->pSurface;
	dlssParams.transparencyAndCompositionResource = nullptr;

	dlssParams.resolvedColorResource = rt_Generic->pSurface;

	dlssParams.renderWidth = (int)RCache.get_width();
	dlssParams.renderHeight = (int)RCache.get_height();

	static Fvector PrevCameraPos = {};
	static Fvector PrevCameraDir = {};
	static u32 PrevFrame = 0;

	bool CameraCut = (Device.dwFrame != PrevFrame + 1)
		|| (PrevCameraPos.distance_to_sqr(Device.vCameraPosition) > 5.f * 5.f)
		|| (PrevCameraDir.dotproduct(Device.vCameraDirection) < 0.5f);

	PrevCameraPos.set(Device.vCameraPosition);
	PrevCameraDir.set(Device.vCameraDirection);
	PrevFrame = Device.dwFrame;

	dlssParams.cameraReset = Device.IsEditorMode() ? GEditorDraw.UpscalerReset : CameraCut;

	dlssParams.cameraJitterX = ps_r_taa_jitter_full.x;
	dlssParams.cameraJitterY = ps_r_taa_jitter_full.y;

	dlssParams.frameTimeDelta = std::max(1.0f + EPS_L, float(Device.dwTimeDelta));

	dlssParams.nearPlane = Device.fViewportNear;
	dlssParams.farPlane = g_pGamePersistent->Environment().CurrentEnv->far_plane;
	dlssParams.fovH = deg2rad(Device.fFOV);

	return g_DLSSWrapper.Draw(dlssParams);
}

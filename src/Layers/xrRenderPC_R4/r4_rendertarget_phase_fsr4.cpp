#include "stdafx.h"

#include "EditorDrawContext.h"

#include "OverlayAPI/FSR4Wrapper.h"

void CRenderTarget::init_fsr4()
{
	g_Fsr4Wrapper.Destroy();

	FSR4Wrapper::ContextParameters InitParams = {};

	InitParams.displaySize.width  = static_cast<u32>(RCache.get_target_width());
	InitParams.displaySize.height = static_cast<u32>(RCache.get_target_height());

	InitParams.maxRenderSize.width  = static_cast<u32>(RCache.get_width());
	InitParams.maxRenderSize.height = static_cast<u32>(RCache.get_height());

	InitParams.device = RDevice;

	g_Fsr4Wrapper.Create(InitParams);
}

bool CRenderTarget::phase_fsr4()
{
	GPU_EVENT(FSR4);

	// Camera-cut thresholds: teleport detection.
	// Units: squared meters for position, cosine for direction.
	constexpr float CameraCutDistanceSqr = 5.0f * 5.0f;
	constexpr float CameraCutDotProduct  = 0.5f; // ~60 degrees per frame

	FSR4Wrapper::DrawParameters Fsr4Params = {};
	Fsr4Params.deviceContext = RContext;
	
	Fsr4Params.flags |= Fsr4Flag_AutoExposure;
	Fsr4Params.flags |= Fsr4Flag_PreferFsr4;

	// Input textures (render resolution).
	Fsr4Params.unresolvedColorResource = (ID3D11Resource*)rt_Generic_0->pSurface->GetRawTexture();
	Fsr4Params.motionvectorResource    = (ID3D11Resource*)rt_Velocity->pSurface->GetRawTexture();
	Fsr4Params.depthbufferResource     = (ID3D11Resource*)rt_Position->pSurface->GetRawTexture();

	// Optional masks. reactiveMapResource is provided by the engine;
	// transparencyAndCompositionResource is not used by FSR4 (auto-handled).
	Fsr4Params.reactiveMapResource = (ID3D11Resource*)ReactiveMask->pSurface->GetRawTexture();
	Fsr4Params.transparencyAndCompositionResource = nullptr;

	// Output texture (display resolution).
	Fsr4Params.resolvedColorResource = (ID3D11Resource*)rt_Generic->pSurface->GetRawTexture();

	Fsr4Params.renderWidth   = (u32)RCache.get_width();
	Fsr4Params.renderHeight  = (u32)RCache.get_height();
	Fsr4Params.displayWidth  = (u32)RCache.get_target_width();
	Fsr4Params.displayHeight = (u32)RCache.get_target_height();

	// Camera-cut detection: teleports, loads, cutscene transitions and fast turns
	// must reset FSR4 temporal history to avoid ghosting.
	static Fvector PrevCameraPos = {};
	static Fvector PrevCameraDir = {};
	static u32     PrevFrame     = 0;

	const bool CameraCut = (Device.dwFrame != PrevFrame + 1)
		|| (PrevCameraPos.distance_to_sqr(Device.vCameraPosition) > CameraCutDistanceSqr)
		|| (PrevCameraDir.dotproduct(Device.vCameraDirection) < CameraCutDotProduct);

	PrevCameraPos.set(Device.vCameraPosition);
	PrevCameraDir.set(Device.vCameraDirection);
	PrevFrame = Device.dwFrame;

	Fsr4Params.cameraReset = Device.IsEditorMode() ? GEditorDraw.UpscalerReset : CameraCut;

	Fsr4Params.cameraJitterX = ps_r_taa_jitter_full.x;
	Fsr4Params.cameraJitterY = ps_r_taa_jitter_full.y;

	Fsr4Params.enableSharpening = false;
	Fsr4Params.sharpness = 0.0f;

	Fsr4Params.frameTimeDelta = std::max(1.0f + EPS_L, float(Device.dwTimeDelta));

	Fsr4Params.farPlane  = g_pGamePersistent->Environment().CurrentEnv->far_plane;
	
	Fsr4Params.nearPlane = Device.fViewportNear;
	Fsr4Params.fovH      = deg2rad(Device.fFOV);

	return g_Fsr4Wrapper.Draw(Fsr4Params);
}
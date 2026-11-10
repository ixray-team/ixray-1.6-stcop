#include "stdafx.h"

#include "r4_rendertarget.h"
#include "../../xrEngine/IGame_Persistent.h"

namespace DofCamera
{
	static float Validate(float Value, float Min, float Max, float Default)
	{
		return _valid(Value) ? clampr(Value, Min, Max) : Default;
	}
}

void CRenderTarget::CreateDofBlurTargets(GfxCmdList& Cmd)
{
	const u32 Divisor = g_DofBlurResolution == 4 ? 4u : 2u;
	const u32 Width = std::max((Device.TargetWidth + Divisor - 1) / Divisor, 1u);
	const u32 Height = std::max((Device.TargetHeight + Divisor - 1) / Divisor, 1u);
	ref_rt* Targets[] = { &DofDownsample, &rt_dof_blur3, &DofDownsampleDepth, &rt_dof_blur1, &rt_dof_blur2, &DofReal1, &DofImaginary1 };
	const u32 TileWidth = (Width + 7) / 8;
	const u32 TileHeight = (Height + 7) / 8;
	ref_rt* TileTargets[] = { &DofTileMax, &DofTileDilated };
	bool HasSize = true;
	for (const ref_rt* Target : Targets)
	{
		HasSize &= *Target && (*Target)->dwWidth == Width && (*Target)->dwHeight == Height;
	}
	for (const ref_rt* Target : TileTargets)
	{
		HasSize &= *Target && (*Target)->dwWidth == TileWidth && (*Target)->dwHeight == TileHeight;
	}
	if (HasSize)
	{
		return;
	}

	// Cached SRVs must follow recreated targets when half/quarter changes.
	Cmd.Invalidate();
	for (ref_rt* Target : Targets)
	{
		Target->destroy();
	}
	for (ref_rt* Target : TileTargets)
	{
		Target->destroy();
	}
	DofDownsample.create(R4_RT_DOF_DOWNSAMPLE, Width, Height, ERHI_FORMAT::R16G16B16A16_FLOAT);
	rt_dof_blur3.create(r2_RT_dof_blur3, Width, Height, ERHI_FORMAT::R16G16B16A16_FLOAT);
	DofDownsampleDepth.create(R4_RT_DOF_DOWNSAMPLE_DEPTH, Width, Height, ERHI_FORMAT::R32G32B32A32_FLOAT);
	rt_dof_blur1.create(r2_RT_dof_blur1, Width, Height, ERHI_FORMAT::R16G16B16A16_FLOAT);
	rt_dof_blur2.create(r2_RT_dof_blur2, Width, Height, ERHI_FORMAT::R16G16B16A16_FLOAT);
	DofReal1.create(R4_RT_DOF_REAL1, Width, Height, ERHI_FORMAT::R16G16B16A16_FLOAT);
	DofImaginary1.create(R4_RT_DOF_IMAGINARY1, Width, Height, ERHI_FORMAT::R16G16B16A16_FLOAT);
	DofTileMax.create(R4_RT_DOF_TILE_MAX, TileWidth, TileHeight, ERHI_FORMAT::R32_FLOAT);
	DofTileDilated.create(R4_RT_DOF_TILE_DILATED, TileWidth, TileHeight, ERHI_FORMAT::R32_FLOAT);
}

void CRenderTarget::phase_new_dof(GfxCmdList& Cmd)
{
	GPU_EVENT(phase_new_dof);
	CreateDofBlurTargets(Cmd);

	const float Width = float(Device.TargetWidth);
	const float Height = float(Device.TargetHeight);
	const float BlurWidth = float(rt_dof_blur1->dwWidth);
	const float BlurHeight = float(rt_dof_blur1->dwHeight);
	const float PixelScale = std::max(Width / BlurWidth, Height / BlurHeight);
	const bool Forced = g_pGamePersistent && g_pGamePersistent->IsDofCameraForced();
	const bool Physical = Forced || (g_NewDofMode != static_cast<u32>(EDofMode::Static) && !(g_pGamePersistent && g_pGamePersistent->IsDofForced()));
	const float Kernel = g_pGamePersistent ? g_pGamePersistent->GetDofKernel(ps_r2_dof_kernel_size) : ps_r2_dof_kernel_size;
	SDofCameraSettings Camera;
	Camera.Mode = static_cast<EDofMode>(g_NewDofMode);
	Camera.FNumber = DofCamera::Validate(g_NewDofFNumber, 0.7f, 64.0f, Camera.FNumber);
	Camera.ManualFocalLength = g_UseManualDofFocalLength;
	Camera.FocalLength = DofCamera::Validate(g_NewDofFocalLength, 1.0f, 1000.0f, Camera.FocalLength);
	Camera.FocusTime = DofCamera::Validate(g_NewDofFocusTime, 0.01f, 10.0f, Camera.FocusTime);
	Camera.FocusDistance = DofCamera::Validate(g_DofFocusDistance, 0.01f, 10000.0f, Camera.FocusDistance);
	Camera.TransitionTime = DofCamera::Validate(g_DofTransitionTime, 0.01f, 10.0f, Camera.TransitionTime);
	if (Physical && g_pGamePersistent)
	{
		g_pGamePersistent->GetDofCameraSettings(Camera);
	}
	const float SensorHeight = DofSensorHeight;
	const bool Instant = Camera.IsInstant || Device.IsCameraCut();
	float FocalLength = Camera.FocalLength;
	const Fmatrix& Projection = Device.mProject_saved;
	if (!Camera.ManualFocalLength && Projection._34 == 1.0f && Projection._44 == 0.0f)
	{
		// The main camera projection stores cot(vertical FOV / 2); convert sensor meters to lens millimeters.
		const float CameraFocalLength = 500.0f * SensorHeight * Projection._22;
		if (_valid(CameraFocalLength) && CameraFocalLength > 0.0f)
		{
			FocalLength = CameraFocalLength;
		}
	}
	const float Delta = Device.Paused() ? 0.0f : std::max(Device.fTimeDelta, 0.0f);
	const Fvector4 TargetLens = {FocalLength, Camera.FNumber, SensorHeight, Camera.FocusTime};
	if (!DofCameraInitialized)
	{
		DofCameraLens = TargetLens;
		DofCameraInitialized = true;
	}
	else
	{
		const float Blend = Instant ? 1.0f : 1.0f - exp(-Delta / Camera.TransitionTime);
		DofCameraLens.lerp(DofCameraLens, TargetLens, Blend);
	}

	RHIViewport Viewport(0.0f, 0.0f, 1.0f, 1.0f);
	Cmd.SetViewport(Viewport);
	{
		GPU_EVENT(dof_focus);
		DrawSQ(Cmd, s_dof_coc, rt_dof_focus, 0, [&]
		{
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.SetStencil(false);
			Cmd.Constants.Write<CBPost>().dof_focus_params.set(Instant ? 1.0f : 1.0f - exp(-Delta / DofCameraLens.w), Camera.Mode == EDofMode::Manual ? 1.0f : 0.0f, Camera.FocusDistance, DofCameraLens.x * 0.001f + EPS);
			Cmd.Constants.Write<CBPost>().dof_focus_point.set(Camera.FocusPoint.x, Camera.FocusPoint.y, 0.0f, 0.0f);
		});
	}

	Viewport.Width = Width;
	Viewport.Height = Height;
	Cmd.SetViewport(Viewport);
	{
		GPU_EVENT(dof_coc);
		DrawSQ(Cmd, s_dof_coc, rt_dof_coc, Physical ? 1 : 4, [&]
		{
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.SetStencil(false);
			Cmd.Constants.Write<CBPost>().dof_params.set(DofCameraLens.x, DofCameraLens.y, DofCameraLens.z, Height);
			Fvector Dof = ps_r2_dof;
			if (!Physical && g_pGamePersistent)
			{
				g_pGamePersistent->GetCurrentDof(Dof);
			}
			const float LensScale = 1.0f + (DofLensKernelScale - 1.0f) * LensDofFactor;
			Cmd.Constants.Write<CBPost>().dof_classic.set(Dof.x, Dof.y, Dof.z, ps_r2_dof_sky);
			Cmd.Constants.Write<CBPost>().dof_kernel.set(0.5f * Kernel * LensScale, 0.0f, DofMaxRadius(Kernel));
		});
	}

	Cmd.Constants.Write<CBPost>().dof_rt_size.set(BlurWidth, BlurHeight, 1.0f / BlurWidth, 1.0f / BlurHeight);
	Cmd.Constants.Write<CBPost>().dof_blur_params.set(1.0f / Width, 1.0f / Height, PixelScale, DofMaxRadius(Kernel));
	const float TileWidth = float(DofTileMax->dwWidth);
	const float TileHeight = float(DofTileMax->dwHeight);
	Cmd.Constants.Write<CBPost>().dof_tile_params.set(TileWidth, TileHeight, 1.0f / TileWidth, 1.0f / TileHeight);
	Viewport.Width = BlurWidth;
	Viewport.Height = BlurHeight;
	Cmd.SetViewport(Viewport);
	{
		GPU_EVENT(dof_downsample);
		u_setrt(Cmd, DofDownsample, rt_dof_blur3, DofDownsampleDepth, nullptr);
		DrawPassSQ(Cmd, s_dof_coc, 3, [&]
		{
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.SetStencil(false);
			Cmd.Constants.Write<CBPost>().dof_downsample_params.set(Width, Height, BlurWidth, BlurHeight);
		});
	}

	Viewport.Width = TileWidth;
	Viewport.Height = TileHeight;
	Cmd.SetViewport(Viewport);
	{
		GPU_EVENT(dof_tile_max);
		DrawSQ(Cmd, s_dof_coc, DofTileMax, 2);
	}
	{
		GPU_EVENT(dof_tile_dilate);
		DrawSQ(Cmd, s_dof_coc, DofTileDilated, 5);
	}

	Viewport.Width = BlurWidth;
	Viewport.Height = BlurHeight;
	Cmd.SetViewport(Viewport);
	{
		GPU_EVENT(dof_far_vertical);
		u_setrt(Cmd, rt_dof_blur1, rt_dof_blur2, DofReal1, DofImaginary1, nullptr);
		DrawPassSQ(Cmd, s_dof_blur1, 0);
	}
	{
		GPU_EVENT(dof_far_horizontal);
		// The resolve reads only complex intermediates; preparation is no longer needed.
		DrawSQ(Cmd, s_dof_blur1, rt_dof_blur3, 1);
	}
	{
		GPU_EVENT(dof_near_vertical);
		u_setrt(Cmd, rt_dof_blur1, rt_dof_blur2, nullptr, nullptr);
		DrawPassSQ(Cmd, s_dof_blur1, 2);
	}
	{
		GPU_EVENT(dof_near_horizontal);
		DrawSQ(Cmd, s_dof_blur1, DofDownsample, 3);
	}

	Viewport.Width = Width;
	Viewport.Height = Height;
	Cmd.SetViewport(Viewport);

	ResolveSurface(rt_dof_focus_prev, rt_dof_focus);
	ResolveSurface(rt_dof_coc_prev, rt_dof_coc);
}

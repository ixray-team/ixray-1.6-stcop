/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "ixr.h"
#include "UpscaleNri.h"
#include "EditorDrawContext.h"

// The upscalers NRI drives take the motion in pixels and the depth as a plain texture: one pass over the screen
// writes both from what the frame has. The frame's own velocity is in clip space, in a format FSR does not take
bool CRenderTarget::PhaseUpscaleNri(GfxCmdList& Cmd)
{
	GPU_EVENT(UpscaleNri);

	const u32 ViewWidth = u32(GfxCmdList::GetTargetWidth());
	const u32 ViewHeight = u32(GfxCmdList::GetTargetHeight());

	if (!UpscaleNri::IsReady() && !UpscaleNri::Sync(ViewWidth, ViewHeight))
	{
		return false;
	}

	// The upscaler is made for the view's size, again once a new size has rested as the interface does: until then the
	// frame is scaled plainly, and the next upscaled frame starts its history anew
	const bool IsRested = Device.IsEditorMode() || (int(ViewWidth) == g_UiView.x && int(ViewHeight) == g_UiView.y);

	if (!UpscaleNri::IsSized(ViewWidth, ViewHeight) && !IsRested)
	{
		phase_scale(Cmd);
		return true;
	}

	if (!UpscaleNri::Resize(ViewWidth, ViewHeight))
	{
		return false;
	}

	const u32 Width = u32(GfxCmdList::GetWidth());
	const u32 Height = u32(GfxCmdList::GetHeight());

	// Made for the largest render size: the frame writes the view's part of them
	if (!rt_UpscaleMotion)
	{
		CRT::MarkViewSized = true;
		rt_UpscaleMotion.create("$user$upscale_motion", u32(AllocHalfTarget.x), u32(AllocHalfTarget.y), ERHI_FORMAT::R16G16_FLOAT);
		rt_UpscaleDepth.create("$user$upscale_depth", u32(AllocHalfTarget.x), u32(AllocHalfTarget.y), ERHI_FORMAT::R32_FLOAT);
		CRT::MarkViewSized = false;
		UpscaleGuides.create("upscale_guides");
	}

	{
		GPU_EVENT(UpscaleGuides);
		u_setrt(Cmd, rt_UpscaleMotion, rt_UpscaleDepth, nullptr);
		Cmd.SetPass(UpscaleGuides->E[0]);
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
		Cmd.SetStencil(false);
		DrawFullscreen(Cmd, 3);
		u_setrt(Cmd, Width, Height, nullptr, nullptr, nullptr, nullptr);
	}

	static Fvector LastPosition = {};
	static Fvector LastDirection = {};
	static u32 LastFrame = 0;
	const bool IsCut = RFrame.dwFrame != LastFrame + 1 || LastPosition.distance_to_sqr(RFrame.vCameraPosition) > 25.f || LastDirection.dotproduct(RFrame.vCameraDirection) < 0.5f;
	LastPosition.set(RFrame.vCameraPosition);
	LastDirection.set(RFrame.vCameraDirection);
	LastFrame = RFrame.dwFrame;

	UpscaleNri::Frame Frame = {};
	Frame.Colour = rt_Generic_0->pSurface;
	Frame.Motion = rt_UpscaleMotion->pSurface;
	Frame.Depth = rt_UpscaleDepth->pSurface;
	Frame.Reactive = ReactiveMask->pSurface;
	Frame.Output = rt_Generic->pRT;
	Frame.Width = Width;
	Frame.Height = Height;
	Frame.JitterX = ps_r_taa_jitter_full.x;
	Frame.JitterY = ps_r_taa_jitter_full.y;
	Frame.Near = RFrame.fViewportNear;
	Frame.Far = RFrame.Env().far_plane;
	Frame.Fov = deg2rad(RFrame.fFOV);
	Frame.Milliseconds = std::max(1.0f + EPS_L, float(RFrame.dwTimeDelta));
	Frame.IsReset = Device.IsEditorMode() ? GEditorDraw.UpscalerReset : IsCut || RFrame.IsCameraCut();
	return UpscaleNri::Draw(Frame);
}

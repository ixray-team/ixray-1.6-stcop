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
#pragma once

// The temporal upscalers NRI drives: DLSS, FSR and XeSS on D3D12. D3D11 keeps FSR 3 through its own wrapper
namespace UpscaleNri
{
	struct Frame
	{
		IRHISurface* Colour;
		IRHISurface* Motion; // in pixels, from this frame to the one before
		IRHISurface* Depth;
		IRHISurface* Reactive;
		IRHIRenderTargetView* Output;
		u32 Width;
		u32 Height;
		float JitterX;
		float JitterY;
		float Near;
		float Far;
		float Fov;
		float Milliseconds;
		bool IsReset;
	};

	bool IsWanted();
	// r_upscaler: 2 DLSS, 3 FSR, 4 XeSS; D3D11 has FSR 3 alone
	bool IsSupported(u32 Mode);
	// Makes the upscaler of the chosen kind and quality for an output of this size
	bool Sync(u32 Width, u32 Height);
	// The quality is taken when the video restarts: a frame draws with the upscaler that restart made
	bool IsReady();
	// The output follows the view with the kind and quality the restart chose
	bool IsSized(u32 Width, u32 Height);
	bool Resize(u32 Width, u32 Height);
	bool RenderSize(u32& Width, u32& Height);
	bool Draw(const Frame& Frame);
	// The frames still named by the upscaler hold the targets a reset is about to make again
	void ReleaseFrames();
	void Destroy();
} // namespace UpscaleNri

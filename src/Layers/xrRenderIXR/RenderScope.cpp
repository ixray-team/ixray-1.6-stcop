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

#include "RenderScope.h"

RenderScope::RenderScope(GfxCmdList& Cmd)
	: Cmd(Cmd)
	, OuterDepth(Cmd.GetDepthStencil())
{
	for (u32 Index = 0; Index < 4; ++Index)
	{
		OuterColor[Index] = Cmd.GetRenderTarget(Index);
	}
}

RenderScope::RenderScope(GfxCmdList& Cmd, std::nullptr_t)
	: RenderScope(Cmd)
{
	IsApplied = false;
	Cmd.SetRenderTargets({}, nullptr);
}

RenderScope::RenderScope(GfxCmdList& Cmd, IRHIRenderTargetView* Color, IRHIDepthStencilView* Depth, u32 Width, u32 Height)
	: RenderScope(Cmd)
{
	Cmd.SetRenderTargets({ Color }, Depth);
	Cmd.ApplyRenderTargets();

	RHIViewport Viewport = { 0.f, 0.f, float(Width), float(Height), 0.f, 1.f };
	Cmd.SetViewport(Viewport);
}

RenderScope::~RenderScope()
{
	Cmd.SetRenderTargets({ OuterColor[0], OuterColor[1], OuterColor[2], OuterColor[3] }, OuterDepth);

	if (IsApplied)
	{
		Cmd.ApplyRenderTargets();
	}
}

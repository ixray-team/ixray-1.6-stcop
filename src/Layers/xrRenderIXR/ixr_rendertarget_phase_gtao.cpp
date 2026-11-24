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

#include "ixr_rendertarget.h"

void CRenderTarget::phase_gtao(GfxCmdList& Cmd)
{
	GPU_EVENT(phase_gtao);

	float p_scale = Cmd.GetHeight() / (tan(deg2rad(RFrame.fFOV) * 0.5f) * 2.0f);
	p_scale *= 0.5;

	{
		GPU_EVENT(gtao_render);

		RHIViewport Viewport(0.0f, 0.0f, (float)rt_gtao_0->dwWidth, (float)rt_gtao_0->dwHeight);
		Cmd.SetViewport(Viewport);

		DrawSQ(Cmd, s_gtao, rt_gtao_0, 0, [&]
		{
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.SetStencil(false);
			Cmd.Constants.Write<CBPost>().gtao_parameters = p_scale;
		});

		Viewport.Width = (float)Cmd.GetWidth();
		Viewport.Height = (float)Cmd.GetHeight();
		Cmd.SetViewport(Viewport);
	}

	{
		GPU_EVENT(gtao_filter);

		// D3D11 drops the SRV of a resource still bound as a render target
		u_setrt(Cmd, (u32)Cmd.GetWidth(), (u32)Cmd.GetHeight(), nullptr, nullptr, nullptr, nullptr);

		SPass& P = *(s_gtao->E[1]->passes[0]);
		Cmd.SetStates(P.state);
		Cmd.SetConstantTable(P.constants);
		Cmd.SetTextures(P.T);

		const u32 GroupsX = (rt_gtao_0->dwWidth + 7u) / 8u;
		const u32 GroupsY = (rt_gtao_0->dwHeight + 7u) / 8u;
		const NriBinding Occlusion = { 0, rt_ssao_temp->pSurface };

		RImplementation.NriPasses.Dispatch(Cmd, P.cs._get(), nullptr, 0, &Occlusion, 1, GroupsX, GroupsY, 1);
	}
}

void CRenderTarget::pharse_velocity(GfxCmdList& Cmd)
{
	GPU_EVENT(pharse_velocity);

	DrawSQ(Cmd, s_gtao, rt_Velocity, 2, [&]
	{
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	});
}

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

void CRenderTarget::phase_taa(GfxCmdList& Cmd)
{
	if (RFrame.IsCameraCut())
	{
		ResolveSurface(rt_Generic_0_prev, rt_Generic_0);
		return;
	}

	DrawSQ(Cmd, s_taa, rt_Generic_2, 0, [&]
	{
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	});

	ResolveSurface(rt_Generic_0_prev, rt_Generic_2);
	ResolveSurface(rt_Generic_0, rt_Generic_2);
}

void CRenderTarget::phase_mblur(GfxCmdList& Cmd)
{
	const float Power = Device.Grade(4, ps_r4_mblur_power);
	if (Power < EPS || !ps_r4_mblur_quality || RFrame.IsCameraCut())
		return;

	GPU_EVENT(PhaseMBlur);

	for (u32 i = 1; i <= ps_r4_mblur_quality; ++i)
	{
		DrawSQ(Cmd, s_taa, rt_Generic_2, 1, [&]
		{
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.Constants.Write<CBPass>().mblur_params.set(Power / i, i, 1.0f / dwWidth, 1.0f / dwHeight);
		});
		ResolveSurface(rt_Generic_0, rt_Generic_2);
	}
}

void CRenderTarget::phase_depth_upscale(GfxCmdList& Cmd)
{
	GPU_EVENT(phase_depth_upscale);
	Fmatrix invVP_old;
	invVP_old.invert44(RFrame.mFullTransform_old);

	DrawSQ(Cmd, s_taa, rt_upscaled_depth, 2, [&]
	{
		Cmd.Constants.Write<CBView>().m_invVP_old = invVP_old;
	});

	ResolveSurface(rt_upscaled_depth_old, rt_upscaled_depth);
}

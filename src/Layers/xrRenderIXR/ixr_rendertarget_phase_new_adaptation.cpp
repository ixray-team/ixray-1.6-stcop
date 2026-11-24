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
#include "blender_new_adaptation.h"
#include "blender_bloom_downsample.h"

RHIViewport VP_NL = {
	0.0f,
	0.0f,
	1024.f,
	1024.f,
	0.0f,
	1.0f
};

// What measures the exposure is compiled for what the scene holds: after a change of it they are made again, and the
// stored exposure, which means something else now, starts over
void CRenderTarget::ResetExposure(GfxCmdList& Cmd)
{
	CreateEffectSQ<CBlender_new_adaptation>(s_lum_copy);
	CreateEffectSQ<CBlender_bloom_downsample>(s_bloom_downsample);
	Cmd.ClearTarget(rt_LUM_D->pRT, ERTColor::Gray);
	IsExposureFresh = true;
}

void CRenderTarget::phase_new_luminance(GfxCmdList& Cmd)
{
	GPU_EVENT(phase_new_luminance);

	ref_rt* targets[] = { &rt_LUM_A, &rt_LUM_B, &rt_LUM_C };
	for (u32 i = 0; i < std::size(targets); ++i)
	{
		DrawSQ(Cmd, s_lum_copy, *targets[i], i, [&]
		{
			RImplementation.rmNormal();
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.SetStencil(false);
			Cmd.Constants.Write<CBPost>().adapt_params.set(float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));
		});
	}

	DrawSQ(Cmd, s_lum_copy, rt_LUM_D, 3, [&]
	{
		RImplementation.rmNormal();
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
		Cmd.SetStencil(false);

		// The first frame measured takes its exposure whole: what the target was cleared to is no exposure
		Cmd.Constants.Write<CBPost>().adapt_params.set(0.0f, 0.0f, RFrame.IsCameraCut() || IsExposureFresh ? 1.0f : 1.0f - exp(-Device.fTimeDeltaSmoothing / ps_r2_autoexposure_speed), 0.0f);
		IsExposureFresh = false;
	});
}

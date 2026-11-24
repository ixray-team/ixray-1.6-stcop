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

void CRenderTarget::phase_bloom_downsample(GfxCmdList& Cmd, u32 Levels)
{
	GPU_EVENT(phase_bloom_downsample);

	ref_rt* targets[] = { &rt_Bloom_A, &rt_Bloom_B, &rt_Bloom_C, &rt_Bloom_D, &rt_Bloom_E, &rt_Bloom_F, &rt_Bloom_G };
	const u32 elements[] = { 0, 1, 2, 3, 4, 5, 5 };

	for (u32 i = 0; i < std::min<u32>(Levels, u32(std::size(targets))); ++i)
	{
		DrawSQ(Cmd, s_bloom_downsample, *targets[i], elements[i], [&]
		{
			RImplementation.rmNormal();
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.SetStencil(false);
			Cmd.Constants.Write<CBPost>().downsample_params.set(float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));
		});
	}
}

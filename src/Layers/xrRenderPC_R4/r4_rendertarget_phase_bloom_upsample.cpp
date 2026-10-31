#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_bloom_upsample(GfxCmdList& Cmd)
{
	GPU_EVENT(phase_bloom_upsample);

	ref_rt* targets[] = { &rt_Bloom_F2, &rt_Bloom_E2, &rt_Bloom_D2, &rt_Bloom_C2, &rt_Bloom_B2, &rt_Bloom_A2 };

	for (u32 i = 0; i < std::size(targets); ++i)
	{
		DrawSQ(Cmd, s_bloom_upsample, *targets[i], i, [&]
		{
			RImplementation.rmNormal();
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.SetStencil(false);
			Cmd.Constants.Write<CBPost>().upsample_params.set(float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));
		});
	}
}

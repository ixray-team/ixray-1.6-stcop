#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_bloom_downsample(GfxCmdList& Cmd)
{
	GPU_EVENT(phase_bloom_downsample);

	ref_rt* targets[] = { &rt_Bloom_A, &rt_Bloom_B, &rt_Bloom_C, &rt_Bloom_D, &rt_Bloom_E, &rt_Bloom_F, &rt_Bloom_G };
	const u32 elements[] = { 0, 1, 2, 3, 4, 5, 5 };

	for (u32 i = 0; i < std::size(targets); ++i)
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

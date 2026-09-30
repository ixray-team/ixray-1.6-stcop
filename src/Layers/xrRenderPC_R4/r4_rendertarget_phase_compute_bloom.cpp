#include "stdafx.h"
#include "r4_rendertarget.h"
#include "blender_compute_bloom.h"

void CRenderTarget::create_compute_bloom(u32 width, u32 height)
{
	CBlender_compute_bloom blender;
	string64 down_names[9], up_names[9];
	// Work backwards from level I: all adjacent levels remain exactly 2:1,
	// including odd screen dimensions. Normalized UVs cover the whole source.
	width = ((width + 511) / 512) * 256;
	height = ((height + 511) / 512) * 256;
	for (u32 i = 0; i < 9; ++i)
	{
		xr_sprintf(down_names[i], "$user$compute_bloom_down_%u", i);
		xr_sprintf(up_names[i], "$user$compute_bloom_up_%u", i);
		rt_compute_bloom_down[i].create(down_names[i], width, height, ERHI_FORMAT::R11G11B10_FLOAT, 1, CRT::USE_UAV_FLAG);
		rt_compute_bloom_up[i].create(up_names[i], width, height, ERHI_FORMAT::R11G11B10_FLOAT, 1, CRT::USE_UAV_FLAG);
		width /= 2;
		height /= 2;
	}
	for (u32 i = 0; i < 9; ++i)
	{
		string256 textures;
		xr_sprintf(textures, "%s,%s,%s", i ? down_names[i - 1] : r2_RT_generic,
			up_names[i < 8 ? i + 1 : i], down_names[i]);
		s_compute_bloom[i].create(&blender, nullptr, textures);
	}
}

void CRenderTarget::phase_compute_bloom()
{
	GPU_EVENT(phase_compute_bloom);
	u_setrt(get_target_width(), get_target_height(), nullptr, nullptr, nullptr, nullptr);
	const u32 levels = ps_r4_bloom_compute_levels;
	ID3D11UnorderedAccessView* null_uav = nullptr;

	auto dispatch = [&](u32 level, u32 element, const ref_rt& target, const char* params)
	{
		SPass& P = *s_compute_bloom[level]->E[element]->passes[0];
		RCache.set_States(P.state);
		RCache.set_Constants(P.constants);
		RCache.set_Textures(P.T);
		RCache.set_CS(P.cs);
		GRHI->ShaderResourceCache->Apply();
		RCache.set_c(params, float(target->dwWidth), float(target->dwHeight),
			1.0f / float(target->dwWidth), 1.0f / float(target->dwHeight));
		ID3D11UnorderedAccessView* uav = reinterpret_cast<ID3D11UnorderedAccessView*>(target->pUAView->GetRaw());
		RContext->CSSetUnorderedAccessViews(0, 1, &uav, nullptr);
		RCache.Compute((target->dwWidth + 7) / 8, (target->dwHeight + 7) / 8, 1);
		RContext->CSSetUnorderedAccessViews(0, 1, &null_uav, nullptr);
	};

	{
		GPU_EVENT(compute_bloom_downsample);
		for (u32 i = 0; i < levels; ++i)
		{
			GPU_EVENT(compute_bloom_downsample_level);
			dispatch(i, 0, rt_compute_bloom_down[i], "downsample_params");
		}
		// Seed the reconstruction at the selected smallest level.
		GRHI->CopySurface(rt_compute_bloom_up[levels - 1]->pSurface, rt_compute_bloom_down[levels - 1]->pSurface);
	}
	{
		GPU_EVENT(compute_bloom_upsample);
		for (int i = int(levels) - 2; i >= 0; --i)
		{
			GPU_EVENT(compute_bloom_upsample_level);
			dispatch(i, 1, rt_compute_bloom_up[i], "upsample_params");
		}
	}
}

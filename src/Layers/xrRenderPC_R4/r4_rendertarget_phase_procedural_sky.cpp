#include "stdafx.h"
#include "r4_rendertarget.h"
#include "../xrRender/dxRenderDeviceRender.h"
#include "../../../gamedata/shaders/d3d11/atmosphere_config.h"

namespace
{
void SetupComputePass(const ref_shader& shader, u32 element_index)
{
	ShaderElement* element = &*(shader->E[element_index]);
	SPass& pass = *(element->passes[0]);

	RCache.set_States(pass.state);
	RCache.set_Constants(pass.constants);
	RCache.set_Textures(pass.T);
	RCache.set_CS(pass.cs);
	// Flush SRV changes before binding UAVs or recreating their surfaces.
	GRHI->ShaderResourceCache->Apply();
}
/*
void UnbindComputeResources(u32 uav_count)
{
	ID3D11UnorderedAccessView* null_uavs[2] = {};
	ID3D11ShaderResourceView* null_srvs[16] = {};
	UINT initial_counts[2] = {};

	RContext->CSSetUnorderedAccessViews(0, uav_count, null_uavs, initial_counts);
	RContext->CSSetShaderResources(0, static_cast<UINT>(std::size(null_srvs)), null_srvs);
}
*/
void UnbindComputeResources(u32 uav_count)
{
	ID3D11UnorderedAccessView* null_uavs[3] = {};
	UINT initial_counts[3] = {};

	// SRV не трогаем: LUT остаются привязанными для следующего compute pass.
	RContext->CSSetUnorderedAccessViews(
		0,
		uav_count,
		null_uavs,
		initial_counts
	);
}
} // namespace

void CRenderTarget::create_aerial_perspective(u32 width, u32 height)
{
	const u32 w = (width + SKY_AP_DOWNSAMPLE - 1) / SKY_AP_DOWNSAMPLE;
	const u32 h = (height + SKY_AP_DOWNSAMPLE - 1) / SKY_AP_DOWNSAMPLE;
	if (aerial_width == w && aerial_height == h)
		return;
	const char* names[3] = { r4_RT_aerial_perspective, r4_RT_aerial_direct, r4_RT_aerial_transmittance };
	RHITextureDesc desc = {};
	desc.Width = w;
	desc.Height = h;
	desc.Depth = SKY_AP_DEPTH;
	desc.MipLevels = 1;
	desc.Format = ERHI_FORMAT::R16G16B16A16_FLOAT;
	desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
	desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE | ERHI_BIND_FLAG::UNORDERED_ACCESS;
	RHIUAVDesc uav_desc = {};
	uav_desc.Format = desc.Format;
	uav_desc.ViewDimension = ERHI_VIEW_DIMENSION::Texture3D;
	uav_desc.WSize = SKY_AP_DEPTH;
	for (u32 i = 0; i < 3; ++i)
	{
		_RELEASE(u_procedural_aerial_perspective[i]);
		if (t_procedural_aerial_perspective[i])
			t_procedural_aerial_perspective[i]->surface_set(nullptr);
		_RELEASE(s_procedural_aerial_perspective[i]);
		s_procedural_aerial_perspective[i] = GRHI->CreateTexture3D(desc, nullptr);
		R_ASSERT(s_procedural_aerial_perspective[i]);
		t_procedural_aerial_perspective[i] = dxRenderDeviceRender::Instance().Resources->_CreateTexture(names[i]);
		t_procedural_aerial_perspective[i]->surface_set(s_procedural_aerial_perspective[i]);
		u_procedural_aerial_perspective[i] = GRHI->CreateUAV(s_procedural_aerial_perspective[i], uav_desc);
		R_ASSERT(u_procedural_aerial_perspective[i]);
	}
	aerial_width = w;
	aerial_height = h;
	clouds_history_valid = false;
}

void CRenderTarget::phase_procedural_sky()
{
	GPU_EVENT(phase_procedural_sky);
	// The first pass reads only baked LUTs; its normal bindings remove previous
	// readers of the generated sky/AP textures before UAV writes/reallocation.
	SetupComputePass(s_procedural_sky, 0);
	create_aerial_perspective(rt_Generic_0->dwWidth, rt_Generic_0->dwHeight);

	ID3D11UnorderedAccessView* uav = nullptr;
	UINT initial_count = 0;

	// 1. Sky-view LUT: 200x100, [numthreads(8, 4, 1)].
	{
		GPU_EVENT(compute_sky_view);
		uav = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_procedural_sky_view->pUAView->GetRaw());
		RContext->CSSetUnorderedAccessViews(0, 1, &uav, &initial_count);
		RCache.Compute(25, 25, 1);
		UnbindComputeResources(1);
	}

	// 2. AP: viewport/20, 64 depth slices integrated sequentially per XY thread.
	{
		GPU_EVENT(compute_aerial_perspective);
		SetupComputePass(s_procedural_sky, 1);
		ID3D11UnorderedAccessView* views[3] = {};
		UINT counts[3] = {};
		for (u32 i = 0; i < 3; ++i)
			views[i] = reinterpret_cast<ID3D11UnorderedAccessView*>(u_procedural_aerial_perspective[i]->GetRaw());
		RContext->CSSetUnorderedAccessViews(0, 3, views, counts);
		RCache.Compute((aerial_width + 7) / 8, (aerial_height + 3) / 4, 1);
		UnbindComputeResources(3);
	}

	// 3. Sky-view LUT -> full octahedral map
	// 512x512 core + 4px padding = 520x520.
	{
		GPU_EVENT(compute_sky_octo);

		SetupComputePass(s_procedural_sky, 2);

		uav = reinterpret_cast<ID3D11UnorderedAccessView*>(
			rt_procedural_sky_octo->pUAView->GetRaw()
		);


		RContext->CSSetUnorderedAccessViews(0, 1, &uav, &initial_count);
		// ceil(520 / 8), ceil(520 / 4)
		RCache.Compute(65, 130, 1);
		UnbindComputeResources(1);
	}

	// 4. 512 core -> 128 core:
	// full padding 4px, middle padding 1px.
	{
		GPU_EVENT(compute_sky_octo_downsample_middle);

		SetupComputePass(s_procedural_sky, 3);

		uav = reinterpret_cast<ID3D11UnorderedAccessView*>(
			rt_procedural_sky_octo_middle->pUAView->GetRaw()
		);

		RCache.set_c(
			"sky_octo_filter_params",
			4.0f, // input padding
			1.0f, // output padding
			0.0f, // no additional blur
			0.0f  // box filter
		);

		RContext->CSSetUnorderedAccessViews(0, 1, &uav, &initial_count);
		// ceil(130 / 8), ceil(130 / 4)
		RCache.Compute(17, 33, 1);
		UnbindComputeResources(1);
	}

	// 5. 128 core -> blurred 32 core:
	// both maps have 1px padding.
	{
		GPU_EVENT(compute_sky_octo_downsample_small);

		SetupComputePass(s_procedural_sky, 4);

		uav = reinterpret_cast<ID3D11UnorderedAccessView*>(
			rt_procedural_sky_octo_small->pUAView->GetRaw()
		);

		RCache.set_c(
			"sky_octo_filter_params",
			1.0f, // input padding
			1.0f, // output padding
			1.5f, // blur radius in small-map texels
			1.0f  // 5x5 blur filter
		);

		RContext->CSSetUnorderedAccessViews(0, 1, &uav, &initial_count);
		// ceil(34 / 8), ceil(34 / 4)
		RCache.Compute(5, 9, 1);
		UnbindComputeResources(1);
	}
	{
		GPU_EVENT(compute_sky_diffuse_irradiance);

		// Element 3 already binds the full 520x520 octomap.
		SetupComputePass(s_procedural_sky, 5);

		uav = reinterpret_cast<ID3D11UnorderedAccessView*>(
			rt_procedural_sky_octo_diffuse->pUAView->GetRaw()
		);

		RCache.set_c(
			"sky_octo_filter_params",
			1.0f, // small-map input padding
			1.0f, // diffuse-map output padding
			0.0f,
			2.0f // Lambertian irradiance mode
		);

		RContext->CSSetUnorderedAccessViews(
			0,
			1,
			&uav,
			&initial_count
		);

		RCache.Compute(5, 9, 1);
		UnbindComputeResources(1);
	}
}

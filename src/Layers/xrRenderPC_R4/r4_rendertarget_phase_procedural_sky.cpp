#include "stdafx.h"
#include "r4_rendertarget.h"

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
	ID3D11UnorderedAccessView* null_uavs[2] = {};
	UINT initial_counts[2] = {};

	// SRV не трогаем: LUT остаются привязанными для следующего compute pass.
	RContext->CSSetUnorderedAccessViews(
		0,
		uav_count,
		null_uavs,
		initial_counts
	);
}
} // namespace

void CRenderTarget::phase_procedural_sky()
{
	GPU_EVENT(phase_procedural_sky);

	ID3D11UnorderedAccessView* uav = nullptr;
	UINT initial_count = 0;

	// 1. Sky-view LUT: 200x100, [numthreads(8, 4, 1)].
	{
		GPU_EVENT(compute_sky_view);
		SetupComputePass(s_procedural_sky, 0);
		uav = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_procedural_sky_view->pUAView->GetRaw());
		RContext->CSSetUnorderedAccessViews(0, 1, &uav, &initial_count);
		RCache.Compute(25, 25, 1);
		UnbindComputeResources(1);
	}

	// 2. Aerial perspective: 32x32x32, [numthreads(4, 4, 2)].
	{
		GPU_EVENT(compute_aerial_perspective);
		SetupComputePass(s_procedural_sky, 1);
		Fmatrix inverse_view_projection;
		inverse_view_projection.invert(Device.mFullTransform);

		constexpr float kAerialPerspectiveMaxDistanceKm = 32.0f;

		RCache.set_c("sky_aerial_max_distance", kAerialPerspectiveMaxDistanceKm);
		uav = reinterpret_cast<ID3D11UnorderedAccessView*>(u_procedural_aerial_perspective->GetRaw());

		RContext->CSSetUnorderedAccessViews(0, 1, &uav, &initial_count);
		RCache.Compute(4, 8, 1);
		UnbindComputeResources(1);
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
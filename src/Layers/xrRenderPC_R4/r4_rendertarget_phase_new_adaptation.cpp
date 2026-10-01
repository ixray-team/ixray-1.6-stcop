#include "stdafx.h"

#include "r4_rendertarget.h"

D3D_VIEWPORT VP_NL = {
	0.0f,
	0.0f,
	1024.f,
	1024.f,
	0.0f,
	1.0f
};

void CRenderTarget::phase_compute_luminance()
{
	GPU_EVENT(phase_compute_luminance);

	// Keep both adaptation paths running for comparison in combine_2.
	u_setrt(get_target_width(), get_target_height(), nullptr, nullptr, nullptr, nullptr);
	ID3D11UnorderedAccessView* null_uav = nullptr;
	const UINT clear_value[4] = {};

	{
		GPU_EVENT(compute_luminance_histogram);
		SPass& P = *s_lum_copy->E[4]->passes[0];
		RCache.set_States(P.state);
		RCache.set_Constants(P.constants);
		RCache.set_Textures(P.T);
		RCache.set_CS(P.cs);
		// Remove previous SRV bindings through the cache before UAV writes.
		GRHI->ShaderResourceCache->Apply();

		ID3D11UnorderedAccessView* uav = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_LUM_histogram->pUAView->GetRaw());
		RContext->ClearUnorderedAccessViewUint(uav, clear_value);
		RContext->CSSetUnorderedAccessViews(0, 1, &uav, nullptr);
		RCache.Compute((get_target_width() + 15) / 16, (get_target_height() + 15) / 16, 1);
		RContext->CSSetUnorderedAccessViews(0, 1, &null_uav, nullptr);
	}

	{
		GPU_EVENT(compute_luminance_reduce);
		SPass& P = *s_lum_copy->E[5]->passes[0];
		RCache.set_States(P.state);
		RCache.set_Constants(P.constants);
		RCache.set_Textures(P.T);
		RCache.set_CS(P.cs);
		GRHI->ShaderResourceCache->Apply();

		float alpha = 1.0f;
		if (compute_luminance_valid && ps_r2_autoexposure_speed > 0.f)
			alpha = 1.0f - exp(-Device.fTimeDeltaSmoothing / ps_r2_autoexposure_speed);
		float range_alpha = compute_luminance_valid ? 1.0f - exp(-Device.fTimeDeltaSmoothing / 0.5f) : 1.0f;
		RCache.set_c("adapt_params", alpha, range_alpha, 0.f, 0.f);
		RCache.set_c("autoexposure_params", ps_r2_autoexposure_key, ps_r2_autoexposure_min, ps_r2_autoexposure_max, ps_r2_autoexposure_bias);

		ID3D11UnorderedAccessView* uavs[2] = {
			reinterpret_cast<ID3D11UnorderedAccessView*>(rt_LUM_compute->pUAView->GetRaw()),
			reinterpret_cast<ID3D11UnorderedAccessView*>(rt_Tonemap_state->pUAView->GetRaw())
		};
		RContext->CSSetUnorderedAccessViews(0, 2, uavs, nullptr);
		RCache.Compute(1, 1, 1);
		ID3D11UnorderedAccessView* null_uavs[2] = {};
		RContext->CSSetUnorderedAccessViews(0, 2, null_uavs, nullptr);
		compute_luminance_valid = true;
	}
}

void CRenderTarget::phase_histogram_debug()
{
    GPU_EVENT(phase_histogram_debug);
    u_setrt(get_target_width(), get_target_height(), nullptr, nullptr, nullptr, nullptr);
    {
        GPU_EVENT(combine_2_histogram);
        SPass& P = *s_histogram_debug->E[0]->passes[0];
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);
        GRHI->ShaderResourceCache->Apply();

        const UINT clear_value[4] = {};
        ID3D11UnorderedAccessView* uav = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_Histogram_debug->pUAView->GetRaw());
        RContext->ClearUnorderedAccessViewUint(uav, clear_value);
        RContext->CSSetUnorderedAccessViews(0, 1, &uav, nullptr);
        RCache.Compute((get_target_width() + 15) / 16, (get_target_height() + 15) / 16, 1);
        ID3D11UnorderedAccessView* null_uav = nullptr;
        RContext->CSSetUnorderedAccessViews(0, 1, &null_uav, nullptr);
    }
    {
        GPU_EVENT(combine_2_histogram_overlay);
        u_setrt(rt_Back_Buffer_AA, nullptr, nullptr, nullptr);
        RImplementation.rmNormal();
        GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
        RCache.set_Stencil(false);
        RCache.set_Element(s_histogram_debug->E[1]);
        RCache.set_Geometry(FSTriangleGeom);
        RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
        GRHI->CopySurface(rt_Back_Buffer->pSurface, rt_Back_Buffer_AA->pSurface);
    }
    u_setrt(rt_Back_Buffer, nullptr, nullptr, nullptr);
}

void CRenderTarget::phase_new_luminance()
{
	GPU_EVENT(phase_new_luminance);

	u_setrt(rt_LUM_A, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(ps_r4_bloom_mode == 1 ? s_lum_copy_compute->E[0] : s_lum_copy->E[0]);
	RCache.set_c("adapt_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_LUM_B, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_lum_copy->E[1]);
	RCache.set_c("adapt_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_LUM_C, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_lum_copy->E[2]);
	RCache.set_c("adapt_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_LUM_D, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_lum_copy->E[3]);

	RCache.set_c("adapt_params", ps_r2_autoexposure_min_weight, ps_r2_autoexposure_gaussian, 1.0f - exp(-Device.fTimeDeltaSmoothing / ps_r2_autoexposure_speed), 0.f);
	RCache.set_c("adapt_params2", ps_r2_autoexposure_soft_log_k, ps_r2_autoexposure_soft_limiter, ps_r2_autoexposure_sensitivity, 0.f);

	f_luminance_adapt = 0.9f * f_luminance_adapt + 0.1f * Device.fTimeDelta * ps_r2_tonemap_adaptation;

	Fvector3 Current, Result;

	Result.set(1, 0, 1);
	Current.set(ps_r2_tonemap_middlegray, 1.f, ps_r2_tonemap_low_lum);

	Result.lerp(Result, Current, ps_r2_tonemap_amount);

	RCache.set_c("MiddleGray", Result.x, Result.y, Result.z, f_luminance_adapt);

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
}

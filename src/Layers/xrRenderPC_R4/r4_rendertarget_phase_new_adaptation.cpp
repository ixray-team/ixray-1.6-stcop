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

void CRenderTarget::phase_new_luminance()
{
	GPU_EVENT(phase_new_luminance);

	u_setrt(rt_LUM_A, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_lum_copy->E[0]);
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
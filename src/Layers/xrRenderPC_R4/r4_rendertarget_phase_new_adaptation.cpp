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
	float rW, rH, W, H;

	W = 1024.f;//float(Device.TargetWidth / 32.0);
	H = 1024.f;//float(Device.TargetHeight / 32.0);
	rW = 1.0f / W;
	rH = 1.0f / H;

	VP_NL.Width = W;
	VP_NL.Height = H;
	RContext->RSSetViewports(1, &VP_NL);

	u_setrt(rt_LUM_A, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	RCache.set_Element(s_lum_copy->E[0]);
	RCache.set_c("adapt_params", W, H, rW, rH);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	//=================================================================

	W /= 8.f;//128
	H /= 8.f;//128
	rW = 1.0f / W;
	rH = 1.0f / H;

	VP_NL.Width = W;
	VP_NL.Height = H;
	RContext->RSSetViewports(1, &VP_NL);

	u_setrt(rt_LUM_B, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	RCache.set_Element(s_lum_copy->E[1]);
	RCache.set_c("adapt_params", W, H, rW, rH);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	//=================================================================

	W /= 8.f;//16
	H /= 8.f;//16
	rW = 1.0f / W;
	rH = 1.0f / H;

	VP_NL.Width = W;
	VP_NL.Height = H;
	RContext->RSSetViewports(1, &VP_NL);

	u_setrt(rt_LUM_C, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	RCache.set_Element(s_lum_copy->E[2]);
	RCache.set_c("adapt_params", W, H, rW, rH);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	//=================================================================

	W = 1.f;//1
	H = 1.f;//1
	rW = 1.0f / W;
	rH = 1.0f / H;

	VP_NL.Width = W;
	VP_NL.Height = H;
	RContext->RSSetViewports(1, &VP_NL);

	u_setrt(rt_LUM_D, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	RCache.set_Element(s_lum_copy->E[3]);
	W = Device.fTimeDelta;
	RCache.set_c("adapt_params", ps_r2_autoexposure_min_weight, ps_r2_autoexposure_gaussian, ps_r2_autoexposure_speed, 0.f);
	RCache.set_c("adapt_params2", ps_r2_autoexposure_soft_log_k, ps_r2_autoexposure_soft_limiter, ps_r2_autoexposure_sensitivity, 0.f);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	// Copy current luminance to previous for next frame
	GRHI->CopySurface(rt_LUM_Prev->pSurface, rt_LUM_D->pSurface);
}
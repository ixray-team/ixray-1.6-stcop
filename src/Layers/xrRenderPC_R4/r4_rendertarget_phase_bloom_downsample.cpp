#include "stdafx.h"

#include "r4_rendertarget.h"

D3D_VIEWPORT VP_BD = {
	0.0f,
	0.0f,
	1024.f,
	1024.f,
	0.0f,
	1.0f
};

void CRenderTarget::phase_bloom_downsample()
{
	//u32 Offset = 0;
	float rW, rH, W, H;
	u32 BW_G = Device.TargetWidth / 128, BH_G = Device.TargetHeight / 128; // ок
	u32 BW_F = BW_G * 2, BH_F = BH_G * 2;
	u32 BW_E = BW_F * 2, BH_E = BH_F * 2;
	u32 BW_D = BW_E * 2, BH_D = BH_E * 2;
	u32 BW_C = BW_D * 2, BH_C = BH_D * 2;
	u32 BW_B = BW_C * 2, BH_B = BH_C * 2;
	u32 BW_A = BW_B * 2, BH_A = BH_B * 2;

	W = BW_A;
	H = BH_A;
	rW = 1.0f / W;
	rH = 1.0f / H;

	VP_BD.Width = W;
	VP_BD.Height = H;
	RContext->RSSetViewports(1, &VP_BD);

	u_setrt(rt_Bloom_A, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	RCache.set_Element(s_bloom_downsample->E[0]);
	RCache.set_c("downsample_params", W, H, rW, rH);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	//==========================================================

	W = BW_B;
	H = BH_B;
	rW = 1.0f / W;
	rH = 1.0f / H;

	VP_BD.Width = W;
	VP_BD.Height = H;
	RContext->RSSetViewports(1, &VP_BD);

	u_setrt(rt_Bloom_B, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	RCache.set_Element(s_bloom_downsample->E[1]);
	RCache.set_c("downsample_params", W, H, rW, rH);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	//==========================================================

	W = BW_C;
	H = BH_C;
	rW = 1.0f / W;
	rH = 1.0f / H;

	VP_BD.Width = W;
	VP_BD.Height = H;
	RContext->RSSetViewports(1, &VP_BD);

	u_setrt(rt_Bloom_C, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	RCache.set_Element(s_bloom_downsample->E[2]);
	RCache.set_c("downsample_params", W, H, rW, rH);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	//==========================================================

	W = BW_D;
	H = BH_D;
	rW = 1.0f / W;
	rH = 1.0f / H;

	VP_BD.Width = W;
	VP_BD.Height = H;
	RContext->RSSetViewports(1, &VP_BD);

	u_setrt(rt_Bloom_D, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	RCache.set_Element(s_bloom_downsample->E[3]);
	RCache.set_c("downsample_params", W, H, rW, rH);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	//==========================================================

	W = BW_E;
	H = BH_E;
	rW = 1.0f / W;
	rH = 1.0f / H;

	VP_BD.Width = W;
	VP_BD.Height = H;
	RContext->RSSetViewports(1, &VP_BD);

	u_setrt(rt_Bloom_E, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	RCache.set_Element(s_bloom_downsample->E[4]);
	RCache.set_c("downsample_params", W, H, rW, rH);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	//==========================================================

	W = BW_F;
	H = BH_F;
	rW = 1.0f / W;
	rH = 1.0f / H;

	VP_BD.Width = W;
	VP_BD.Height = H;
	RContext->RSSetViewports(1, &VP_BD);

	u_setrt(rt_Bloom_F, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	RCache.set_Element(s_bloom_downsample->E[5]);
	RCache.set_c("downsample_params", W, H, rW, rH);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	//==========================================================

	W = BW_G;
	H = BH_G;
	rW = 1.0f / W;
	rH = 1.0f / H;

	VP_BD.Width = W;
	VP_BD.Height = H;
	RContext->RSSetViewports(1, &VP_BD);

	u_setrt(rt_Bloom_G, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	RCache.set_Element(s_bloom_downsample->E[5]);
	RCache.set_c("downsample_params", W, H, rW, rH);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
}
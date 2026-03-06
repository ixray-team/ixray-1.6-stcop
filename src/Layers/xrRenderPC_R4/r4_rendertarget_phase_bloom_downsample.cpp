#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_bloom_downsample()
{
	GPU_EVENT(phase_bloom_downsample)

	u_setrt(rt_Bloom_A, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_downsample->E[0]);
	RCache.set_c("downsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);

	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_Bloom_B, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_downsample->E[1]);
	RCache.set_c("downsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);

	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_Bloom_C, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_downsample->E[2]);
	RCache.set_c("downsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_Bloom_D, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_downsample->E[3]);
	RCache.set_c("downsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_Bloom_E, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_downsample->E[4]);
	RCache.set_c("downsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_Bloom_F, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_downsample->E[5]);
	RCache.set_c("downsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_Bloom_G, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_downsample->E[5]);
	RCache.set_c("downsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
}
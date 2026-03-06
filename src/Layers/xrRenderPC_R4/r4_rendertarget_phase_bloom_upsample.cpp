#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_bloom_upsample()
{
	GPU_EVENT(phase_bloom_upsample)

	u_setrt(rt_Bloom_F2, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_upsample->E[0]);
	RCache.set_c("upsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_Bloom_E2, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_upsample->E[1]);
	RCache.set_c("upsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_Bloom_D2, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_upsample->E[2]);
	RCache.set_c("upsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_Bloom_C2, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_upsample->E[3]);
	RCache.set_c("upsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_Bloom_B2, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_upsample->E[4]);
	RCache.set_c("upsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	u_setrt(rt_Bloom_A2, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	RCache.set_Element(s_bloom_upsample->E[5]);
	RCache.set_c("upsample_params", float(dwWidth), float(dwHeight), 1.0f / float(dwWidth), 1.0f / float(dwHeight));

	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
}
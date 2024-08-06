#include "stdafx.h"

void CRenderTarget::phase_smap_direct(light* L, u32 sub_phase)
{
	u_setrt(rt_smap_surf, nullptr, nullptr, rt_smap_depth->pZRT);
	g_RenderRHI->ClearDepthStencilView(rt_smap_depth->pZRT, D3D_CLEAR_DEPTH, 1.0f, 0L);

	//	Prepare viewport for shadow map rendering
	if(sub_phase != SE_SUN_RAIN_SMAP) {
		RImplementation.rmNormal();
	}
	else {
		D3D_VIEWPORT VP = {
			(float)L->X.D.minX,(float)L->X.D.minY,
			(float)(L->X.D.maxX - L->X.D.minX),
			(float)(L->X.D.maxY - L->X.D.minY),
			0.0f, 1.0f
		};

		RContext->RSSetViewports(1, &VP);
	}

	// Stencil	- disable
	RCache.set_Stencil(FALSE);
}

void CRenderTarget::phase_smap_direct_tsh(light* L, u32 sub_phase)
{
	VERIFY(RImplementation.o.Tshadows);
	FLOAT ColorRGBA[4] = {1.0f, 1.0f, 1.0f, 1.0f};
	RCache.set_ColorWriteEnable();
	//	Prepare viewport for shadow map rendering
	RImplementation.rmNormal();
	g_RenderRHI->ClearRenderTargetView(RCache.get_RT(0), ColorRGBA);
}

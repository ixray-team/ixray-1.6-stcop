#include "stdafx.h"

void CRenderTarget::phase_smap_direct(light* L, u32 sub_phase)
{
	u_setrt(nullptr, nullptr, nullptr, (ID3DDepthStencilView*)rt_smap_depth->pZRT->GetRawDSV());
	RContext->ClearDepthStencilView((ID3DDepthStencilView*)rt_smap_depth->pZRT->GetRawDSV(), D3D_CLEAR_DEPTH, 1.0f, 0L);

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
	RCache.set_ColorWriteEnable();

	//	Prepare viewport for shadow map rendering
	RImplementation.rmNormal();
	GRHI->ClearRawTarget(RCache.get_RT(), ERTColor::Black);
}

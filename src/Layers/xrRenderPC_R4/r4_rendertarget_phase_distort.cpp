#include "stdafx.h"

void CRenderTarget::phase_distort()
{
	bool _menu_pp = g_pGamePersistent ? g_pGamePersistent->OnRenderPPUI_query() : false;

	// Distortion filter
	BOOL bDistort = RImplementation.o.distortion_enabled;				// This can be modified
	{
		if ((0 == RImplementation.mapDistort.size()) && !_menu_pp)
			bDistort = FALSE;
		if (bDistort)
		{
			PIX_EVENT(render_distort_objects);
			FLOAT ColorRGBA_[4] = { 127.0f / 255.0f, 127.0f / 255.0f, 0.0f, 127.0f / 255.0f };

			u_setrt(rt_Distort, 0, 0, rt_HWDepth->pZRT);		// Now RT is a distortion mask
			HW.pContext->ClearRenderTargetView(rt_Distort->pRT, ColorRGBA_);

			RCache.set_CullMode(CULL_CCW);
			RCache.set_Stencil(FALSE);
			RCache.set_ColorWriteEnable();
			RImplementation.r_dsgraph_render_distort();
			if (g_pGamePersistent)
			{
				g_pGamePersistent->OnRenderPPUI_PP();	// PP-UI
			}
		}
	}
}
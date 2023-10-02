#include "stdafx.h"

extern int			ps_r4_native_forward;

void CRenderTarget::phase_forward()
{
	PIX_EVENT(Forward_rendering);
	if (ps_r4_native_forward) {
		u_setrt(RCache.get_target_width(), RCache.get_target_height(), rt_Output->pRT, 0, 0, rt_HWScaledTargetDepth->pZRT);
	} else {
		u_setrt(RCache.get_width(), RCache.get_height(), rt_Target->pRT, rt_Motion->pRT, 0, rt_HWDepth->pZRT);
	}

	RCache.set_CullMode(CULL_CCW);
	RCache.set_Stencil(FALSE);
	RCache.set_ColorWriteEnable();
	RImplementation.render_forward();
	if (g_pGamePersistent)	g_pGamePersistent->OnRenderPPUI_main();	// PP-UI
}
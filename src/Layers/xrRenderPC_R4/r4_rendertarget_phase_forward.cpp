#include "stdafx.h"

void CRenderTarget::phase_forward()
{
	PIX_EVENT(Forward_rendering);
	u_setrt(rt_Target, 0, 0, rt_HWDepth->pZRT);
	RCache.set_CullMode(CULL_CCW);
	RCache.set_Stencil(FALSE);
	RCache.set_ColorWriteEnable();
	RImplementation.render_forward();
	if (g_pGamePersistent)	g_pGamePersistent->OnRenderPPUI_main();	// PP-UI
}
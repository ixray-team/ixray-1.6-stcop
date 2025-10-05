#include "stdafx.h"

void	CRenderTarget::phase_smap_direct(light* L, u32 sub_phase)
{
	// Targets
	u_setrt(rt_smap_surf, nullptr, nullptr, rt_smap_depth->pZRT);

	// Clear
	if (SE_SUN_NEAR == sub_phase)
	{
		// optimized clear
		D3DRECT R;
		R.x1 = L->X.D.minX;
		R.x2 = L->X.D.maxX;
		R.y1 = L->X.D.minY;
		R.y2 = L->X.D.maxY;
		CHK_DX(RDevice->Clear(1L, &R, D3DCLEAR_ZBUFFER, 0xFFFFFFFF, 1.0f, 0L));
	}
	else
	{
		// full-clear
		CHK_DX(RDevice->Clear(0L, nullptr, D3DCLEAR_ZBUFFER, 0xFFFFFFFF, 1.0f, 0L));
	}

	// Stencil	- disable
	RCache.set_Stencil(FALSE);

	//	Cull always CCW. If you want to revert to previouse solution, please, revert bias setup/
	RCache.set_CullMode(CULL_CCW);	// near
	RCache.set_ColorWriteEnable(FALSE);
}

void CRenderTarget::phase_smap_direct_tsh(light* L, u32 sub_phase)
{
	VERIFY(RImplementation.o.Tshadows);
	RCache.set_ColorWriteEnable();
	GRHI->ClearRawTarget(RCache.get_RT(), ERTColor::Black);
}
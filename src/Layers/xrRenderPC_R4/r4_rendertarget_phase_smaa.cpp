#include "stdafx.h"

void CRenderTarget::phase_smaa()
{
	DrawSQ(s_smaa, rt_smaa_edgetex, 0, []
	{
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		RCache.set_Stencil(true, D3DCMP_ALWAYS, 0x1, 0, 0, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
		GRHI->ClearTarget(RCache.get_RT());
	});

	DrawSQ(s_smaa, rt_smaa_blendtex, 1, []
	{
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		RCache.set_Stencil(true, D3DCMP_EQUAL, 0x1, 0, 0, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
		GRHI->ClearTarget(RCache.get_RT());
	});

	DrawSQ(s_smaa, rt_Generic_2, 2, []
	{
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		RCache.set_Stencil(false);
	});

	ResolveSurface(rt_Generic_0, rt_Generic_2);
}

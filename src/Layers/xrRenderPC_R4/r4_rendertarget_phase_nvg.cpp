#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_nvg()
{
	DrawSQ(s_nvg, rt_Back_Buffer_AA, 0, []
	{
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		RCache.set_Stencil(false);
	});

	ResolveSurface(rt_Back_Buffer, rt_Back_Buffer_AA);
}

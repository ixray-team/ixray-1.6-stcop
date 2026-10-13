#include "stdafx.h"
#include "r4_rendertarget.h"

void CRenderTarget::PhaseRainPuddles()
{
	if (!RImplementation.o.RainPuddles || !g_pGamePersistent || g_pGamePersistent->Environment().wetness_factor < EPS_L)
	{
		return;
	}

	GPU_EVENT(RainPuddlesReflections);
	u_setrt(rt_Generic_0, nullptr, nullptr, RDepth);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);
	RCache.set_Element(RainPuddlesShader->E[0]);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
}

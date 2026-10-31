#include "stdafx.h"
#include "r4_rendertarget.h"

void CRenderTarget::PhaseRainPuddles(GfxCmdList& Cmd)
{
	if (!RImplementation.o.RainPuddles || !g_pGamePersistent || g_pGamePersistent->Environment().wetness_factor < EPS_L)
	{
		return;
	}

	GPU_EVENT(RainPuddlesReflections);
	u_setrt(Cmd, rt_Generic_0, nullptr, nullptr, RDepth);
	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetStencil(false);
	Cmd.SetPass(RainPuddlesShader->E[0]);
	Cmd.SetGeometry(FSTriangleGeom);
	Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 3, 0, 0);
}

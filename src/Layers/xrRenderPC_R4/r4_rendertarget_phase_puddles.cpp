#include "stdafx.h"
#include "r4_rendertarget.h"

void CRenderTarget::phase_puddles()
{
	if(RImplementation.m_levels_puddles.empty() || g_pGamePersistent->Environment().wetness_factor == 0.0f) {
		return;
	}

	u_setrt(rt_Generic_0, 0, 0, RDepth);

	for(CRender::PuddleBase& puddle : RImplementation.m_levels_puddles) {
		if(!RImplementation.ViewBase.testSphere_dirty(puddle.m_world.c, puddle.m_radius)) {
			continue;
		}

		//Set xform, shader, and constants
		RCache.set_xform_world(puddle.m_world);

		RCache.set_Shader(s_puddles);
		RCache.set_CullMode(CULL_CCW);

		RCache.set_c("puddle_constants", g_pGamePersistent->Environment().wetness_factor * puddle.m_height);

		//Draw the quad
		RCache.Render_noIA(6);
	}
}
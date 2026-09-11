#include "stdafx.h"
#include "r4_rendertarget.h"

bool CRenderTarget::phase_puddles()
{
	auto& wetness_factor = g_pGamePersistent->Environment().wetness_factor;

	if(RImplementation.m_levels_puddles.empty() || wetness_factor == 0.0f)
	{
		return false;
	}

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	RCache.set_Shader(s_puddles);

	auto puddles = 0;

	for(CRender::PuddleBase& puddle : RImplementation.m_levels_puddles) 
	{
		if(!RImplementation.ViewBase.testSphere_dirty(puddle.m_world.c, puddle.m_radius))
		{
			continue;
		}

		RCache.set_xform_world(puddle.m_world);

		RCache.set_c("puddle_constants", g_pGamePersistent->Environment().wetness_factor * puddle.m_height);
		RCache.Render_noIA(6);

		++puddles;
	}

	return puddles > 0;
}

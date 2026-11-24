#include "stdafx.h"
#include "r4_rendertarget.h"

bool CRender::PuddlesEnabled() const
{
	return ps_r2_ls_flags_ext.test(R4FLAG_PUDDLES);
}

bool CRenderTarget::phase_puddles()
{
	const auto& Surfaces = g_pGameLevel->Puddles.GetSurfaces();
	const float Wetness = g_pGamePersistent->Environment().wetness_factor;

	if (Surfaces.empty() || Wetness == 0.f)
	{
		return false;
	}

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	RCache.set_Shader(s_puddles);

	bool Rendered = false;

	for (const auto& Puddle : Surfaces)
	{
		const float Height = Wetness * Puddle.MaxHeight;
		const Fvector Center = Fvector().mad(Puddle.World.c, Puddle.World.j, Height);
		if (!RImplementation.ViewBase.testSphere_dirty(Center, Puddle.Radius))
		{
			continue;
		}

		RCache.set_xform_world(Puddle.World);

		RCache.set_c("puddle_constants", Height);
		RCache.Render_noIA(6);

		Rendered = true;
	}

	return Rendered;
}

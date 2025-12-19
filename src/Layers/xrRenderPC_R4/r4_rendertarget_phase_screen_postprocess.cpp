#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"

bool UseGasmak = false;
bool UseRainDrops = false;

void CRenderTarget::RenderEffect(ScreenPostProcessType postProcessType, bool postProcessMode)
{
	// Set render target
	if (postProcessMode)
	{
		u_setrt(rt_Back_Buffer_AA, nullptr, nullptr, nullptr);
	}

	// Configure rendering settings
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	// Lock and set vertices

	// Set shader and geometry
	RCache.set_Element(s_spp->E[postProcessType]);
	RCache.set_Geometry(FSTriangleGeom);

	// Render
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	// Copy resource
	if (postProcessMode)
	{
		GRHI->CopySurface(rt_Back_Buffer->pSurface, rt_Back_Buffer_AA->pSurface);
	}
}

void CRenderTarget::PhaseAberration()
{
	RenderEffect(ScreenPostProcessType::Aberration);
}

void CRenderTarget::PhaseVignette()
{
	RenderEffect(ScreenPostProcessType::Vignette);
}

void CRenderTarget::PhaseSaturation()
{
	RenderEffect(ScreenPostProcessType::Saturation);
}

void CRenderTarget::PhaseRaindrops()
{
	const bool ItemCfgHudRainDropsAvialable = g_pGamePersistent->ShaderParams.ItemCfgHudRainDropsAvialable;
	if (!ItemCfgHudRainDropsAvialable)
	{
		return;
	}

	const float condition = g_pGamePersistent->ShaderParams.HelmetCondition;
	if (condition < 0)
	{
		return;
	}

	if (g_pGamePersistent->Environment().wetness_factor < EPS_L)
	{
		return;
	}

	RenderEffect(ScreenPostProcessType::Raindrops);
}

void CRenderTarget::PhaseGasmask()
{
	const bool ItemCfgHudGasMaskAvialable = g_pGamePersistent->ShaderParams.ItemCfgHudGasMaskAvialable;
	if (!ItemCfgHudGasMaskAvialable)
	{
		return;
	}

	const float condition = g_pGamePersistent->ShaderParams.HelmetCondition;
	if (condition < 0)
	{
		return;
	}

	size_t currentState = 4 - ((1.f * condition) * 4);
	clamp(currentState, 0ull, 3ull);

	// Set render target
	u_setrt(rt_Back_Buffer_AA, nullptr, nullptr, nullptr);

	// Configure rendering settings
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	// Set shader and geometry
	RCache.set_Element(s_gasmask->E[currentState]);
	RCache.set_Geometry(FSTriangleGeom);

	// Render
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	// Copy resource
	GRHI->CopySurface(rt_Back_Buffer->pSurface, rt_Back_Buffer_AA->pSurface);
}

void CRenderTarget::PhaseWinter()
{
	RCache.set_xform_world(Fidentity);
	RCache.set_xform_world_old(Fidentity);
	RenderEffect(ScreenPostProcessType::Winter, false);
}

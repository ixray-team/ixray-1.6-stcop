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

void CRenderTarget::PhaseRaindrops()
{
	const auto& Params = g_pGamePersistent->ShaderParams;
	const bool HasGlass = Params.ItemCfgHudRainDropsAvailable && Params.HelmetCondition >= 0.f;
	g_pGamePersistent->VisorRain.Update(UseRainDrops && HasGlass);
	if (g_pGamePersistent->VisorRain.GetAmount() < EPS_L)
	{
		return;
	}

	if (UseGasmak && Params.ItemCfgHudGasMaskAvailable && Params.GlassType == VisorGlassType::Flat)
	{
		// The flat glass pass already includes the selected rain effect.
		return;
	}

	RenderEffect(g_RainDropsMode == 1 ? ScreenPostProcessType::ProceduralRaindrops : ScreenPostProcessType::Raindrops);
}

void CRenderTarget::PhaseGasmask()
{
	const bool ItemCfgHudGasMaskAvailable = g_pGamePersistent->ShaderParams.ItemCfgHudGasMaskAvailable;
	if (!ItemCfgHudGasMaskAvailable)
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
	const auto& Params = g_pGamePersistent->ShaderParams;
	if (Params.GlassType == VisorGlassType::Flat)
	{
		g_pGamePersistent->VisorRain.Update(UseRainDrops && Params.ItemCfgHudRainDropsAvailable);
		const float Condition = clampr(condition, 0.f, 1.f);
		const u32 DamagePhase = std::min(u32((1.f - Condition) * 10.f) + 1, 10u);
		const float RainMode = UseRainDrops && Params.ItemCfgHudRainDropsAvailable ? float(g_RainDropsMode + 1) : 0.f;
		RCache.set_Element(FlatGasMaskShader->E[0]);
		RCache.set_c("mask_control", float(DamagePhase), RainMode, 1.f, 0.f);
	}
	else
	{
		RCache.set_Element(s_gasmask->E[currentState]);
	}
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

#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"

bool UseGasmak = false;
bool UseRainDrops = false;

static void BindScreenPass()
{
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);
}

void CRenderTarget::RenderEffect(ScreenPostProcessType postProcessType, bool postProcessMode)
{
	if (postProcessMode)
	{
		DrawSQ(s_spp, rt_Back_Buffer_AA, postProcessType, BindScreenPass);
		ResolveSurface(rt_Back_Buffer, rt_Back_Buffer_AA);
		return;
	}

	DrawPassSQ(s_spp, postProcessType, BindScreenPass);
}

void CRenderTarget::PhaseAberration()
{
	RenderEffect(ScreenPostProcessType::Aberration);
}

void CRenderTarget::PhaseRaindrops()
{
	const bool ItemCfgHudRainDropsAvailable = g_pGamePersistent->ShaderParams.ItemCfgHudRainDropsAvailable;
	if (!ItemCfgHudRainDropsAvailable)
	{
		return;
	}

	const float Condition = g_pGamePersistent->ShaderParams.HelmetCondition;
	if (Condition < 0)
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
	const bool ItemCfgHudGasMaskAvailable = g_pGamePersistent->ShaderParams.ItemCfgHudGasMaskAvailable;
	if (!ItemCfgHudGasMaskAvailable)
	{
		return;
	}

	const float Condition = g_pGamePersistent->ShaderParams.HelmetCondition;
	if (Condition < 0)
	{
		return;
	}

	size_t CurrentState = 4 - ((1.f * Condition) * 4);
	clamp(CurrentState, 0ull, 3ull);

	DrawSQ(s_gasmask, rt_Back_Buffer_AA, (u32)CurrentState, BindScreenPass);
	ResolveSurface(rt_Back_Buffer, rt_Back_Buffer_AA);
}

void CRenderTarget::PhaseWinter()
{
	RCache.set_xform_world(Fidentity);
	RCache.set_xform_world_old(Fidentity);

	RenderEffect(ScreenPostProcessType::Winter, false);
}

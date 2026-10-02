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

	const float Condition = g_pGamePersistent->ShaderParams.HelmetCondition;
	if (Condition < 0)
	{
		return;
	}

	const auto& Params = g_pGamePersistent->ShaderParams;
	if (Params.GlassType == VisorGlassType::Flat)
	{
		g_pGamePersistent->VisorRain.Update(UseRainDrops && Params.ItemCfgHudRainDropsAvailable);
		const float ClampedCondition = clampr(Condition, 0.f, 1.f);
		const u32 DamagePhase = std::min(u32((1.f - ClampedCondition) * 10.f) + 1, 10u);
		const float RainMode = UseRainDrops && Params.ItemCfgHudRainDropsAvailable ? float(g_RainDropsMode + 1) : 0.f;
		DrawSQ(FlatGasMaskShader, rt_Back_Buffer_AA, 0, [&]
		{
			BindScreenPass();
			RCache.set_c("mask_control", float(DamagePhase), RainMode, 1.f, 0.f);
		});
	}
	else
	{
		size_t CurrentState = 4 - ((1.f * Condition) * 4);
		clamp(CurrentState, 0ull, 3ull);

		DrawSQ(s_gasmask, rt_Back_Buffer_AA, (u32)CurrentState, BindScreenPass);
	}

	ResolveSurface(rt_Back_Buffer, rt_Back_Buffer_AA);
}

void CRenderTarget::PhaseWinter()
{
	RCache.set_xform_world(Fidentity);
	RCache.set_xform_world_old(Fidentity);

	RenderEffect(ScreenPostProcessType::Winter, false);
}

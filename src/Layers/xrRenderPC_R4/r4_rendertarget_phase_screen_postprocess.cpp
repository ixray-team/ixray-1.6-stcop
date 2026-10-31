#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"

bool UseGasmak = false;
bool UseRainDrops = false;

static void BindScreenPass()
{
	RCache.set_CullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);
}

static void DrawBackBuffer(CRenderTarget& Target, const ref_shader& Shader, u32 Element)
{
	Target.DrawSQ(Shader, Target.rt_Back_Buffer_AA, Element, BindScreenPass);
	Target.ResolveSurface(Target.rt_Back_Buffer, Target.rt_Back_Buffer_AA);
}

void CRenderTarget::PhaseEffectSQ(EffectSQ Effect)
{
	switch (Effect)
	{
	case EffectSQ::FXAA:
	{
		DrawSQ(s_fxaa, rt_Generic_2);
		ResolveSurface(rt_Generic_0, rt_Generic_2);
		break;
	}
	case EffectSQ::CAS:
	{
		const u32 Element = ps_r4_sharpening_mode == 0 ? 1 : 0;
		DrawSQ(s_cas, rt_Back_Buffer_AA, Element, []
		{
			RCache.set_CullMode(ERHI_CULLMODE::NONE);
			RCache.set_Stencil(false);
			RCache.Constants.Write<CBPost>().sharpening_intensity = ps_r4_cas_sharpening;
		});
		ResolveSurface(rt_Back_Buffer, rt_Back_Buffer_AA);
		break;
	}
	case EffectSQ::NVG:
	{
		DrawBackBuffer(*this, s_nvg, 0);
		break;
	}
	case EffectSQ::Aberration:
	{
		DrawBackBuffer(*this, s_spp, ScreenPostProcessType::Aberration);
		break;
	}
	case EffectSQ::Raindrops:
	{
		const auto& Params = g_pGamePersistent->ShaderParams;
		const bool HasGlass = Params.ItemCfgHudRainDropsAvailable && Params.HelmetCondition >= 0.f;
		g_pGamePersistent->VisorRain.Update(UseRainDrops && HasGlass);
		if (g_pGamePersistent->VisorRain.GetAmount() < EPS_L)
		{
			break;
		}

		if (UseGasmak && Params.ItemCfgHudGasMaskAvailable && Params.GlassType == VisorGlassType::Flat)
		{
			// The flat glass pass already includes the selected rain effect.
			break;
		}

		DrawBackBuffer(*this, s_spp, g_RainDropsMode == 1 ? ScreenPostProcessType::ProceduralRaindrops : ScreenPostProcessType::Raindrops);
		break;
	}
	case EffectSQ::Gasmask:
	{
		const bool ItemCfgHudGasMaskAvailable = g_pGamePersistent->ShaderParams.ItemCfgHudGasMaskAvailable;
		if (!ItemCfgHudGasMaskAvailable)
		{
			break;
		}

		const float Condition = g_pGamePersistent->ShaderParams.HelmetCondition;
		if (Condition < 0)
		{
			break;
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
				RCache.Constants.Write<CBVisor>().mask_control.set(float(DamagePhase), RainMode, 1.f, 0.f);
			});
			ResolveSurface(rt_Back_Buffer, rt_Back_Buffer_AA);
		}
		else
		{
			size_t CurrentState = 4 - ((1.f * Condition) * 4);
			clamp(CurrentState, 0ull, 3ull);

			DrawBackBuffer(*this, s_gasmask, (u32)CurrentState);
		}
		break;
	}
	case EffectSQ::Winter:
	{
		RCache.set_xform_world(Fidentity);
		RCache.set_xform_world_old(Fidentity);
		DrawPassSQ(s_spp, ScreenPostProcessType::Winter, BindScreenPass);
		break;
	}
	}
}

void CRenderTarget::phase_ui_postprocess(Fcolor* color, bool HasBackground)
{
	DrawPassSQ(s_cas, 3, [&]
	{
		RCache.Constants.Write<CBPost>().ui_has_background.set(HasBackground ? 1.0f : 0.0f, 0.0f, 0.0f, 0.0f);

		if (color)
			RCache.Constants.Write<CBPost>().static_color.set(color->r, color->g, color->b, color->a);
		else
			RCache.Constants.Write<CBPost>().static_color.set(1, 1, 1, 1);
	});
}

void CRenderTarget::phase_ui_postprocess_copy()
{
	DrawPassSQ(s_cas, 2);
}

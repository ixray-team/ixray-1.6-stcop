#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"

bool UseGasmak = false;
bool UseRainDrops = false;

static void BindScreenPass(GfxCmdList& Cmd)
{
	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetStencil(false);
}

static void DrawBackBuffer(GfxCmdList& Cmd, CRenderTarget& Target, const ref_shader& Shader, u32 Element)
{
	Target.DrawSQ(Cmd, Shader, Target.rt_Back_Buffer_AA, Element, [&] { BindScreenPass(Cmd); });
	Target.ResolveSurface(Target.rt_Back_Buffer, Target.rt_Back_Buffer_AA);
}

void CRenderTarget::PhaseEffectSQ(GfxCmdList& Cmd, EffectSQ Effect)
{
	switch (Effect)
	{
	case EffectSQ::FXAA:
	{
		DrawSQ(Cmd, s_fxaa, rt_Generic_2);
		ResolveSurface(rt_Generic_0, rt_Generic_2);
		break;
	}
	case EffectSQ::CAS:
	{
		const u32 Element = ps_r4_sharpening_mode == 0 ? 1 : 0;
		DrawSQ(Cmd, s_cas, rt_Back_Buffer_AA, Element, [&]
		{
			Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			Cmd.SetStencil(false);
			Cmd.Constants.Write<CBPost>().sharpening_intensity = ps_r4_cas_sharpening;
		});
		ResolveSurface(rt_Back_Buffer, rt_Back_Buffer_AA);
		break;
	}
	case EffectSQ::NVG:
	{
		DrawBackBuffer(Cmd, *this, s_nvg, 0);
		break;
	}
	case EffectSQ::Aberration:
	{
		DrawBackBuffer(Cmd, *this, s_spp, ScreenPostProcessType::Aberration);
		break;
	}
	case EffectSQ::Raindrops:
	{
		const auto& Params = g_pGamePersistent->ShaderParams;
		const bool HasGlass = Params.ItemCfgHudRainDropsAvailable && Params.HelmetCondition >= 0.f;
		g_pGamePersistent->VisorRain.Update(UseRainDrops && HasGlass);
		Cmd.Constants.Write<CBVisor>().visor_rain.set(g_pGamePersistent->VisorRain.GetAmount(), 0.f, 0.f, 0.f);
		if (g_pGamePersistent->VisorRain.GetAmount() < EPS_L)
		{
			break;
		}

		if (UseGasmak && Params.ItemCfgHudGasMaskAvailable && Params.GlassType == VisorGlassType::Flat)
		{
			// The flat glass pass already includes the selected rain effect.
			break;
		}

		DrawBackBuffer(Cmd, *this, s_spp, g_RainDropsMode == 1 ? ScreenPostProcessType::ProceduralRaindrops : ScreenPostProcessType::Raindrops);
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
			Cmd.Constants.Write<CBVisor>().visor_rain.set(g_pGamePersistent->VisorRain.GetAmount(), 0.f, 0.f, 0.f);
			const float ClampedCondition = clampr(Condition, 0.f, 1.f);
			const u32 DamagePhase = std::min(u32((1.f - ClampedCondition) * 10.f) + 1, 10u);
			const float RainMode = UseRainDrops && Params.ItemCfgHudRainDropsAvailable ? float(g_RainDropsMode + 1) : 0.f;
			DrawSQ(Cmd, FlatGasMaskShader, rt_Back_Buffer_AA, 0, [&]
			{
				BindScreenPass(Cmd);
				Cmd.Constants.Write<CBVisor>().mask_control.set(float(DamagePhase), RainMode, 1.f, 0.f);
			});
			ResolveSurface(rt_Back_Buffer, rt_Back_Buffer_AA);
		}
		else
		{
			size_t CurrentState = 4 - ((1.f * Condition) * 4);
			clamp(CurrentState, 0ull, 3ull);

			DrawBackBuffer(Cmd, *this, s_gasmask, (u32)CurrentState);
		}
		break;
	}
	case EffectSQ::Winter:
	{
		Cmd.SetObjectTransform(Fidentity, Fidentity);
		DrawPassSQ(Cmd, s_spp, ScreenPostProcessType::Winter, [&] { BindScreenPass(Cmd); });
		break;
	}
	}
}

void CRenderTarget::phase_ui_postprocess(GfxCmdList& Cmd, Fcolor* color, bool HasBackground)
{
	DrawPassSQ(Cmd, s_cas, 3, [&]
	{
		Cmd.Constants.Write<CBPost>().ui_has_background.set(HasBackground ? 1.0f : 0.0f, 0.0f, 0.0f, 0.0f);

		if (color)
			Cmd.Constants.Write<CBPost>().static_color.set(color->r, color->g, color->b, color->a);
		else
			Cmd.Constants.Write<CBPost>().static_color.set(1, 1, 1, 1);
	});
}

void CRenderTarget::phase_ui_postprocess_copy(GfxCmdList& Cmd)
{
	DrawPassSQ(Cmd, s_cas, 2);
}

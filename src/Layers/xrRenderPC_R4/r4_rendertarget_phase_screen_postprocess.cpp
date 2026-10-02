#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"

bool UseGasmak = false;
bool UseRainDrops = false;

static void BindScreenPass()
{
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
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
			const u32 Element = ps_r4_sharpening_mode == 0 ? 1u : 0u;
			DrawSQ(s_cas, rt_Back_Buffer_AA, Element, []
			{
				GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
				RCache.set_Stencil(false);
				RCache.set_c("sharpening_intensity", ps_r4_cas_sharpening);
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
			if (!g_pGamePersistent->ShaderParams.ItemCfgHudRainDropsAvailable)
			{
				break;
			}

			const float Condition = g_pGamePersistent->ShaderParams.HelmetCondition;
			if (Condition < 0)
			{
				break;
			}

			if (g_pGamePersistent->Environment().wetness_factor < EPS_L)
			{
				break;
			}

			DrawBackBuffer(*this, s_spp, ScreenPostProcessType::Raindrops);
			break;
		}
		case EffectSQ::Gasmask:
		{
			if (!g_pGamePersistent->ShaderParams.ItemCfgHudGasMaskAvailable)
				break;

			const float Condition = g_pGamePersistent->ShaderParams.HelmetCondition;
			if (Condition < 0)
				break;

			size_t CurrentState = 4 - ((1.f * Condition) * 4);
			clamp(CurrentState, 0ull, 3ull);

			DrawBackBuffer(*this, s_gasmask, (u32)CurrentState);
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

void CRenderTarget::phase_ui_postprocess(Fcolor* color)
{
	DrawPassSQ(s_cas, 3, [&]
	{
		if (color)
		{
			RCache.set_c("static_color", color->r, color->g, color->b, color->a);
		}
		else
		{
			RCache.set_c("static_color", 1, 1, 1, 1);
		} 
	});
}

void CRenderTarget::phase_ui_postprocess_copy()
{
	DrawPassSQ(s_cas, 2);
}

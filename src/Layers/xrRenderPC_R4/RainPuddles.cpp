#include "stdafx.h"
#include "r4_rendertarget.h"
#include "BlenderRainPuddles.h"
#include "DX10 Rain/dx10RainBlender.h"

void CRender::UpdateRainPuddleMode()
{
	string_path Path{};
	// The shared texture is a placeholder, not an authored mask for the current level.
	o.HasRainPuddleMask = FS.exist(Path, "$level$", "rain_mask", ".dds") != nullptr;
	o.ProceduralRainPuddles = o.RainPuddles && (o.dx11_use_legacy_light || !o.HasRainPuddleMask);
	if (o.RainPuddles && !o.dx11_use_legacy_light)
	{
		if (o.HasRainPuddleMask)
		{
			Msg("* Rain puddles: PBR level mask '%s'", Path);
		}
		else
		{
			Msg("* Rain puddles: PBR procedural (no level rain_mask.dds)");
		}
	}
}

void CRenderTarget::CreateRainShaders()
{
	// The level alias and the selected puddle algorithm may change on a level transition.
	s_rain.destroy();
	RainPuddlesShader.destroy();
	if (RImplementation.o.HasRainPuddleMask)
	{
		ref_texture Mask;
		Mask.create("rain_mask");
		Mask->Unload();
	}
	CBlender_rain RainBlender;
	s_rain.create(&RainBlender, "null", RImplementation.o.HasRainPuddleMask ? "rain_mask" : nullptr);
	if (RImplementation.o.dx11_use_legacy_light && RImplementation.o.ProceduralRainPuddles)
	{
		BlenderRainPuddles PuddleBlender;
		RainPuddlesShader.create(&PuddleBlender, "null");
	}
}

void CRenderTarget::PhaseRainPuddles(GfxCmdList& Cmd)
{
	if (!RImplementation.o.dx11_use_legacy_light || !RImplementation.o.ProceduralRainPuddles || !ps_r2_ls_flags_ext.test(R4FLAG_RAIN_PUDDLES) || !g_pGamePersistent || g_pGamePersistent->Environment().wetness_factor < EPS_L)
	{
		return;
	}

	GPU_EVENT(RainPuddlesReflections);
	u_setrt(Cmd, rt_Generic_0, nullptr, nullptr, RDepth);
	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetStencil(false);
	Cmd.SetPass(RainPuddlesShader->E[0]);
	Cmd.DrawNoInput(3);
}

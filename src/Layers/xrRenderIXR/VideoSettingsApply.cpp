/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "ixr.h"
#include "xrRender_console.h"
#include "DetailManager.h"
#include "UpscaleNri.h"
#include "StateManager/dx10SamplerStateCache.h"

namespace
{
	using enum EVideoSetting;

	u32 Get(EVideoSetting Setting)
	{
		return VideoSettings::Effective(Setting);
	}

	bool On(EVideoSetting Setting)
	{
		return Get(Setting) != 0;
	}

	bool IsDrawn(EVideoSetting Setting, u32 Value)
	{
		if (Setting == VariableShading)
		{
			return Value == 0 || GpuScene::IsVariableRate();
		}

		return Setting != Upscaler || Value < 2 || UpscaleNri::IsSupported(Value);
	}
}

void CRender::RegisterVideoSettingsSupport(bool IsRegistered)
{
	VideoSettings::SetSupportQuery(IsRegistered ? &IsDrawn : nullptr);
}

// The variables of the frame from the effective settings. The flags a restart takes are read from them by
// reset_begin, the shader options by SyncShadingQuality
void CRender::ApplyVideoSettings()
{
	if (VideoSettingsApplied == VideoSettings::Version())
	{
		return;
	}

	VideoSettingsApplied = VideoSettings::Version();

	static constexpr int Anisotropy[] = { 1, 2, 4, 8 };
	ps_r_shading_quality = Get(ShadingQuality);
	ps_r__tf_Anisotropic = Anisotropy[ps_r_shading_quality];

	// The sun's cascades: none, the widest alone, all of them. Whose sun it is the shading quality says (ForwardDynamicLight)
	static constexpr u32 Lighting[] = { 0, 1, 1, 2, 2 };
	static constexpr float LampShadowScale[] = { 0.5f, 0.5f, 0.75f, 1.0f, 1.0f };
	const u32 Shadow = Get(ShadowQuality);
	ps_r_lighting_quality = Lighting[Shadow];
	// The sun's filter: soft where asked for, else a square of five taps a side from ultra and of three below it
	ps_r_shadow_quality = On(Pcss) ? 2 : Shadow >= 4 ? 1 : 0;
	ps_r2_ls_squality = LampShadowScale[Shadow];
	ps_r_lamp_shadows = Shadow >= 2;
	ps_r2_ls_flags_ext.set(RFLAG_CLOUD_SHADOWS, Shadow >= 4);
	ps_r2_ls_flags_ext.set(R4FLAG_SCREEN_SPACE_HUD_SHADOWS, Shadow >= 2);
	ps_r2_ls_flags.set(R2FLAG_SUN_DETAILS | R2FLAG_LIGHTS_DETAILS, Shadow >= 3);

	// Low is the original shafts with one step, medium and up the alternative with two and three, as the frame of the
	// old light model drew them by default
	static constexpr u32 SunShafts[] = { 0, 1, 2, 2, 3 };
	const u32 Volumetric = Get(VolumetricQuality);
	ps_r_sun_shafts = SunShafts[Volumetric];
	g_SunShaftsAlternative = Volumetric >= 2;
	ps_r2_ls_flags.set(R2FLAG_VOLUMETRIC_LIGHTS, Volumetric >= 1);

	ps_r_ssao_mode = On(OcclusionQuality) ? 2 : 0;
	ps_r2_ls_flags_ext.set(R4FLAG_GTAO_HQ | R4FLAG_SSLR_HQ, On(ScreenSpaceQuality));
	ps_r2_ls_flags_ext.set(R4FLAG_SSLR_ON_WORLD, Get(ReflectionQuality) >= 1);
	ps_r2_ls_flags_ext.set(R4FLAG_OFFSCREEN_REFLECTIONS, Get(ReflectionQuality) >= 2);
	g_Bloom = On(PostProcessingQuality);

	// The editors keep the density their scene was made with
	static constexpr float GrassDensity[] = { 0.6f, 0.4f, 0.25f, 0.15f };
	ps_r_grass_quality = Get(GrassQuality);

	if (!Device.IsEditorMode() && ps_current_detail_density != GrassDensity[ps_r_grass_quality])
	{
		ps_current_detail_density = GrassDensity[ps_r_grass_quality];

		if (Details)
		{
			Details->RequestCacheRebuild();
		}
	}

	static constexpr float LodError[] = { 4.0f, 2.0f, 1.0f, 0.5f };
	ps_r_lod_error = LodError[Get(DistanceQuality)];
	ps_r_culling_quality = Get(CullingQuality);
	ps_r_motion_vectors_quality = Get(MotionVectorsQuality);

	static constexpr u32 MotionBlurPasses[] = { 0, 2, 4 };
	const u32 AntiAliasingMode = Get(AntiAliasing);
	ps_r2_aa_type = AntiAliasingMode < 4 ? AntiAliasingMode : 0;
	ps_r_msaa = AntiAliasingMode == 4 ? Get(Msaa) + 1 : 0;
	ps_r4_mblur_quality = MotionBlurPasses[Get(MotionBlur)];
	ps_r2_ls_flags.set(R2FLAG_DOF, On(Dof));

	ps_r2_ls_flags.set(R3FLAG_DYN_WET_SURF, On(WetSurfaces));
	ps_r2_ls_flags_ext.set(R4FLAG_RAIN_PUDDLES | R4FLAG_PUDDLES, On(WetSurfaces));
	ps_r2_ls_flags.set(R2FLAG_SOFT_PARTICLES, On(SoftParticles));
	ps_r2_ls_flags.set(R2FLAG_SOFT_WATER, On(SoftWater));
	ps_r2_ls_flags_ext.set(R4FLAG_SSLR_ON_WATER, On(WaterReflections));
	ps_r2_ls_flags.set(R2FLAG_STEEP_PARALLAX, On(Parallax));
	ps_r2_ls_flags.set(R2FLAG_DETAIL_BUMP, On(DetailBump));
	ps_r_variable_shading = On(VariableShading);

	UseRainDrops = UseWeaponRainDrops = On(EVideoSetting::Raindrops);
	UseGasmak = On(Gasmask);
	ps_r2_ls_flags_ext.set(R2FLAG_SPP_VIGNETTE, On(EVideoSetting::Vignette));
	ps_r2_ls_flags_ext.set(R2FLAG_SPP_ABERRATION, On(EVideoSetting::Aberration));

	// Before the targets are made (create, reset) xrRender_apply_tf sets it
	if (Target)
	{
		SSManager.SetMaxAnisotropy(ps_r__tf_Anisotropic);
	}
}

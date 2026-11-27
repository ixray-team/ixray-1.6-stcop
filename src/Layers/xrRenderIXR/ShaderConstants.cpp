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
#include "SunCloudMask.h"
#include "ShaderConstants.h"

#include "xrRender_console.h"
#include "../../../shaders/common/debug_view.hlsli"
#include "dxEnvironmentRender.h"
#include "AmbientCubeLevel.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/Environment.h"
#include "../../xrEngine/EngineAPI.h"
#include "../../xrEngine/date_time.h"
#include "../../xrEngine/Rain.h"

extern ENGINE_API Fcolor nvg_color;

namespace
{
	template<class Tuple, size_t... Index>
	constexpr bool SlotsMatchOrder(std::index_sequence<Index...>)
	{
		return ((std::tuple_element_t<Index, Tuple>::Slot == Index) && ...);
	}

	Fmatrix Inverse43(const Fmatrix& M)
	{
		Fmatrix Result = Fidentity;
		if (std::abs(M._11) + std::abs(M._22) + std::abs(M._33) > EPS_S)
		{
			Result.invert(M);
		}
		return Result;
	}

	Fmatrix Inverse44(const Fmatrix& M)
	{
		Fmatrix Result = Fidentity;
		if (std::abs(M._11) + std::abs(M._22) + std::abs(M._33) > EPS_S)
		{
			Result.invert44(M);
		}
		return Result;
	}
}

void CShaderConstants::Create()
{
	static_assert(SlotsMatchOrder<Blocks>(std::make_index_sequence<Count>()));

	CBObject& Object = Write<CBObject>();
	Object.m_plmap_xform = Fidentity;
	Object.m_plmap_clamp[0].set(0, 0, 0, 1);
	ResetMaterial();
	PushedEpoch = ~0u;
}

template<class T>
void CShaderConstants::Collect(const T& Block, u32 Force, RHIConstantWrite* Writes, u32& Num)
{
	constexpr u32 Bit = 1u << T::Slot;
	T& Last = std::get<T>(Uploaded);
	if (!(Force & Bit) && (!(Dirty & Bit) || !std::memcmp(&Last, &Block, sizeof(T))))
	{
		return;
	}

	std::memcpy(&Last, &Block, sizeof(T));
	Writes[Num++] = { T::Slot, sizeof(T), &Last };
}

void CShaderConstants::Flush()
{
	const u32 Epoch = GRHI->DevicePtr->ConstantEpoch;
	const u32 Force = Epoch != PushedEpoch ? (1u << Count) - 1 : 0;
	PushedEpoch = Epoch;
	if (!(Dirty | Force))
	{
		return;
	}

	RHIConstantWrite Writes[Count];
	u32 Num = 0;
	std::apply([&](auto&... Block) { (Collect(Block, Force, Writes, Num), ...); }, Shadow);
	Dirty = 0;
	if (Num)
	{
		GRHI->SetConstants(Writes, Num);
	}
}

void CShaderConstants::UpdateFrame()
{
	CBFrame& Frame = Write<CBFrame>();
	const float Time = RFrame.fTimeGlobal;
	Frame.timers.set(Time, Time - RFrame.fTimeDelta, Time * 0.1f, std::sin(Time));
	if (RFrame.HasEnv())
	{
		const CEnvDescriptorMixer& Env = RFrame.Env();
		const Fmatrix& M = RFrame.mFullTransform;
		Fvector4 Plane;
		Plane.set(-(M._14 + M._13), -(M._24 + M._23), -(M._34 + M._33), -(M._44 + M._43));
		Plane.mul(-1.0f / _sqrt(_sqr(Plane.x) + _sqr(Plane.y) + _sqr(Plane.z)));
		const float Near = Env.fog_near;
		const float Far = Env.fog_far;
		const float Range = 1.0f / (Far - Near);
		Frame.fog_plane.set(-Plane.x * Range, -Plane.y * Range, -Plane.z * Range, 1 - (Plane.w - Near) * Range);
		Frame.fog_params.set(-Near * Range, Near, Far, Range);
		Fvector Sun = RImplementation.Lights.SunDirection();

		// The forward level frame lights with the direction its cascades are drawn with: the weather's sun is tilted
		// down for them, as the frame that shades a pixel once has it
		if (!RImplementation.Lights.IsSunBaked() && RImplementation.Lights.sun_adapted)
		{
			Sun = ((light*)RImplementation.Lights.sun_adapted._get())->direction;
		}

		Frame.L_sun_dir_w.set(Sun.x, Sun.y, Sun.z, ps_r_lambert_factor);
		Fvector SunDir;
		RFrame.mView.transform_dir(SunDir, Sun);
		SunDir.normalize();
		Frame.L_sun_dir_e.set(SunDir.x, SunDir.y, SunDir.z, 0);

		// Low shading quality keeps the classic look: the colours as the weather has them. Above it the shaders
		// double a light and decode it, so each colour is half of what the dynamic renderer decodes: its sun, its
		// ambient (doubled there) and its hemi, which is four times the weather's through an ambient cube. That
		// cube is not in these shaders: how bright the two weathers' cubes are is measured when they load, 0.54
		// by day and half of that at night, when a fixed 0.54 left the night three times too bright
		const CEnvironment& Weather = g_pGamePersistent->Environment();
		float Cube = AmbientCubeUnknown;

		if (Weather.Current[0] && Weather.Current[1])
		{
			const float From = static_cast<const dxEnvDescriptorRender*>(&*Weather.Current[0]->m_pDescriptor)->AmbientLevel;
			const float To = static_cast<const dxEnvDescriptorRender*>(&*Weather.Current[1]->m_pDescriptor)->AmbientLevel;
			Cube = From + (To - From) * Env.weight;
		}

		const bool IsClassic = ps_r_shading_quality == 0;
		const float FogScale = IsClassic ? ps_r1_fog_luminance : 1.f;
		Frame.fog_color.set(Env.fog_color.x * FogScale, Env.fog_color.y * FogScale, Env.fog_color.z * FogScale, 0);
		const float BakedSunScale = IsClassic ? 1.f : ps_r2_sun_lumscale * 0.5f;
		const float BakedHemiScale = (IsClassic ? 1.f : ps_r2_sun_lumscale_hemi * 2.f * Cube) * Env.m_fAmbientScale;
		const float BakedAmbientScale = (IsClassic ? 1.f : ps_r2_sun_lumscale_amb) * Env.m_fAmbientScale;
		const float BakedSkyScale = IsClassic ? 1.f : 1.2f;
		Frame.L_sun_color.set(Env.sun_color.x * BakedSunScale, Env.sun_color.y * BakedSunScale, Env.sun_color.z * BakedSunScale, 0);
		Frame.L_ambient.set(Env.ambient.x * BakedAmbientScale, Env.ambient.y * BakedAmbientScale, Env.ambient.z * BakedAmbientScale, Env.weight);
		// w is how far the sky has blended from the last weather's to the next one's, as every shader that mirrors
		// the sky reads it
		Frame.L_hemi_color.set(Env.hemi_color.x * BakedHemiScale, Env.hemi_color.y * BakedHemiScale, Env.hemi_color.z * BakedHemiScale, Env.weight);
		Frame.L_sky_color.set(Env.sky_color.x * BakedSkyScale, Env.sky_color.y * BakedSkyScale, Env.sky_color.z * BakedSkyScale, Env.sky_rotation);
		Frame.water_intensity.set(Env.m_fWaterIntensity, Env.m_fWaterIntensity, Env.m_fWaterIntensity, 0);
		Frame.sun_shafts_intensity.set(Env.m_fSunShaftsIntensity, Env.m_fSunShaftsIntensity, Env.m_fSunShaftsIntensity, g_SunShaftsFalloff);
		// z: how far the streaks of rain have run down the walls, faster in heavier rain
		static float RainRun = 0.0f;
		static u32 RainRunFrame = u32(-1);

		if (RainRunFrame != RFrame.dwFrame)
		{
			RainRunFrame = RFrame.dwFrame;
			RainRun += RFrame.fTimeDelta * (0.15f + Env.rain_density * 0.15f);
			RainRun -= std::floor(RainRun);
		}

		Frame.rain_params.set(Env.rain_density, RFrame.Wetness, RainRun, g_pGameLevel ? (float)g_pGameLevel->UseSnowmask : 0.f);
		// z: the brightness of the sky's light the HUD's raindrops are scaled by, as the frame that lights by itself has
		// its hemi; the forward level frame's hemi is scaled for its own light model
		const Fvector RainHemi = Env.old_style ? Env.sky_color : Fvector{ Env.hemi_color.x, Env.hemi_color.y, Env.hemi_color.z };
		const float RainSky = (std::max(RainHemi.x, 0.f) * 0.30f + std::max(RainHemi.y, 0.f) * 0.59f + std::max(RainHemi.z, 0.f) * 0.11f) * ps_r2_sun_lumscale_hemi * 4.f;
		// w: how wet the HUD is, with the drops shown or not
		Frame.hud_rain.set(RFrame.HudRainTime, UseWeaponRainDrops ? RFrame.HudRainAmount : 0.f, RainSky, RFrame.HudRainAmount);
	}

	const auto& Hud = RDEVICE.hudViewportData;
	Frame.nvg_color.set(::nvg_color.r, ::nvg_color.g, ::nvg_color.b, ::nvg_color.a);
	Frame.m_hud_params.set(float(Hud.isRenderProcess), float(Hud.isRenderActive), Hud.ScopeAberration, Hud.renderZoomRotateFactor);
	Frame.m_zoom_deviation.set(0, 0, Hud.renderScopeBrightnessValue, Hud.renderScopeBrightnessJitterValue);
	Frame.m_affects.set(Hud.CurrentElectronicsProblemsCnt / 10.0f, ::Random.randF(0, 1), Hud.TargetElectronicsProblemsCnt / 10.0f, Hud.IsElectronicsProblemsDecreasing ? 1.0f : 0.0f);
	Frame.m_actor_params.set(Hud.ActorHealth, Hud.ActorOutfitCondition, Hud.ActorWeaponCondition, Hud.ActorWeaponLoading);
	Frame.m_digiclock.set(0.f, 0.f, 0.f, 0.f);
	if (g_pGameLevel && g_pGameLevel->bReady && 0 == RFrame.dwPrecacheFrame)
	{
		u32 Year, Month, Day, Hours, Mins, Secs, Millis;
		split_time(g_pGameLevel->GetGameTime(), Year, Month, Day, Hours, Mins, Secs, Millis);
		Frame.m_digiclock.set((Hours / 10) / 10.f, (Hours % 10) / 10.f, (Mins / 10) / 10.f, (Mins % 10) / 10.f);
		const float SecFraction = Secs / 60.f;
		const float MinFraction = (SecFraction + Mins) / 60.f;
		const float HourFraction = (MinFraction + Hours) / 12.f;
		Frame.m_timearrow.set(std::sin(PI_MUL_2 * HourFraction), std::cos(PI_MUL_2 * HourFraction), std::sin(PI_MUL_2 * MinFraction), std::cos(PI_MUL_2 * MinFraction));
		float Heading, Pitch;
		RFrame.vCameraDirection.getHP(Heading, Pitch);
		Frame.m_timearrow2.set(std::sin(PI_MUL_2 * SecFraction), std::cos(PI_MUL_2 * SecFraction), std::sin(Pitch), std::cos(Pitch));
	}
	// The velocity view is drawn over the frame as it is (phase_post): the surfaces shade as without a view
	Frame.debug_view.set(ps_r_debug_view == DebugView_Velocity ? 0.0f : float(ps_r_debug_view), 0, 0, 0);
	Frame.test_exp_to_shaders_2.set(ps_r__test_exp_to_shaders_2, 0, 0, 0);
	if (g_pGamePersistent)
	{
		Frame.env_wind.set(RFrame.WindBlast.x, RFrame.WindBlast.y, RFrame.WindBlast.z, RFrame.WindStrength);
	}

#ifdef _EDITOR
	Frame.def_aref = 100.f / 255.f;
#else
	Frame.def_aref = ps_r2_def_aref_quality / 255.f;
#endif

	CBUIIcon& Icon = Write<CBUIIcon>();
	const UIIconLighting& IconLight = *Device.m_pRender->GetUIIconLighting();
	Icon.ui_icon_light.set(IconLight.Color.x * IconLight.Intensity, IconLight.Color.y * IconLight.Intensity, IconLight.Color.z * IconLight.Intensity, IconLight.Ambient);
	Icon.ui_icon_direction.set(IconLight.Direction.x, IconLight.Direction.y, IconLight.Direction.z, 0.0f);
	Icon.ui_emissive_tint.set(1.0f, 1.0f, 1.0f, 1.0f);

	// x: 1 in the unlit view (no weather): forward shaders that depend on the environment (water) show their base texture instead
	Write<CBEditor>().editor_params.set(Device.IsEditorMode() && !RImplementation.EditorIsLit() ? 1.f : 0.f, 0.f, 0.f, 0.f);

	CBVisor& Visor = Write<CBVisor>();
	Visor.RainPuddleWaterBounds = RImplementation.RainPuddleWater.GetBounds();
	Visor.RainPuddleWaterHeightRange = RImplementation.RainPuddleWater.GetHeightRange();
	Visor.RainPuddleControl.set(RImplementation.o.RainPuddles && ps_r2_ls_flags_ext.test(R4FLAG_RAIN_PUDDLES) ? 1.0f : 0.0f, 0.0f, 0.0f, 0.0f);
}

void CShaderConstants::UpdateView(const GfxView& X)
{
	CBView& View = Write<CBView>();

	Fmatrix VP;
	VP.mul(X.Project, X.View);

	Fmatrix VPOld;
	VPOld.mul(X.ProjectOld, X.ViewOld);
	View.m_V = X.View;
	View.m_P = X.Project;
	View.m_VP = VP;

	const Fmatrix InvView = Inverse43(X.View);
	View.m_invV = InvView;
	View.m_invP = Inverse44(RFrame.mProject_saved);
	View.m_VP_old = VPOld;
	View.m_invVP_old = Inverse44(VPOld);

	Fmatrix Probe;
	View.m_probe = Probe.mul_43(RImplementation.reflection_view, InvView);

	// The HUD view is camera-relative: its eye sits at the origin
	const bool IsHudView = std::memcmp(&X.View, &RFrame.mView_hud, sizeof(Fmatrix)) == 0;
	View.eye_position = IsHudView ? Fvector().set(0.f, 0.f, 0.f) : RFrame.vCameraPosition;
	View.eye_world = RFrame.vCameraPosition;
	View.eye_direction = RFrame.vCameraDirection;
	View.eye_normal = RFrame.vCameraTop;
	View.pos_decompression_params2.set(GfxCmdList::GetWidth(), GfxCmdList::GetHeight(), 1.0f / GfxCmdList::GetWidth(), 1.0f / GfxCmdList::GetHeight());

	CBPass& Pass = Write<CBPass>();
	Pass.m_P_hud = RFrame.mProject_hud;
	Pass.m_invP_hud = Inverse44(RFrame.mProject_hud);
	Pass.m_VP_hud = RFrame.mFullTransform_hud;
	Pass.m_VP_hud_old = RFrame.mFullTransform_hud_old;

	UpdateSunShadows();
	UpdateScreen();
}

void CShaderConstants::UpdateSunShadows()
{
	CBLight& Light = Write<CBLight>();
	Light.SunCacheParams.set(float(RImplementation.GetSunShadowCacheMode()), 0, 0, 0);
	for (u32 Index = 0; Index < 3; ++Index)
	{
		Fmatrix Transform;
		Fvector4 Capture, Warp;
		RImplementation.GetSunShadowConstants(Index, Transform, Capture, Warp);
		Light.m_shadow_sun[Index] = Transform;
		Light.SunCaptureDirectionDepth[Index] = Capture;
		Light.SunReceiverWarpBias[Index] = Warp;
	}

	// The forward level frame shades in the world: its clouds' shadow is found from a point of the world
	if (RImplementation.Lights.sun_adapted)
	{
		Light.m_sunmask = SunCloudMask::Matrix(((light*)RImplementation.Lights.sun_adapted._get())->direction);
	}
}

void CShaderConstants::UpdateObject(const Fmatrix& World, const Fmatrix& WorldView, const Fmatrix& WorldViewProject, const Fmatrix& WorldViewProjectOld)
{
	CBObject& Object = Write<CBObject>();
	Object.m_W = World;
	Object.m_WV = WorldView;
	Object.m_WVP = WorldViewProject;
	Object.m_WVP_old = WorldViewProjectOld;
}

void CShaderConstants::ResetMaterial()
{
	CBMaterial& Material = Write<CBMaterial>();
	Material = {};
	Write<CBDraw>() = {};
	Material.tfactor.set(1.0f, 1.0f, 1.0f, 1.0f);
}

void CShaderConstants::UpdateScreen()
{
	CBView& View = Write<CBView>();
	const float TargetWidth = GfxCmdList::GetTargetWidth();
	const float TargetHeight = GfxCmdList::GetTargetHeight();
	const float Width = GfxCmdList::GetWidth();
	const float Height = GfxCmdList::GetHeight();
	// xy: the view in pixels. zw: a texel of the targets, which may be larger than the view and hold it in their
	// corner: a pixel times zw is its place in such a target, not its share of the view
	const float AllocWidth = float(Device.OutputWidth);
	const float AllocHeight = float(Device.OutputHeight);
	View.screen_res.set(TargetWidth, TargetHeight, 1.0f / AllocWidth, 1.0f / AllocHeight);
	View.scaled_screen_res.set(Width, Height, 1.0f / float(AllocHalfTarget.x), 1.0f / float(AllocHalfTarget.y));
	View.view_uv.set(TargetWidth / AllocWidth, TargetHeight / AllocHeight, (TargetWidth - 0.5f) / AllocWidth, (TargetHeight - 0.5f) / AllocHeight);

	// What the frame before left in the targets lies in the view it had
	static Fvector4 Last = View.view_uv;
	static Fvector4 Before = View.view_uv;
	static u32 Frame = RFrame.dwFrame;

	if (Frame != RFrame.dwFrame)
	{
		Frame = RFrame.dwFrame;
		Before = Last;
	}

	Last = View.view_uv;
	View.view_uv_prev = Before;
	View.m_taa_jitter.set(ps_r_taa_jitter.x, ps_r_taa_jitter.y, ps_r_taa_jitter.z, float(RFrame.dwFrame));
}

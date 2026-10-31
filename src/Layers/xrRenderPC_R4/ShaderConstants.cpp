#include "stdafx.h"
#include "ShaderConstants.h"

#include "xrRender_console.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/Environment.h"
#include "../../xrEngine/EngineAPI.h"
#include "../../xrEngine/date_time.h"
#include "../../xrEngine/Rain.h"

extern float r_dtex_range;
extern ENGINE_API Fcolor nvg_color;

namespace
{
	template<class Tuple, size_t... Index>
	constexpr bool SlotsMatchOrder(std::index_sequence<Index...>)
	{
		return ((std::tuple_element_t<Index, Tuple>::Slot == Index) && ...);
	}

	void Store(IRHIBuffer* Buffer, const void* Data, u32 Size)
	{
		RHIMappedSubresource Mapped{};
		if (Buffer && Buffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &Mapped))
		{
			CopyMemory(Mapped.pData, Data, Size);
			Buffer->Unmap();
		}
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

	std::apply([this](auto&... Block) { (RHIUtils::CreateConstantBuffer(&Buffers[Block.Slot], sizeof(Block)), ...); }, Shadow);

	CBObject& Object = Write<CBObject>();
	Object.m_plmap_xform = Fidentity;
	Object.m_plmap_clamp[0].set(0, 0, 0, 1);
	ResetMaterial();

	std::apply([this](auto&... Block) { (Upload(Block, true), ...); }, Shadow);
	Dirty = 0;
	Bound = false;
}

void CShaderConstants::Destroy()
{
	for (IRHIBuffer*& Buffer : Buffers)
	{
		_RELEASE(Buffer);
	}
}

template<class T>
void CShaderConstants::Upload(const T& Block, bool Force)
{
	T& Last = std::get<T>(Uploaded);
	if (!Force && (!(Dirty & (1u << T::Slot)) || !std::memcmp(&Last, &Block, sizeof(T))))
	{
		return;
	}

	Last = Block;
	Store(Buffers[T::Slot], &Block, sizeof(T));
}

void CShaderConstants::Flush()
{
	if (!Bound)
	{
		for (ERHI_SHADER_TYPE Stage : { ERHI_SHADER_TYPE::VS, ERHI_SHADER_TYPE::PS, ERHI_SHADER_TYPE::GS, ERHI_SHADER_TYPE::HS, ERHI_SHADER_TYPE::DS, ERHI_SHADER_TYPE::CS })
		{
			GRHI->SetConstantBuffers(0, Count, Buffers, Stage);
		}
		Bound = true;
	}

	if (Dirty)
	{
		std::apply([this](auto&... Block) { (Upload(Block, false), ...); }, Shadow);
		Dirty = 0;
	}
}

void CShaderConstants::UpdateFrame()
{
	InvalidateBindings();

	CBFrame& Frame = Write<CBFrame>();
	const float Time = Device.fTimeGlobal;
	Frame.timers.set(Time, Time - Device.fTimeDelta, Time * 0.1f, std::sin(Time));
	if (g_pGamePersistent && g_pGamePersistent->Environment().CurrentEnv)
	{
		CEnvDescriptorMixer& Env = *g_pGamePersistent->Environment().CurrentEnv;
		const Fmatrix& M = Device.mFullTransform;
		Fvector4 Plane;
		Plane.set(-(M._14 + M._13), -(M._24 + M._23), -(M._34 + M._33), -(M._44 + M._43));
		Plane.mul(-1.0f / _sqrt(_sqr(Plane.x) + _sqr(Plane.y) + _sqr(Plane.z)));
		const float Near = Env.fog_near;
		const float Far = Env.fog_far;
		const float Range = 1.0f / (Far - Near);
		Frame.fog_plane.set(-Plane.x * Range, -Plane.y * Range, -Plane.z * Range, 1 - (Plane.w - Near) * Range);
		Frame.fog_params.set(-Near * Range, Near, Far, Range);
		Frame.L_sun_dir_w.set(Env.sun_dir.x, Env.sun_dir.y, Env.sun_dir.z, 0);
		Fvector SunDir;
		Device.mView.transform_dir(SunDir, Env.sun_dir);
		SunDir.normalize();
		Frame.L_sun_dir_e.set(SunDir.x, SunDir.y, SunDir.z, 0);
		if (LightingModeIsStaticRender())
		{
			Frame.fog_color.set(Env.fog_color.x * ps_r1_fog_luminance, Env.fog_color.y * ps_r1_fog_luminance, Env.fog_color.z * ps_r1_fog_luminance, 0);
			Frame.L_sun_color.set(Env.sun_color.x, Env.sun_color.y, Env.sun_color.z, 0);
			Frame.L_ambient.set(Env.ambient.x, Env.ambient.y, Env.ambient.z, Env.weight);
			Frame.L_hemi_color = Env.hemi_color;
			Frame.L_sky_color.set(Env.sky_color.x, Env.sky_color.y, Env.sky_color.z, Env.sky_rotation);
		}
		else
		{
			const Fvector Hemi = Env.old_style ? Env.sky_color : Fvector{ Env.hemi_color.x, Env.hemi_color.y, Env.hemi_color.z };
			Frame.fog_color.set(Env.fog_color.x, Env.fog_color.y, Env.fog_color.z, 0);
			Frame.L_sun_color.set(Env.sun_color.x * ps_r2_sun_lumscale, Env.sun_color.y * ps_r2_sun_lumscale, Env.sun_color.z * ps_r2_sun_lumscale, 0);
			Frame.L_ambient.set(Env.ambient.x * ps_r2_sun_lumscale_amb * 2, Env.ambient.y * ps_r2_sun_lumscale_amb * 2, Env.ambient.z * ps_r2_sun_lumscale_amb * 2, Env.weight);
			Frame.L_hemi_color.set(Hemi.x * ps_r2_sun_lumscale_hemi * 4, Hemi.y * ps_r2_sun_lumscale_hemi * 4, Hemi.z * ps_r2_sun_lumscale_hemi * 4, Env.weight);
			Frame.L_sky_color.set(Env.sky_color.x * ps_r2_sun_lumscale_sky, Env.sky_color.y * ps_r2_sun_lumscale_sky, Env.sky_color.z * ps_r2_sun_lumscale_sky, Env.sky_rotation);
		}
		Frame.water_intensity.set(Env.m_fWaterIntensity, Env.m_fWaterIntensity, Env.m_fWaterIntensity, 0);
		Frame.sun_shafts_intensity.set(Env.m_fSunShaftsIntensity, Env.m_fSunShaftsIntensity, Env.m_fSunShaftsIntensity, 0);
		Frame.rain_params.set(Env.rain_density, g_pGamePersistent->Environment().wetness_factor, 0, g_pGameLevel ? (float)g_pGameLevel->UseSnowmask : 0.f);
		CEffect_Rain* Rain = g_pGamePersistent->Environment().eff_Rain;
		Frame.hud_rain.set(Rain ? Rain->HudDropsTime() : 0.f, Rain && UseWeaponRainDrops ? Rain->HudDropsAmount() : 0.f, 0.f, 0.f);
	}

	const auto& Hud = RDEVICE.hudViewportData;
	Frame.nvg_color.set(::nvg_color.r, ::nvg_color.g, ::nvg_color.b, ::nvg_color.a);
	Frame.m_hud_params.set(float(Hud.isRenderProcess), float(Hud.isRenderActive), 0, Hud.renderZoomRotateFactor);
	Frame.m_zoom_deviation.set(0, 0, Hud.renderScopeBrightnessValue, Hud.renderScopeBrightnessJitterValue);
	Frame.m_affects.set(Hud.CurrentElectronicsProblemsCnt / 10.0f, ::Random.randF(0, 1), Hud.TargetElectronicsProblemsCnt / 10.0f, Hud.IsElectronicsProblemsDecreasing ? 1.0f : 0.0f);
	Frame.m_actor_params.set(Hud.ActorHealth, Hud.ActorOutfitCondition, Hud.ActorWeaponCondition, Hud.ActorWeaponLoading);
	Frame.m_digiclock.set(0.f, 0.f, 0.f, 0.f);
	if (g_pGameLevel && g_pGameLevel->bReady && 0 == Device.dwPrecacheFrame)
	{
		u32 Year, Month, Day, Hours, Mins, Secs, Millis;
		split_time(g_pGameLevel->GetGameTime(), Year, Month, Day, Hours, Mins, Secs, Millis);
		Frame.m_digiclock.set((Hours / 10) / 10.f, (Hours % 10) / 10.f, (Mins / 10) / 10.f, (Mins % 10) / 10.f);
		const float SecFraction = Secs / 60.f;
		const float MinFraction = (SecFraction + Mins) / 60.f;
		const float HourFraction = (MinFraction + Hours) / 12.f;
		Frame.m_timearrow.set(std::sin(PI_MUL_2 * HourFraction), std::cos(PI_MUL_2 * HourFraction), std::sin(PI_MUL_2 * MinFraction), std::cos(PI_MUL_2 * MinFraction));
		float Heading, Pitch;
		RDEVICE.vCameraDirection.getHP(Heading, Pitch);
		Frame.m_timearrow2.set(std::sin(PI_MUL_2 * SecFraction), std::cos(PI_MUL_2 * SecFraction), std::sin(Pitch), std::cos(Pitch));
	}
	Frame.test_exp_to_shaders_1.set(ps_r__test_exp_to_shaders_1, 0, 0, 0);
	Frame.test_exp_to_shaders_2.set(ps_r__test_exp_to_shaders_2, 0, 0, 0);
	if (g_pGamePersistent)
	{
		const CEnvironment& Env = g_pGamePersistent->Environment();
		Frame.env_wind.set(Env.wind_blast_direction.x, Env.wind_blast_direction.y, Env.wind_blast_direction.z, Env.wind_strength_factor);
	}
}

void CShaderConstants::UpdateView(const R_xforms& X)
{
	CBView& View = Write<CBView>();
	Fmatrix VP;
	VP.mul(X.m_p, X.m_v);
	Fmatrix VPOld;
	VPOld.mul(X.m_p_old, X.m_v_old);
	View.m_V = X.m_v;
	View.m_P = X.m_p;
	View.m_VP = VP;
	View.m_invV = Inverse43(X.m_v);
	View.m_invP = Inverse44(X.m_p);
	View.m_VP_old = VPOld;
	View.m_invVP_old = Inverse44(VPOld);
	View.eye_position = Device.vCameraPosition;
	View.eye_direction = Device.vCameraDirection;
	View.eye_normal = Device.vCameraTop;
	View.pos_decompression_params2.set(RCache.get_width(), RCache.get_height(), 1.0f / RCache.get_width(), 1.0f / RCache.get_height());

	CBPass& Pass = Write<CBPass>();
	Pass.m_P_hud = Device.mProject_hud;
	Pass.m_invP_hud = Inverse44(Device.mProject_hud);

	if (!Device.IsEditorMode())
	{
		CBLight& Light = Write<CBLight>();
		for (u32 Index = 0; Index < 3 && Index < RImplementation.m_sun_cascades.size(); ++Index)
		{
			const Fmatrix Adjust = { 0.5f, 0, 0, 0, 0, -0.5f, 0, 0, 0, 0, 1, 0, 0.5f, 0.5f, RImplementation.m_sun_cascades[Index].bias, 1 };
			Fmatrix Shadow;
			Shadow.mul(Adjust, RImplementation.m_sun_cascades[Index].xform);
			Light.m_shadow_sun[Index] = Shadow;
		}
	}

	CBObject& Object = Write<CBObject>();
	Object.m_WV = X.m_wv;
	Object.m_WVP = X.m_wvp;
	Object.m_WVP_old = X.m_wvp_old;
	UpdateScreen();
}

void CShaderConstants::UpdateObject(const R_xforms& X)
{
	CBObject& Object = Write<CBObject>();
	Fmatrix InvW;
	InvW.invert_b(X.m_w);
	Object.m_W = X.m_w;
	Object.m_invW = InvW;
	Object.m_WV = X.m_wv;
	Object.m_WVP = X.m_wvp;
	Object.m_WVP_old = X.m_wvp_old;
}

void CShaderConstants::ResetMaterial()
{
	CBMaterial& Material = Write<CBMaterial>();
	Material = {};
	Material.parallax.set(ps_r2_df_parallax_h, -ps_r2_df_parallax_h / 2, 1.0f / r_dtex_range, 1.0f / r_dtex_range);
	Material.def_aref = ps_r2_def_aref_quality / 255.0f;
	Material.tfactor.set(1.0f, 1.0f, 1.0f, 1.0f);
}

void CShaderConstants::SetLOD(float LOD)
{
	const float Factor = clampr(ceil(LOD * LOD * LOD * LOD * LOD * 8.0f), 1.0f, 7.0f);
	Write<CBMaterial>().triLOD.set(Factor, Factor, Factor, Factor);
}

void CShaderConstants::UpdateScreen()
{
	CBView& View = Write<CBView>();
	const float TargetWidth = RCache.get_target_width();
	const float TargetHeight = RCache.get_target_height();
	const float Width = RCache.get_width();
	const float Height = RCache.get_height();
	View.screen_res.set(TargetWidth, TargetHeight, 1.0f / TargetWidth, 1.0f / TargetHeight);
	View.scaled_screen_res.set(Width, Height, 1.0f / Width, 1.0f / Height);
	View.m_taa_jitter.set(ps_r_taa_jitter.x, ps_r_taa_jitter.y, ps_r_taa_jitter.z, float(Device.dwFrame));
}

void CShaderConstants::OnShaderBind(const R_xforms& X)
{
	UpdateScreen();

#ifdef _EDITOR
	Write<CBMaterial>().def_aref = 100.f / 255.f;
#else
	Write<CBMaterial>().def_aref = ps_r2_def_aref_quality / 255.f;
#endif

	Fmatrix Adjust = Fidentity;
	Adjust._11 = 0.5f;
	Adjust._22 = -0.5f;
	Adjust._41 = 0.5f;
	Adjust._42 = 0.5f;
	Fmatrix Texgen;
	Texgen.mul(Adjust, X.m_wvp);
	Write<CBPass>().m_texgen = Texgen;
}

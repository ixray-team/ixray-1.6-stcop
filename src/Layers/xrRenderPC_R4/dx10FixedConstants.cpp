#include "stdafx.h"
#include "dx10FixedConstants.h"

#include "dxRenderDeviceRender.h"
#include "xrRender_console.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/Environment.h"
#include "../../xrEngine/EngineAPI.h"
#include "../../xrEngine/date_time.h"
#include "../../xrEngine/Rain.h"

static constexpr u32 chash(const char* s, u32 h = 2166136261u)
{
	return *s ? chash(s + 1, (h ^ (u32)(u8)*s) * 16777619u) : h;
}

extern float r_dtex_range;
extern ENGINE_API Fcolor nvg_color;

static IRHIBuffer* cb_frame = nullptr;
static IRHIBuffer* cb_view = nullptr;
static IRHIBuffer* cb_object = nullptr;
static IRHIBuffer* cb_material = nullptr;
static IRHIBuffer* cb_light = nullptr;
static IRHIBuffer* cb_pass = nullptr;

static CBFrame cpu_frame{};
static CBView cpu_view{};
static CBObject cpu_object{};
static CBMaterial cpu_material{};
static CBLight cpu_light{};
static CBPass cpu_pass{};

static bool dirty_frame = true, dirty_view = true, dirty_object = true, dirty_material = true, dirty_light = true, dirty_pass = true;

static constexpr bool fixed_cb_roundtrip()
{
	const shader_float4 r0(1.f, 2.f, 3.f, 4.f);
	const shader_float4 r1(5.f, 6.f, 7.f, 8.f);
	const shader_float4 r2(9.f, 10.f, 11.f, 12.f);
	const shader_float4 r3(13.f, 14.f, 15.f, 16.f);
	shader_float4 s0, s1, s2, s3;
	encode_float4x4(s0, s1, s2, s3, r0, r1, r2, r3);
	if (!(s0 == shader_float4(1.f, 5.f, 9.f, 13.f))) return false;
	if (!(s1 == shader_float4(2.f, 6.f, 10.f, 14.f))) return false;
	if (!(s2 == shader_float4(3.f, 7.f, 11.f, 15.f))) return false;
	if (!(s3 == shader_float4(4.f, 8.f, 12.f, 16.f))) return false;
	shader_float4 d0, d1, d2, d3;
	decode_float4x4(d0, d1, d2, d3, s0, s1, s2, s3);
	if (!(d0 == r0 && d1 == r1 && d2 == r2 && d3 == r3)) return false;

	shader_float4 t0, t1, t2;
	encode_float3x4(t0, t1, t2, r0, r1, r2, r3);
	if (!(t0 == shader_float4(1.f, 5.f, 9.f, 13.f))) return false;
	if (!(t1 == shader_float4(2.f, 6.f, 10.f, 14.f))) return false;
	if (!(t2 == shader_float4(3.f, 7.f, 11.f, 15.f))) return false;
	shader_float4 a0, a1, a2, a3;
	decode_float3x4(a0, a1, a2, a3, t0, t1, t2);
	return a0 == shader_float4(1.f, 2.f, 3.f, 0.f)
		&& a1 == shader_float4(5.f, 6.f, 7.f, 0.f)
		&& a2 == shader_float4(9.f, 10.f, 11.f, 0.f)
		&& a3 == shader_float4(13.f, 14.f, 15.f, 1.f);
}
static_assert(fixed_cb_roundtrip());
static_assert(sizeof(CBFrame) == 28 * 16);
static_assert(sizeof(CBView) == 33 * 16);
static_assert(sizeof(CBObject) == 24 * 16);
static_assert(sizeof(CBMaterial) == 12 * 16);
static_assert(sizeof(CBLight) == 27 * 16);
static_assert(sizeof(CBPass) == 38 * 16);
static_assert(offsetof(CBFrame, hud_rain) == 27 * 16);
static_assert(offsetof(CBView, eye_position) == 26 * 16);
static_assert(offsetof(CBView, m_VP_old) == 18 * 16);
static_assert(offsetof(CBObject, L_dynamic_props) == 17 * 16);
static_assert(offsetof(CBObject, m_WVP_old) == 13 * 16);
static_assert(offsetof(CBMaterial, def_aref) == 5 * 16);
static_assert(offsetof(CBMaterial, L_model_light_color) == 6 * 16);
static_assert(offsetof(CBMaterial, m_lmap) == 9 * 16);
static_assert(offsetof(CBLight, Ldynamic_hud) == 3 * 16);
static_assert(offsetof(CBLight, m_shadow_sun) == 4 * 16);
static_assert(offsetof(CBLight, L_dynamic_xform) == 23 * 16);
static_assert(offsetof(CBPass, mblur_params) == 29 * 16);
static_assert(offsetof(CBPass, m_reflectionV) == 30 * 16);
static_assert(offsetof(CBPass, reflection_history_jitter) == 37 * 16);

static bool store_Float3x4(shader_float4 dst[3], const Fmatrix& m)
{
	shader_float4 s0, s1, s2;
	encode_float3x4(s0, s1, s2,
		shader_float4(m._11, m._12, m._13, m._14),
		shader_float4(m._21, m._22, m._23, m._24),
		shader_float4(m._31, m._32, m._33, m._34),
		shader_float4(m._41, m._42, m._43, m._44));
	if (s0 == dst[0] && s1 == dst[1] && s2 == dst[2])
		return false;
	dst[0] = s0;
	dst[1] = s1;
	dst[2] = s2;
	return true;
}
static bool store_Float4x4(shader_float4 dst[4], const Fmatrix& m)
{
	shader_float4 s0, s1, s2, s3;
	encode_float4x4(s0, s1, s2, s3,
		shader_float4(m._11, m._12, m._13, m._14),
		shader_float4(m._21, m._22, m._23, m._24),
		shader_float4(m._31, m._32, m._33, m._34),
		shader_float4(m._41, m._42, m._43, m._44));
	if (s0 == dst[0] && s1 == dst[1] && s2 == dst[2] && s3 == dst[3])
		return false;
	dst[0] = s0;
	dst[1] = s1;
	dst[2] = s2;
	dst[3] = s3;
	return true;
}
static bool set4(shader_float4& d, float x, float y, float z, float w)
{
	if (d.x == x && d.y == y && d.z == z && d.w == w)
		return false;
	d.set(x, y, z, w);
	return true;
}
static bool set4(shader_float4& d, const Fvector4& A)
{
	return set4(d, A.x, A.y, A.z, A.w);
}
static bool set1(float& d, float v)
{
	if (d == v)
		return false;
	d = v;
	return true;
}
static bool updateBuffer(IRHIBuffer* buf, const void* data, u32 size)
{
	if (!buf)
		return false;
	RHIMappedSubresource m{};
	if (!buf->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &m))
		return false;
	CopyMemory(m.pData, data, size);
	buf->Unmap();
	return true;
}

static IRHIBuffer* s_bound[6][FixedConstants::kSlots];

static void bindAllStages()
{
	IRHIBuffer* bufs[FixedConstants::kSlots] = {cb_frame, cb_view, cb_object, cb_material, cb_light, cb_pass};
	static const ERHI_SHADER_TYPE stages[6] =
		{ERHI_SHADER_TYPE::VS, ERHI_SHADER_TYPE::PS, ERHI_SHADER_TYPE::GS, ERHI_SHADER_TYPE::HS, ERHI_SHADER_TYPE::DS, ERHI_SHADER_TYPE::CS};

	for (u32 i = 0; i < 6; ++i)
	{
		if (!std::memcmp(s_bound[i], bufs, sizeof(bufs)))
		{
			continue;
		}
		std::memcpy(s_bound[i], bufs, sizeof(bufs));
		GRHI->SetConstantBuffers(0, FixedConstants::kSlots, bufs, stages[i]);
	}
}

void FixedConstants::Create()
{
	RHIUtils::CreateConstantBuffer(&cb_frame, sizeof(CBFrame));
	RHIUtils::CreateConstantBuffer(&cb_view, sizeof(CBView));
	RHIUtils::CreateConstantBuffer(&cb_object, sizeof(CBObject));
	RHIUtils::CreateConstantBuffer(&cb_material, sizeof(CBMaterial));
	RHIUtils::CreateConstantBuffer(&cb_light, sizeof(CBLight));
	RHIUtils::CreateConstantBuffer(&cb_pass, sizeof(CBPass));

	cpu_object.L_dynamic_props.set(0, 0, 0, 0);
	store_Float4x4(cpu_object.m_plmap_xform, Fidentity);
	cpu_object.m_plmap_clamp[0].set(0, 0, 0, 1);
	cpu_object.m_plmap_clamp[1].set(0, 0, 0, 0);
	SetReflectionCapture(Fidentity, 0.f, false);
	cpu_pass.reflection_history_jitter.set(0, 0, 0, 0);
	UpdateMaterial();
	UpdateObject(Fidentity);
	UpdateView();
}
void FixedConstants::Destroy()
{
	_RELEASE(cb_frame);
	_RELEASE(cb_view);
	_RELEASE(cb_object);
	_RELEASE(cb_material);
	_RELEASE(cb_light);
	_RELEASE(cb_pass);
}
void FixedConstants::UpdateFrame()
{
	InvalidateBindings();
	const CBFrame frame_prev = cpu_frame;
	float t = Device.fTimeGlobal;
	cpu_frame.timers.set(t, t - Device.fTimeDelta, t * 0.1f, std::sin(t));
	if (g_pGamePersistent && g_pGamePersistent->Environment().CurrentEnv)
	{
		auto* env = g_pGamePersistent->Environment().CurrentEnv;
		Fmatrix& M = Device.mFullTransform;
		Fvector4 plane;
		plane.x = -(M._14 + M._13);
		plane.y = -(M._24 + M._23);
		plane.z = -(M._34 + M._33);
		plane.w = -(M._44 + M._43);
		float denom = -1.0f / _sqrt(_sqr(plane.x) + _sqr(plane.y) + _sqr(plane.z));
		plane.mul(denom);
		float n = env->fog_near, f = env->fog_far, r = 1.0f / (f - n);
		cpu_frame.fog_plane.set(-plane.x * r, -plane.y * r, -plane.z * r, 1 - (plane.w - n) * r);
		cpu_frame.fog_params.set(-n * r, n, f, r);
		cpu_frame.fog_color.set(env->fog_color.x, env->fog_color.y, env->fog_color.z, 0);
		cpu_frame.L_sun_color.set(env->sun_color.x * ps_r2_sun_lumscale, env->sun_color.y * ps_r2_sun_lumscale, env->sun_color.z * ps_r2_sun_lumscale, 0);
		cpu_frame.L_sun_dir_w.set(env->sun_dir.x, env->sun_dir.y, env->sun_dir.z, 0);
		Fvector D;
		Device.mView.transform_dir(D, env->sun_dir);
		D.normalize();
		cpu_frame.L_sun_dir_e.set(D.x, D.y, D.z, 0);
		CEnvDescriptorMixer& m = *g_pGamePersistent->Environment().CurrentEnv;
		cpu_frame.L_ambient.set(m.ambient.x * ps_r2_sun_lumscale_amb * 2, m.ambient.y * ps_r2_sun_lumscale_amb * 2, m.ambient.z * ps_r2_sun_lumscale_amb * 2, m.weight);
		if (m.old_style)
		{
			cpu_frame.L_hemi_color.set(m.sky_color.x * ps_r2_sun_lumscale_hemi * 4, m.sky_color.y * ps_r2_sun_lumscale_hemi * 4, m.sky_color.z * ps_r2_sun_lumscale_hemi * 4, m.weight);
		}
		else
		{
			cpu_frame.L_hemi_color.set(m.hemi_color.x * ps_r2_sun_lumscale_hemi * 4, m.hemi_color.y * ps_r2_sun_lumscale_hemi * 4, m.hemi_color.z * ps_r2_sun_lumscale_hemi * 4, m.weight);
		}
		cpu_frame.L_sky_color.set(m.sky_color.x * ps_r2_sun_lumscale_sky, m.sky_color.y * ps_r2_sun_lumscale_sky, m.sky_color.z * ps_r2_sun_lumscale_sky, m.sky_rotation);
		if (LightingModeIsStatic() && !Device.IsEditorMode())
		{
			cpu_frame.fog_color.set(env->fog_color.x * ps_r1_fog_luminance, env->fog_color.y * ps_r1_fog_luminance, env->fog_color.z * ps_r1_fog_luminance, 0);
			cpu_frame.L_sun_color.set(env->sun_color.x, env->sun_color.y, env->sun_color.z, 0);
			cpu_frame.L_ambient.set(m.ambient.x, m.ambient.y, m.ambient.z, m.weight);
			cpu_frame.L_hemi_color.set(m.hemi_color);
			cpu_frame.L_sky_color.set(m.sky_color.x, m.sky_color.y, m.sky_color.z, m.sky_rotation);
		}
		cpu_frame.water_intensity.set(m.m_fWaterIntensity, m.m_fWaterIntensity, m.m_fWaterIntensity, 0);
		cpu_frame.sun_shafts_intensity.set(m.m_fSunShaftsIntensity, m.m_fSunShaftsIntensity, m.m_fSunShaftsIntensity, 0);
		// no level at the main menu
		const float snowmask = g_pGameLevel ? (float)g_pGameLevel->UseSnowmask : 0.f;
		cpu_frame.rain_params.set(m.rain_density, g_pGamePersistent->Environment().wetness_factor, 0, snowmask);
		if (CEffect_Rain* rain = g_pGamePersistent->Environment().eff_Rain)
			cpu_frame.hud_rain.set(rain->HudDropsTime(), UseWeaponRainDrops ? rain->HudDropsAmount() : 0.f, 0.f, 0.f);
		else
			cpu_frame.hud_rain.set(0.f, 0.f, 0.f, 0.f);
	}
	cpu_frame.nvg_color.set(::nvg_color.r, ::nvg_color.g, ::nvg_color.b, ::nvg_color.a);
	cpu_frame.m_hud_params.set(float(RDEVICE.hudViewportData.isRenderProcess), float(RDEVICE.hudViewportData.isRenderActive), 0, RDEVICE.hudViewportData.renderZoomRotateFactor);
	cpu_frame.m_zoom_deviation.set(0, 0, RDEVICE.hudViewportData.renderScopeBrightnessValue, RDEVICE.hudViewportData.renderScopeBrightnessJitterValue);
	float decr = RDEVICE.hudViewportData.IsElectronicsProblemsDecreasing ? 1.0f : 0.0f;
	cpu_frame.m_affects.set(RDEVICE.hudViewportData.CurrentElectronicsProblemsCnt / 10.0f, ::Random.randF(0, 1), RDEVICE.hudViewportData.TargetElectronicsProblemsCnt / 10.0f, decr);
	cpu_frame.m_actor_params.set(RDEVICE.hudViewportData.ActorHealth, RDEVICE.hudViewportData.ActorOutfitCondition, RDEVICE.hudViewportData.ActorWeaponCondition, RDEVICE.hudViewportData.ActorWeaponLoading);
	if (g_pGameLevel && g_pGameLevel->bReady && 0 == Device.dwPrecacheFrame)
	{
		u32 y, mn, d, h, mi, s, ms;
		split_time(g_pGameLevel->GetGameTime(), y, mn, d, h, mi, s, ms);
		float sf = s / 60.f, mf = (sf + mi) / 60.f, hf = (mf + h) / 12.f;
		cpu_frame.m_timearrow.set(std::sin(PI_MUL_2 * hf), std::cos(PI_MUL_2 * hf), std::sin(PI_MUL_2 * mf), std::cos(PI_MUL_2 * mf));
		float sa = PI_MUL_2 * sf, hp;
		RDEVICE.vCameraDirection.getHP(hp, *(float*)&hp);
		cpu_frame.m_timearrow2.set(std::sin(sa), std::cos(sa), 0, 0);
		RDEVICE.vCameraDirection.getHP(hp, *(float*)&hp);
		cpu_frame.m_timearrow2.z = std::sin(hp);
		cpu_frame.m_timearrow2.w = std::cos(hp);
	}
	cpu_frame.test_exp_to_shaders_1.set(ps_r__test_exp_to_shaders_1, 0, 0, 0);
	cpu_frame.test_exp_to_shaders_2.set(ps_r__test_exp_to_shaders_2, 0, 0, 0);
	if (g_pGamePersistent)
	{
		const CEnvironment& E = g_pGamePersistent->Environment();
		cpu_frame.env_wind.set(E.wind_blast_direction.x, E.wind_blast_direction.y, E.wind_blast_direction.z, E.wind_strength_factor);
	}
	if (std::memcmp(&frame_prev, &cpu_frame, sizeof(frame_prev)) != 0)
		dirty_frame = true;
	BindFrame();
}

static bool matrix_usable(const Fmatrix& m)
{
	return (std::abs(m._11) + std::abs(m._22) + std::abs(m._33)) > EPS_S;
}

static void inv43(Fmatrix& d, const Fmatrix& s)
{
	if (matrix_usable(s))
	{
		d.invert(s);
	}
	else
	{
		d.identity();
	}
}

static void inv44(Fmatrix& d, const Fmatrix& s)
{
	if (matrix_usable(s))
	{
		d.invert44(s);
	}
	else
	{
		d.identity();
	}
}

void FixedConstants::UpdateView()
{
	const CBView view_prev = cpu_view;
	const CBPass pass_prev = cpu_pass;
	const CBLight light_prev = cpu_light;
	const CBObject object_prev = cpu_object;
	const Fmatrix& mV = RCache.xforms.m_v;
	const Fmatrix& mP = RCache.xforms.m_p;
	const Fmatrix& mV_old = RCache.xforms.m_v_old;
	const Fmatrix& mP_old = RCache.xforms.m_p_old;

	store_Float3x4(cpu_view.m_V, mV);
	store_Float4x4(cpu_view.m_P, mP);
	Fmatrix vp;
	vp.mul(mP, mV);
	store_Float4x4(cpu_view.m_VP, vp);
	Fmatrix invV;
	inv43(invV, mV);
	store_Float3x4(cpu_view.m_invV, invV);
	Fmatrix invP;
	inv44(invP, mP);
	store_Float4x4(cpu_view.m_invP, invP);
	Fmatrix invP_hud;
	inv44(invP_hud, Device.mProject_hud);
	store_Float4x4(cpu_pass.m_invP_hud, invP_hud);
	store_Float4x4(cpu_pass.m_P_hud, Device.mProject_hud);
	Fmatrix vp_old;
	vp_old.mul(mP_old, mV_old);
	store_Float4x4(cpu_view.m_VP_old, vp_old);
	Fmatrix invVP_old;
	inv44(invVP_old, vp_old);
	store_Float4x4(cpu_view.m_invVP_old, invVP_old);
	cpu_view.eye_position.set(Device.vCameraPosition.x, Device.vCameraPosition.y, Device.vCameraPosition.z, 1);
	cpu_view.eye_direction.set(Device.vCameraDirection.x, Device.vCameraDirection.y, Device.vCameraDirection.z, 0);
	cpu_view.eye_normal.set(Device.vCameraTop.x, Device.vCameraTop.y, Device.vCameraTop.z, 0);
	cpu_view.m_taa_jitter.set(ps_r_taa_jitter.x, ps_r_taa_jitter.y, ps_r_taa_jitter.z, float(Device.dwFrame));
	cpu_view.screen_res.set(float(RDEVICE.TargetWidth), float(RDEVICE.TargetHeight), 1.0f / float(RDEVICE.TargetWidth), 1.0f / float(RDEVICE.TargetHeight));
	cpu_view.scaled_screen_res.set(RCache.get_width(), RCache.get_height(), 1.0f / RCache.get_width(), 1.0f / RCache.get_height());
	cpu_view.pos_decompression_params2.set(RCache.get_width(), RCache.get_height(), 1.0f / RCache.get_width(), 1.0f / RCache.get_height());

#ifndef _EDITOR
	for (int i = 0; i < 3 && i < (int)RImplementation.m_sun_cascades.size(); ++i)
	{
		Fmatrix adj{0.5f, 0, 0, 0, 0, -0.5f, 0, 0, 0, 0, 1, 0, 0.5f, 0.5f, RImplementation.m_sun_cascades[i].bias, 1};
		Fmatrix xf;
		xf.mul(adj, RImplementation.m_sun_cascades[i].xform);
		store_Float4x4(&cpu_light.m_shadow_sun[i * 4], xf);
	}
#endif

	const R_xforms& x = RCache.xforms;
	store_Float3x4(cpu_object.m_WV, x.m_wv);
	store_Float4x4(cpu_object.m_WVP, x.m_wvp);
	if (std::memcmp(&view_prev, &cpu_view, sizeof(view_prev)) != 0)
		dirty_view = true;
	if (std::memcmp(&pass_prev, &cpu_pass, sizeof(pass_prev)) != 0)
		dirty_pass = true;
	if (std::memcmp(&light_prev, &cpu_light, sizeof(light_prev)) != 0)
		dirty_light = true;
	if (std::memcmp(&object_prev, &cpu_object, sizeof(object_prev)) != 0)
		dirty_object = true;
	BindView();
}
void FixedConstants::SetReflectionHistory(const Fvector& jitter, bool isValid)
{
	dirty_pass |= set4(cpu_pass.reflection_history_jitter, jitter.x, jitter.y, isValid ? 1.f : 0.f, 0.f);
}

void FixedConstants::SetReflectionCapture(const Fmatrix& view, float radius, bool isValid)
{
	Fmatrix inverseView;
	inverseView.invert(view);
	bool changed = store_Float3x4(cpu_pass.m_reflectionV, view);
	changed = store_Float3x4(cpu_pass.m_invReflectionV, inverseView) || changed;
	changed = set4(cpu_pass.reflection_params, radius, isValid ? 1.f : 0.f, 0.f, 0.f) || changed;
	dirty_pass |= changed;
}

void FixedConstants::UpdateObject(const Fmatrix& mW)
{
	const CBObject object_prev = cpu_object;
	const R_xforms& x = RCache.xforms;
	store_Float3x4(cpu_object.m_W, mW);
	store_Float3x4(cpu_object.m_WV, x.m_wv);
	store_Float4x4(cpu_object.m_WVP, x.m_wvp);
	store_Float4x4(cpu_object.m_WVP_old, x.m_wvp_old);
	Fmatrix invW;
	invW.invert_b(mW);
	store_Float3x4(cpu_object.m_invW, invW);
	if (std::memcmp(&object_prev, &cpu_object, sizeof(object_prev)) != 0)
		dirty_object = true;
}
void FixedConstants::UpdateMaterial()
{
	const CBMaterial material_prev = cpu_material;
	cpu_material.L_material.set(0, 0, 0, 0);
	cpu_material.hemi_cube_pos_faces.set(0, 0, 0, 0);
	cpu_material.hemi_cube_neg_faces.set(0, 0, 0, 0);
	cpu_material.dt_params.set(0, 0, 0, 0);
	cpu_material.parallax.set(ps_r2_df_parallax_h, -ps_r2_df_parallax_h / 2, 1.0f / r_dtex_range, 1.0f / r_dtex_range);
	cpu_material.def_aref = ps_r2_def_aref_quality / 255.0f;
	cpu_material.m_AlphaRef = 0;
	cpu_material.L_model_light_color.set(0, 0, 0, 0);
	cpu_material.L_model_light_dir.set(0, 0, 0, 0);
	cpu_material.triLOD.set(0, 0, 0, 0);
	cpu_material.m_lmap[0].set(0, 0, 0, 0);
	cpu_material.m_lmap[1].set(0, 0, 0, 0);
	cpu_material.tfactor.set(1.0f, 1.0f, 1.0f, 1.0f);
	if (std::memcmp(&material_prev, &cpu_material, sizeof(material_prev)) != 0)
		dirty_material = true;
	BindMaterial();
}
void FixedConstants::BindFrame()
{
	bindAllStages();
}
void FixedConstants::BindView()
{
	bindAllStages();
}
void FixedConstants::BindObject()
{
	bindAllStages();
}
void FixedConstants::BindMaterial()
{
	bindAllStages();
}
void FixedConstants::BindLight()
{
	bindAllStages();
}
void FixedConstants::BindAll()
{
	bindAllStages();
}
void FixedConstants::InvalidateBindings()
{
	ZeroMemory(s_bound, sizeof(s_bound));
}

u32 FixedConstants::NameHash(const char* n)
{
	return n ? chash(n) : 0;
}

bool FixedConstants::IsFixedName(const char* n)
{
	return FixedClass(n) != 0;
}

int FixedConstants::FixedClass(const char* n)
{
	static const char* const owned[] =
		{"cb_frame", "cb_view", "cb_object", "cb_material", "cb_light", "cb_pass"};

	for (const char* f : owned)
	{
		if (!std::strcmp(n, f))
		{
			return 2;
		}
	}

	return 0;
}
void FixedConstants::Flush()
{
	if (dirty_frame && updateBuffer(cb_frame, &cpu_frame, sizeof(cpu_frame)))
		dirty_frame = false;
	if (dirty_view && updateBuffer(cb_view, &cpu_view, sizeof(cpu_view)))
		dirty_view = false;
	if (dirty_object && updateBuffer(cb_object, &cpu_object, sizeof(cpu_object)))
		dirty_object = false;
	if (dirty_material && updateBuffer(cb_material, &cpu_material, sizeof(cpu_material)))
		dirty_material = false;
	if (dirty_light && updateBuffer(cb_light, &cpu_light, sizeof(cpu_light)))
		dirty_light = false;
	if (dirty_pass && updateBuffer(cb_pass, &cpu_pass, sizeof(cpu_pass)))
		dirty_pass = false;
}
void FixedConstants::SetHemiMaterial(float x, float y, float z, float w)
{
	dirty_material |= set4(cpu_material.L_material, x, y, z, w);
}
void FixedConstants::SetHemiPosFaces(float x, float y, float z)
{
	dirty_material |= set4(cpu_material.hemi_cube_pos_faces, x, y, z, 0);
}
void FixedConstants::SetHemiNegFaces(float x, float y, float z)
{
	dirty_material |= set4(cpu_material.hemi_cube_neg_faces, x, y, z, 0);
}
void FixedConstants::SetHemiTfactor(const Fvector4& v)
{
	dirty_material |= set4(cpu_material.tfactor, v);
}
void FixedConstants::SetHemiTfactor(float x, float y, float z, float w)
{
	dirty_material |= set4(cpu_material.tfactor, x, y, z, w);
}
void FixedConstants::SetLitColor(const Fvector& c, const Fvector& d)
{
	bool changed = set4(cpu_material.L_model_light_color, c.x, c.y, c.z, 0);
	changed = set4(cpu_material.L_model_light_dir, d.x, d.y, d.z, 0) || changed;
	dirty_material |= changed;
}
void FixedConstants::SetDtParams(float x, float y, float z, float w)
{
	dirty_material |= set4(cpu_material.dt_params, x, y, z, w);
}
void FixedConstants::SetDtParamsScale(float s)
{
	dirty_material |= set4(cpu_material.dt_params, s, s, s, 1 / r_dtex_range);
}
void FixedConstants::SetParallax(float h)
{
	dirty_material |= set4(cpu_material.parallax, h, -h / 2, 1 / r_dtex_range, 1 / r_dtex_range);
}
void FixedConstants::SetAlphaRef(float a)
{
	dirty_material |= set1(cpu_material.m_AlphaRef, a);
}
void FixedConstants::SetLModelLight(const Fvector& c, const Fvector& d)
{
	bool changed = set4(cpu_material.L_model_light_color, c.x, c.y, c.z, 0);
	changed = set4(cpu_material.L_model_light_dir, d.x, d.y, d.z, 0) || changed;
	dirty_material |= changed;
}
void FixedConstants::SetTriLOD(float lod)
{
	dirty_material |= set4(cpu_material.triLOD, lod, lod, lod, lod);
}
void FixedConstants::SetTfactor(const Fvector4& v)
{
	dirty_material |= set4(cpu_material.tfactor, v);
}
void FixedConstants::SetTreeXform(const Fmatrix& m)
{
	dirty_pass |= store_Float4x4(cpu_pass.m_xform, m);
}
void FixedConstants::SetTreeXformV(const Fmatrix& m)
{
	dirty_pass |= store_Float4x4(cpu_pass.m_xform_v, m);
}
void FixedConstants::SetTreeConsts(float x, float y, float z, float w)
{
	dirty_pass |= set4(cpu_pass.consts, x, y, z, w);
}
void FixedConstants::SetTreeWave(const Fvector4& v)
{
	dirty_pass |= set4(cpu_pass.wave, v);
}
void FixedConstants::SetTreeWind(const Fvector4& v)
{
	dirty_pass |= set4(cpu_pass.wind, v);
}
void FixedConstants::SetTreeConstsOld(float x, float y, float z, float w)
{
	dirty_pass |= set4(cpu_pass.consts_old, x, y, z, w);
}
void FixedConstants::SetTreeWaveOld(const Fvector4& v)
{
	dirty_pass |= set4(cpu_pass.wave_old, v);
}
void FixedConstants::SetTreeWindOld(const Fvector4& v)
{
	dirty_pass |= set4(cpu_pass.wind_old, v);
}
void FixedConstants::SetTreeCScale(float x, float y, float z, float w)
{
	dirty_pass |= set4(cpu_pass.c_scale, x, y, z, w);
}
void FixedConstants::SetTreeCBias(float x, float y, float z, float w)
{
	dirty_pass |= set4(cpu_pass.c_bias, x, y, z, w);
}
void FixedConstants::SetTreeCSun(float x, float y, float z, float w)
{
	dirty_pass |= set4(cpu_pass.c_sun, x, y, z, w);
}
void FixedConstants::SetLMap(const Fmatrix& m)
{
	bool changed = set4(cpu_material.m_lmap[0], m._11, m._21, m._31, m._41);
	changed = set4(cpu_material.m_lmap[1], m._12, m._22, m._32, m._42) || changed;
	dirty_material |= changed;
}
void FixedConstants::SetShadow(const Fmatrix& m)
{
	dirty_light |= store_Float4x4(cpu_light.m_shadow, m);
}
void FixedConstants::SetShadowSun(int idx, const Fmatrix& m)
{
	if (idx >= 0 && idx < 3)
		dirty_light |= store_Float4x4(&cpu_light.m_shadow_sun[idx * 4], m);
}
void FixedConstants::SetLdynamic(const Fvector4& c, const Fvector4& p, const Fvector4& d)
{
	bool changed = set4(cpu_light.Ldynamic_color, c);
	changed = set4(cpu_light.Ldynamic_pos, p) || changed;
	changed = set4(cpu_light.Ldynamic_dir, d) || changed;
	dirty_light |= changed;
}
bool FixedConstants::OnSet(u32 h, const Fmatrix& A)
{
	switch (h)
	{
		case chash("m_plmap_xform"):
			dirty_object |= store_Float4x4(cpu_object.m_plmap_xform, A);
			break;
		case chash("L_dynamic_xform"):
			dirty_light |= store_Float4x4(cpu_light.L_dynamic_xform, A);
			break;
		case chash("m_W"):
			dirty_object |= store_Float3x4(cpu_object.m_W, A);
			break;
		case chash("m_invW"):
			dirty_object |= store_Float3x4(cpu_object.m_invW, A);
			break;
		case chash("m_WV"):
			dirty_object |= store_Float3x4(cpu_object.m_WV, A);
			break;
		case chash("m_WVP"):
			dirty_object |= store_Float4x4(cpu_object.m_WVP, A);
			break;
		case chash("m_V"):
			dirty_view |= store_Float3x4(cpu_view.m_V, A);
			break;
		case chash("m_invV"):
			dirty_view |= store_Float3x4(cpu_view.m_invV, A);
			break;
		case chash("m_P"):
			dirty_view |= store_Float4x4(cpu_view.m_P, A);
			break;
		case chash("m_VP"):
			dirty_view |= store_Float4x4(cpu_view.m_VP, A);
			break;
		case chash("m_invP"):
			dirty_view |= store_Float4x4(cpu_view.m_invP, A);
			break;
		case chash("m_invP_hud"):
			dirty_pass |= store_Float4x4(cpu_pass.m_invP_hud, A);
			break;
		case chash("m_P_hud"):
			dirty_pass |= store_Float4x4(cpu_pass.m_P_hud, A);
			break;
		case chash("m_xform"):
			dirty_pass |= store_Float4x4(cpu_pass.m_xform, A);
			break;
		case chash("m_xform_v"):
			dirty_pass |= store_Float4x4(cpu_pass.m_xform_v, A);
			break;
		case chash("m_shadow"):
			dirty_light |= store_Float4x4(cpu_light.m_shadow, A);
			break;
		case chash("m_sunmask"):
			dirty_light |= store_Float3x4(cpu_light.m_sunmask, A);
			break;
		// previous-frame matrices drive motion vectors; without these TAA reprojects against
		// zero and moving/skinned meshes smear
		case chash("m_WVP_old"):
			dirty_object |= store_Float4x4(cpu_object.m_WVP_old, A);
			break;
		case chash("m_VP_old"):
			dirty_view |= store_Float4x4(cpu_view.m_VP_old, A);
			break;
		case chash("m_invVP_old"):
			dirty_view |= store_Float4x4(cpu_view.m_invVP_old, A);
			break;
		case chash("m_texgen"):
			dirty_pass |= store_Float4x4(cpu_pass.m_texgen, A);
			break;
		default:
			return false;
	}
	return true;
}
bool FixedConstants::OnSet(u32 h, const Fvector4& A)
{
	switch (h)
	{
		case chash("L_dynamic_props"):
			dirty_object |= set4(cpu_object.L_dynamic_props, A);
			break;
		case chash("L_material"):
			SetHemiMaterial(A.x, A.y, A.z, A.w);
			break;
		case chash("hemi_cube_pos_faces"):
			SetHemiPosFaces(A.x, A.y, A.z);
			break;
		case chash("hemi_cube_neg_faces"):
			SetHemiNegFaces(A.x, A.y, A.z);
			break;
		case chash("dt_params"):
			SetDtParams(A.x, A.y, A.z, A.w);
			break;
		case chash("parallax"):
			SetParallax(A.x);
			break;
		case chash("L_model_light_color"):
			SetLModelLight(*reinterpret_cast<const Fvector*>(&A), Fvector{0, 0, 0});
			break;
		case chash("L_model_light_dir"):
			break;
		case chash("tfactor"):
			SetTfactor(A);
			break;
		case chash("consts"):
			SetTreeConsts(A.x, A.y, A.z, A.w);
			break;
		case chash("wave"):
			SetTreeWave(A);
			break;
		case chash("dir2D"):
		case chash("wind"):
			SetTreeWind(A);
			break;
		case chash("consts_old"):
			SetTreeConstsOld(A.x, A.y, A.z, A.w);
			break;
		case chash("wave_old"):
			SetTreeWaveOld(A);
			break;
		case chash("dir2D_old"):
		case chash("wind_old"):
			SetTreeWindOld(A);
			break;
		case chash("c_scale"):
			SetTreeCScale(A.x, A.y, A.z, A.w);
			break;
		case chash("c_bias"):
			SetTreeCBias(A.x, A.y, A.z, A.w);
			break;
		case chash("c_sun"):
			SetTreeCSun(A.x, A.y, A.z, A.w);
			break;
		case chash("L_dynamic_color"):
		case chash("Ldynamic_color"):
			dirty_light |= set4(cpu_light.Ldynamic_color, A);
			break;
		case chash("L_dynamic_pos"):
		case chash("Ldynamic_pos"):
			dirty_light |= set4(cpu_light.Ldynamic_pos, A);
			break;
		case chash("Ldynamic_dir"):
			dirty_light |= set4(cpu_light.Ldynamic_dir, A);
			break;
		case chash("c_brightness"):
			dirty_frame |= set4(cpu_frame.c_brightness, A);
			break;
		case chash("c_colormap"):
			dirty_frame |= set4(cpu_frame.c_colormap, A);
			break;
		case chash("color_params"):
			dirty_frame |= set4(cpu_frame.color_params, A);
			break;
		case chash("color_grading"):
			dirty_frame |= set4(cpu_frame.color_grading, A);
			break;
		case chash("fog_plane"):
			dirty_frame |= set4(cpu_frame.fog_plane, A);
			break;
		case chash("fog_params"):
			dirty_frame |= set4(cpu_frame.fog_params, A);
			break;
		case chash("fog_color"):
			dirty_frame |= set4(cpu_frame.fog_color, A);
			break;
		case chash("timers"):
			dirty_frame |= set4(cpu_frame.timers, A);
			break;
		case chash("eye_position"):
			dirty_view |= set4(cpu_view.eye_position, A);
			break;
		case chash("eye_direction"):
			dirty_view |= set4(cpu_view.eye_direction, A);
			break;
		case chash("eye_normal"):
			dirty_view |= set4(cpu_view.eye_normal, A);
			break;
		case chash("m_taa_jitter"):
			dirty_view |= set4(cpu_view.m_taa_jitter, A);
			break;
		case chash("L_sun_color"):
			dirty_frame |= set4(cpu_frame.L_sun_color, A);
			break;
		case chash("L_sun_dir_w"):
			dirty_frame |= set4(cpu_frame.L_sun_dir_w, A);
			break;
		case chash("L_sun_dir_e"):
			dirty_frame |= set4(cpu_frame.L_sun_dir_e, A);
			break;
		case chash("L_hemi_color"):
			dirty_frame |= set4(cpu_frame.L_hemi_color, A);
			break;
		case chash("L_ambient"):
			dirty_frame |= set4(cpu_frame.L_ambient, A);
			break;
		case chash("L_sky_color"):
			dirty_frame |= set4(cpu_frame.L_sky_color, A);
			break;
		case chash("water_intensity"):
			dirty_frame |= set4(cpu_frame.water_intensity, A);
			break;
		case chash("sun_shafts_intensity"):
			dirty_frame |= set4(cpu_frame.sun_shafts_intensity, A);
			break;
		case chash("rain_params"):
			dirty_frame |= set4(cpu_frame.rain_params, A);
			break;
		case chash("env_wind"):
			dirty_frame |= set4(cpu_frame.env_wind, A);
			break;
		case chash("mblur_params"):
			dirty_pass |= set4(cpu_pass.mblur_params, A);
			break;
		case chash("pos_decompression_params2"):
			dirty_view |= set4(cpu_view.pos_decompression_params2, A);
			break;
		default:
			return false;
	}
	return true;
}
bool FixedConstants::OnSet(u32 h, float A)
{
	switch (h)
	{
		case chash("def_aref"):
			dirty_material |= set1(cpu_material.def_aref, A);
			break;
		case chash("m_AlphaRef"):
			dirty_material |= set1(cpu_material.m_AlphaRef, A);
			break;
		case chash("triLOD"):
			dirty_material |= set4(cpu_material.triLOD, A, A, A, A);
			break;
		default:
			return false;
	}
	return true;
}
bool FixedConstants::OnSet(u32 h, int A)
{
	switch (h)
	{
		case chash("Ldynamic_hud"):
			if (cpu_light.Ldynamic_hud != A)
			{
				cpu_light.Ldynamic_hud = A;
				dirty_light = true;
			}
			break;
		default:
			return false;
	}
	return true;
}
bool FixedConstants::OnSetA(u32 h, u32 e, const Fvector4& A)
{
	switch (h)
	{
		case chash("m_plmap_clamp"):
			R_ASSERT(e < 2);
			dirty_object |= set4(cpu_object.m_plmap_clamp[e], A);
			break;
		case chash("m_lmap"):
			if (e < 2)
				dirty_material |= set4(cpu_material.m_lmap[e], A);
			break;
		case chash("L_dynamic_color"):
		case chash("Ldynamic_color"):
			if (e == 0)
				dirty_light |= set4(cpu_light.Ldynamic_color, A);
			break;
		case chash("L_dynamic_pos"):
		case chash("Ldynamic_pos"):
			if (e == 0)
				dirty_light |= set4(cpu_light.Ldynamic_pos, A);
			break;
		case chash("Ldynamic_dir"):
			if (e == 0)
				dirty_light |= set4(cpu_light.Ldynamic_dir, A);
			break;
		default:
			return false;
	}
	return true;
}
bool FixedConstants::OnSetA(u32 h, u32 e, const Fmatrix& A)
{
	switch (h)
	{
		case chash("m_shadow_sun"):
		{
			if (e < 3)
				dirty_light |= store_Float4x4(&cpu_light.m_shadow_sun[e * 4], A);
		}
		break;
		default:
			return false;
	}
	return true;
}

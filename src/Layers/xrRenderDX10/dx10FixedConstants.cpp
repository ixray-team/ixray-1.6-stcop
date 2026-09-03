#include "stdafx.h"
#include "dx10FixedConstants.h"

#include "../xrRender/dxRenderDeviceRender.h"
#include "../xrRender/xrRender_console.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/Environment.h"
#include "../../xrEngine/date_time.h"

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

static void store_Float3x4(Fvector4 dst[3], const Fmatrix& m)
{
	dst[0].set(m._11, m._21, m._31, m._41);
	dst[1].set(m._12, m._22, m._32, m._42);
	dst[2].set(m._13, m._23, m._33, m._43);
}
static void store_Float4x4(Fvector4 dst[4], const Fmatrix& m)
{
	dst[0].set(m._11, m._21, m._31, m._41);
	dst[1].set(m._12, m._22, m._32, m._42);
	dst[2].set(m._13, m._23, m._33, m._43);
	dst[3].set(m._14, m._24, m._34, m._44);
}
static void updateBuffer(IRHIBuffer* buf, const void* data, u32 size)
{
	if (!buf)
	{
		return;
	}
	RHIMappedSubresource m{};
	if (buf->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &m))
	{
		CopyMemory(m.pData, data, size);
		buf->Unmap();
	}
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
		cpu_frame.water_intensity.set(m.m_fWaterIntensity, m.m_fWaterIntensity, m.m_fWaterIntensity, 0);
		cpu_frame.sun_shafts_intensity.set(m.m_fSunShaftsIntensity, m.m_fSunShaftsIntensity, m.m_fSunShaftsIntensity, 0);
		// no level at the main menu
		const float snowmask = g_pGameLevel ? (float)g_pGameLevel->UseSnowmask : 0.f;
		cpu_frame.rain_params.set(m.rain_density, g_pGamePersistent->Environment().wetness_factor, 0, snowmask);
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

	dirty_view = true;
	dirty_pass = true;
	dirty_light = true;
	BindView();
}
void FixedConstants::UpdateObject(const Fmatrix& mW)
{
	const R_xforms& x = RCache.xforms;
	store_Float3x4(cpu_object.m_W, mW);
	store_Float3x4(cpu_object.m_WV, x.m_wv);
	store_Float4x4(cpu_object.m_WVP, x.m_wvp);
	store_Float4x4(cpu_object.m_WVP_old, x.m_wvp_old);
	Fmatrix invW;
	invW.invert_b(mW);
	store_Float3x4(cpu_object.m_invW, invW);
	dirty_object = true;
}
void FixedConstants::UpdateMaterial()
{
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
	cpu_material.tfactor.set(0, 0, 0, 0);
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
	if (dirty_frame)
	{
		updateBuffer(cb_frame, &cpu_frame, sizeof(cpu_frame));
		dirty_frame = false;
	}
	if (dirty_view)
	{
		updateBuffer(cb_view, &cpu_view, sizeof(cpu_view));
		dirty_view = false;
	}
	if (dirty_object)
	{
		updateBuffer(cb_object, &cpu_object, sizeof(cpu_object));
		dirty_object = false;
	}
	if (dirty_material)
	{
		updateBuffer(cb_material, &cpu_material, sizeof(cpu_material));
		dirty_material = false;
	}
	if (dirty_light)
	{
		updateBuffer(cb_light, &cpu_light, sizeof(cpu_light));
		dirty_light = false;
	}
	if (dirty_pass)
	{
		updateBuffer(cb_pass, &cpu_pass, sizeof(cpu_pass));
		dirty_pass = false;
	}
}
void FixedConstants::SetHemiMaterial(float x, float y, float z, float w)
{
	cpu_material.L_material.set(x, y, z, w);
	dirty_material = true;
}
void FixedConstants::SetHemiPosFaces(float x, float y, float z)
{
	cpu_material.hemi_cube_pos_faces.set(x, y, z, 0);
	dirty_material = true;
}
void FixedConstants::SetHemiNegFaces(float x, float y, float z)
{
	cpu_material.hemi_cube_neg_faces.set(x, y, z, 0);
	dirty_material = true;
}
void FixedConstants::SetHemiTfactor(const Fvector4& v)
{
	cpu_material.tfactor.set(v.x, v.y, v.z, v.w);
	dirty_material = true;
}
void FixedConstants::SetHemiTfactor(float x, float y, float z, float w)
{
	cpu_material.tfactor.set(x, y, z, w);
	dirty_material = true;
}
void FixedConstants::SetLitColor(const Fvector& c, const Fvector& d)
{
	cpu_material.L_model_light_color.set(c.x, c.y, c.z, 0);
	cpu_material.L_model_light_dir.set(d.x, d.y, d.z, 0);
	dirty_material = true;
}
void FixedConstants::SetDtParams(float x, float y, float z, float w)
{
	cpu_material.dt_params.set(x, y, z, w);
	dirty_material = true;
}
void FixedConstants::SetDtParamsScale(float s)
{
	cpu_material.dt_params.set(s, s, s, 1 / r_dtex_range);
	dirty_material = true;
}
void FixedConstants::SetParallax(float h)
{
	cpu_material.parallax.set(h, -h / 2, 1 / r_dtex_range, 1 / r_dtex_range);
	dirty_material = true;
}
void FixedConstants::SetAlphaRef(float a)
{
	cpu_material.m_AlphaRef = a;
	dirty_material = true;
}
void FixedConstants::SetLModelLight(const Fvector& c, const Fvector& d)
{
	cpu_material.L_model_light_color.set(c.x, c.y, c.z, 0);
	cpu_material.L_model_light_dir.set(d.x, d.y, d.z, 0);
	dirty_material = true;
}
void FixedConstants::SetTriLOD(float lod)
{
	cpu_material.triLOD.set(lod, lod, lod, lod);
	dirty_material = true;
}
void FixedConstants::SetTfactor(const Fvector4& v)
{
	cpu_material.tfactor.set(v.x, v.y, v.z, v.w);
	dirty_material = true;
}
void FixedConstants::SetTreeXform(const Fmatrix& m)
{
	store_Float4x4(cpu_pass.m_xform, m);
	dirty_pass = true;
}
void FixedConstants::SetTreeXformV(const Fmatrix& m)
{
	store_Float4x4(cpu_pass.m_xform_v, m);
	dirty_pass = true;
}
void FixedConstants::SetTreeConsts(float x, float y, float z, float w)
{
	cpu_pass.consts.set(x, y, z, w);
	dirty_pass = true;
}
void FixedConstants::SetTreeWave(const Fvector4& v)
{
	cpu_pass.wave.set(v.x, v.y, v.z, v.w);
	dirty_pass = true;
}
void FixedConstants::SetTreeWind(const Fvector4& v)
{
	cpu_pass.wind.set(v.x, v.y, v.z, v.w);
	dirty_pass = true;
}
void FixedConstants::SetTreeConstsOld(float x, float y, float z, float w)
{
	cpu_pass.consts_old.set(x, y, z, w);
	dirty_pass = true;
}
void FixedConstants::SetTreeWaveOld(const Fvector4& v)
{
	cpu_pass.wave_old.set(v.x, v.y, v.z, v.w);
	dirty_pass = true;
}
void FixedConstants::SetTreeWindOld(const Fvector4& v)
{
	cpu_pass.wind_old.set(v.x, v.y, v.z, v.w);
	dirty_pass = true;
}
void FixedConstants::SetTreeCScale(float x, float y, float z, float w)
{
	cpu_pass.c_scale.set(x, y, z, w);
	dirty_pass = true;
}
void FixedConstants::SetTreeCBias(float x, float y, float z, float w)
{
	cpu_pass.c_bias.set(x, y, z, w);
	dirty_pass = true;
}
void FixedConstants::SetTreeCSun(float x, float y, float z, float w)
{
	cpu_pass.c_sun.set(x, y, z, w);
	dirty_pass = true;
}
void FixedConstants::SetLMap(const Fmatrix& m)
{
	cpu_material.m_lmap[0].set(m._11, m._21, m._31, m._41);
	cpu_material.m_lmap[1].set(m._12, m._22, m._32, m._42);
	dirty_material = true;
}
void FixedConstants::SetShadow(const Fmatrix& m)
{
	store_Float4x4(cpu_light.m_shadow, m);
	dirty_light = true;
}
void FixedConstants::SetShadowSun(int idx, const Fmatrix& m)
{
	if (idx >= 0 && idx < 3)
	{
		store_Float4x4(&cpu_light.m_shadow_sun[idx * 4], m);
	}
	dirty_light = true;
}
void FixedConstants::SetLdynamic(const Fvector4& c, const Fvector4& p, const Fvector4& d)
{
	cpu_light.Ldynamic_color.set(c.x, c.y, c.z, c.w);
	cpu_light.Ldynamic_pos.set(p.x, p.y, p.z, p.w);
	cpu_light.Ldynamic_dir.set(d.x, d.y, d.z, d.w);
	dirty_light = true;
}
bool FixedConstants::OnSet(u32 h, const Fmatrix& A)
{
	switch (h)
	{
		case chash("m_W"):
		{
			store_Float3x4(cpu_object.m_W, A);
			dirty_object = true;
		}
		break;
		case chash("m_invW"):
		{
			store_Float3x4(cpu_object.m_invW, A);
			dirty_object = true;
		}
		break;
		case chash("m_WV"):
		{
			store_Float3x4(cpu_object.m_WV, A);
			dirty_object = true;
		}
		break;
		case chash("m_WVP"):
		{
			store_Float4x4(cpu_object.m_WVP, A);
			dirty_object = true;
		}
		break;
		case chash("m_V"):
		{
			store_Float3x4(cpu_view.m_V, A);
			dirty_view = true;
		}
		break;
		case chash("m_invV"):
		{
			store_Float3x4(cpu_view.m_invV, A);
			dirty_view = true;
		}
		break;
		case chash("m_P"):
		{
			store_Float4x4(cpu_view.m_P, A);
			dirty_view = true;
		}
		break;
		case chash("m_VP"):
		{
			store_Float4x4(cpu_view.m_VP, A);
			dirty_view = true;
		}
		break;
		case chash("m_invP"):
		{
			store_Float4x4(cpu_view.m_invP, A);
			dirty_view = true;
		}
		break;
		case chash("m_invP_hud"):
		{
			store_Float4x4(cpu_pass.m_invP_hud, A);
			dirty_pass = true;
		}
		break;
		case chash("m_P_hud"):
		{
			store_Float4x4(cpu_pass.m_P_hud, A);
			dirty_pass = true;
		}
		break;
		case chash("m_xform"):
		{
			store_Float4x4(cpu_pass.m_xform, A);
			dirty_pass = true;
		}
		break;
		case chash("m_xform_v"):
		{
			store_Float4x4(cpu_pass.m_xform_v, A);
			dirty_pass = true;
		}
		break;
		case chash("m_shadow"):
		{
			store_Float4x4(cpu_light.m_shadow, A);
			dirty_light = true;
		}
		break;
		case chash("m_sunmask"):
		{
			store_Float3x4(cpu_light.m_sunmask, A);
			dirty_light = true;
		}
		break;
		// previous-frame matrices drive motion vectors; without these TAA reprojects against
		// zero and moving/skinned meshes smear
		case chash("m_WVP_old"):
		{
			store_Float4x4(cpu_object.m_WVP_old, A);
			dirty_object = true;
		}
		break;
		case chash("m_VP_old"):
		{
			store_Float4x4(cpu_view.m_VP_old, A);
			dirty_view = true;
		}
		break;
		case chash("m_invVP_old"):
		{
			store_Float4x4(cpu_view.m_invVP_old, A);
			dirty_view = true;
		}
		break;
		case chash("m_texgen"):
		{
			store_Float4x4(cpu_pass.m_texgen, A);
			dirty_pass = true;
		}
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
		case chash("wind"):
			SetTreeWind(A);
			break;
		case chash("consts_old"):
			SetTreeConstsOld(A.x, A.y, A.z, A.w);
			break;
		case chash("wave_old"):
			SetTreeWaveOld(A);
			break;
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
		case chash("Ldynamic_color"):
		{
			cpu_light.Ldynamic_color.set(A.x, A.y, A.z, A.w);
			dirty_light = true;
		}
		break;
		case chash("Ldynamic_pos"):
		{
			cpu_light.Ldynamic_pos.set(A.x, A.y, A.z, A.w);
			dirty_light = true;
		}
		break;
		case chash("Ldynamic_dir"):
		{
			cpu_light.Ldynamic_dir.set(A.x, A.y, A.z, A.w);
			dirty_light = true;
		}
		break;
		case chash("c_brightness"):
		{
			cpu_frame.c_brightness.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("c_colormap"):
		{
			cpu_frame.c_colormap.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("color_params"):
		{
			cpu_frame.color_params.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("color_grading"):
		{
			cpu_frame.color_grading.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("fog_plane"):
		{
			cpu_frame.fog_plane.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("fog_params"):
		{
			cpu_frame.fog_params.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("fog_color"):
		{
			cpu_frame.fog_color.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("timers"):
		{
			cpu_frame.timers.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("eye_position"):
		{
			cpu_view.eye_position.set(A.x, A.y, A.z, A.w);
			dirty_view = true;
		}
		break;
		case chash("eye_direction"):
		{
			cpu_view.eye_direction.set(A.x, A.y, A.z, A.w);
			dirty_view = true;
		}
		break;
		case chash("eye_normal"):
		{
			cpu_view.eye_normal.set(A.x, A.y, A.z, A.w);
			dirty_view = true;
		}
		break;
		case chash("m_taa_jitter"):
		{
			cpu_view.m_taa_jitter.set(A.x, A.y, A.z, A.w);
			dirty_view = true;
		}
		break;
		case chash("L_sun_color"):
		{
			cpu_frame.L_sun_color.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("L_sun_dir_w"):
		{
			cpu_frame.L_sun_dir_w.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("L_sun_dir_e"):
		{
			cpu_frame.L_sun_dir_e.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("L_hemi_color"):
		{
			cpu_frame.L_hemi_color.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("L_ambient"):
		{
			cpu_frame.L_ambient.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("L_sky_color"):
		{
			cpu_frame.L_sky_color.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("water_intensity"):
		{
			cpu_frame.water_intensity.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("sun_shafts_intensity"):
		{
			cpu_frame.sun_shafts_intensity.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("rain_params"):
		{
			cpu_frame.rain_params.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("env_wind"):
		{
			cpu_frame.env_wind.set(A.x, A.y, A.z, A.w);
			dirty_frame = true;
		}
		break;
		case chash("mblur_params"):
		{
			cpu_pass.mblur_params.set(A.x, A.y, A.z, A.w);
			dirty_pass = true;
		}
		break;
		case chash("pos_decompression_params2"):
		{
			cpu_view.pos_decompression_params2.set(A.x, A.y, A.z, A.w);
			dirty_view = true;
		}
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
		{
			cpu_material.def_aref = A;
			dirty_material = true;
		}
		break;
		case chash("m_AlphaRef"):
		{
			cpu_material.m_AlphaRef = A;
			dirty_material = true;
		}
		break;
		case chash("triLOD"):
		{
			cpu_material.triLOD.set(A, A, A, A);
			dirty_material = true;
		}
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
		case chash("m_lmap"):
		{
			if (e < 2)
			{
				cpu_material.m_lmap[e].set(A.x, A.y, A.z, A.w);
				dirty_material = true;
			}
		}
		break;
		case chash("Ldynamic_color"):
		{
			if (e == 0)
			{
				cpu_light.Ldynamic_color.set(A.x, A.y, A.z, A.w);
				dirty_light = true;
			}
		}
		break;
		case chash("Ldynamic_pos"):
		{
			if (e == 0)
			{
				cpu_light.Ldynamic_pos.set(A.x, A.y, A.z, A.w);
				dirty_light = true;
			}
		}
		break;
		case chash("Ldynamic_dir"):
		{
			if (e == 0)
			{
				cpu_light.Ldynamic_dir.set(A.x, A.y, A.z, A.w);
				dirty_light = true;
			}
		}
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
			{
				store_Float4x4(&cpu_light.m_shadow_sun[e * 4], A);
				dirty_light = true;
			}
		}
		break;
		default:
			return false;
	}
	return true;
}

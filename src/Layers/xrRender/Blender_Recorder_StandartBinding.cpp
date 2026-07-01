#include "stdafx.h"


#include "ResourceManager.h"
#include "blenders/Blender_Recorder.h"
#include "blenders/Blender.h"

#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

#include "dxRenderDeviceRender.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/date_time.h"
#include "../../xrEngine/device.h"
#include "../../xrEngine/WristwatchTypes.h"
#include "../../xrEngine/WristwatchSettings.h"
// matrices
#define	BIND_DECLARE(xf)	\
class cl_xform_##xf	: public RHIShaderConstant::Setup {	virtual void setup (RHIShaderConstant* C) { RCache.xforms.set_c_##xf (C); } }; \
	static cl_xform_##xf	binder_##xf

BIND_DECLARE(invw);
BIND_DECLARE(invv);

BIND_DECLARE(w);
BIND_DECLARE(v);
BIND_DECLARE(p);
BIND_DECLARE(wv);
BIND_DECLARE(vp);
BIND_DECLARE(wvp);

#ifdef USE_DX11
BIND_DECLARE(w_old);
BIND_DECLARE(v_old);
BIND_DECLARE(p_old);
BIND_DECLARE(wv_old);
BIND_DECLARE(vp_old);
BIND_DECLARE(wvp_old);
#endif

#define DECLARE_TREE_BIND(c)	\
	class cl_tree_##c: public RHIShaderConstant::Setup	{virtual void setup(RHIShaderConstant* C) {RCache.tree.set_c_##c(C);} };	\
	static cl_tree_##c	tree_binder_##c

DECLARE_TREE_BIND(m_xform_v);
DECLARE_TREE_BIND(m_xform);

DECLARE_TREE_BIND(consts);
DECLARE_TREE_BIND(wave);
DECLARE_TREE_BIND(wind);

#ifdef USE_DX11
DECLARE_TREE_BIND(consts_old);
DECLARE_TREE_BIND(wave_old);
DECLARE_TREE_BIND(wind_old);
#endif

DECLARE_TREE_BIND(c_scale);
DECLARE_TREE_BIND(c_bias);
DECLARE_TREE_BIND(c_sun);

class cl_lit_color : public RHIShaderConstant::Setup {
	virtual void setup(RHIShaderConstant* C) { RCache.hemi.set_c_lit_color(C); }
};

static cl_lit_color binder_lit_color;

class cl_lit_dir : public RHIShaderConstant::Setup {
	virtual void setup(RHIShaderConstant* C) { RCache.hemi.set_c_lit_dir(C); }
};

static cl_lit_dir binder_lit_dir;

class cl_hemi_cube_pos_faces: public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C) {RCache.hemi.set_c_pos_faces(C);}
};

static cl_hemi_cube_pos_faces binder_hemi_cube_pos_faces;

class cl_hemi_cube_neg_faces: public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C) {RCache.hemi.set_c_neg_faces(C);}
};

static cl_hemi_cube_neg_faces binder_hemi_cube_neg_faces;

class cl_material: public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C) {RCache.hemi.set_c_material(C);}
};

static cl_material binder_material;

class cl_texgen : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		Fmatrix mTexgen;

#ifdef USE_DX11
		Fmatrix			mTexelAdjust		= 
		{
			0.5f,				0.0f,				0.0f,			0.0f,
			0.0f,				-0.5f,				0.0f,			0.0f,
			0.0f,				0.0f,				1.0f,			0.0f,
			0.5f,				0.5f,				0.0f,			1.0f
		};
#else //USE_DX11
		float	_w						= float(RCache.get_width());
		float	_h						= float(RCache.get_height());
		float	o_w						= (.5f / _w);
		float	o_h						= (.5f / _h);
		Fmatrix			mTexelAdjust		= 
		{
			0.5f,				0.0f,				0.0f,			0.0f,
			0.0f,				-0.5f,				0.0f,			0.0f,
			0.0f,				0.0f,				1.0f,			0.0f,
			0.5f + o_w,			0.5f + o_h,			0.0f,			1.0f
		};
#endif

		mTexgen.mul	(mTexelAdjust,RCache.xforms.m_wvp);

		RCache.set_c( C, mTexgen);
	}
};
static cl_texgen		binder_texgen;

class cl_VPtexgen : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		Fmatrix mTexgen;

#ifdef USE_DX11
		Fmatrix			mTexelAdjust		= 
		{
			0.5f,				0.0f,				0.0f,			0.0f,
			0.0f,				-0.5f,				0.0f,			0.0f,
			0.0f,				0.0f,				1.0f,			0.0f,
			0.5f,				0.5f,				0.0f,			1.0f
		};
#else //USE_DX11
		float	_w						= float(RCache.get_width());
		float	_h						= float(RCache.get_height());
		float	o_w						= (.5f / _w);
		float	o_h						= (.5f / _h);
		Fmatrix			mTexelAdjust		= 
		{
			0.5f,				0.0f,				0.0f,			0.0f,
			0.0f,				-0.5f,				0.0f,			0.0f,
			0.0f,				0.0f,				1.0f,			0.0f,
			0.5f + o_w,			0.5f + o_h,			0.0f,			1.0f
		};
#endif

		mTexgen.mul	(mTexelAdjust,RCache.xforms.m_vp);

		RCache.set_c( C, mTexgen);
	}
};
static cl_VPtexgen		binder_VPtexgen;

// fog
#if 1 //ndef _EDITOR
class cl_fog_plane	: public RHIShaderConstant::Setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(RHIShaderConstant* C)
	{
#ifdef _EDITOR
		if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv) {
			RCache.set_c(C, 0, 0, 0.0f, 0.0f);
			return;
		}
#endif
		if (marker!=Device.dwFrame)
		{
			// Plane
			Fvector4		plane;
			Fmatrix&	M	= Device.mFullTransform;
			plane.x			= -(M._14 + M._13);
			plane.y			= -(M._24 + M._23);
			plane.z			= -(M._34 + M._33);
			plane.w			= -(M._44 + M._43);
			float denom		= -1.0f / _sqrt(_sqr(plane.x)+_sqr(plane.y)+_sqr(plane.z));
			plane.mul		(denom);

			// Near/Far
			float A			= g_pGamePersistent->Environment().CurrentEnv->fog_near;
			float B			= 1/(g_pGamePersistent->Environment().CurrentEnv->fog_far - A);
			result.set		(-plane.x*B, -plane.y*B, -plane.z*B, 1 - (plane.w-A)*B	);								// view-plane
		}
		RCache.set_c	(C,result);
	}
};
static cl_fog_plane		binder_fog_plane;

// fog-params
class cl_fog_params	: public RHIShaderConstant::Setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(RHIShaderConstant* C)
	{
#ifdef _EDITOR
		if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv) {
			RCache.set_c(C, 0, 0, 0.0f, 0.0f);
			return;
		}
#endif
		if (marker!=Device.dwFrame)
		{
			// Near/Far
			float	n		= g_pGamePersistent->Environment().CurrentEnv->fog_near;
			float	f		= g_pGamePersistent->Environment().CurrentEnv->fog_far;
			float	r		= 1/(f-n);
			result.set		(-n*r, n, f, r);
		}
		RCache.set_c	(C,result);
	}
};	static cl_fog_params	binder_fog_params;

// fog-color
class cl_fog_color	: public RHIShaderConstant::Setup {
	u32			marker;
	Fvector4	result;
	virtual void setup	(RHIShaderConstant* C)	{
#ifdef _EDITOR
		if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv) {
			RCache.set_c(C, 0, 0, 0.0f, 0.0f);
			return;
		}
#endif
		if (marker!=Device.dwFrame)	{
			CEnvDescriptor&	desc	= *g_pGamePersistent->Environment().CurrentEnv;
#if RENDER == R_R1
			result.set(desc.fog_color.x * ps_r1_fog_luminance, desc.fog_color.y * ps_r1_fog_luminance, desc.fog_color.z * ps_r1_fog_luminance, 0);
#else
			result.set(desc.fog_color.x, desc.fog_color.y, desc.fog_color.z, 0);
#endif // RENDER==R_R1
		}
		RCache.set_c	(C,result);
	}
};	static cl_fog_color		binder_fog_color;
#endif

// times
class cl_times : public RHIShaderConstant::Setup 
{
	virtual void setup(RHIShaderConstant* C)
	{
		float t = RDEVICE.fTimeGlobal;
		RCache.set_c(C, t, t - RDEVICE.fTimeDelta, t * 0.1f, std::sin(t));
	}
};

static cl_times binder_times;

// eye-params
class cl_eye_P		: public RHIShaderConstant::Setup {
	virtual void setup(RHIShaderConstant* C)
	{
		Fvector&		V	= RDEVICE.vCameraPosition;
		RCache.set_c	(C,V.x,V.y,V.z,1);
	}
};
static cl_eye_P		binder_eye_P;

// eye-params
class cl_eye_D		: public RHIShaderConstant::Setup {
	virtual void setup(RHIShaderConstant* C)
	{
		Fvector&		V	= RDEVICE.vCameraDirection;
		RCache.set_c	(C,V.x,V.y,V.z,0);
	}
};
static cl_eye_D		binder_eye_D;

// eye-params
class cl_eye_N		: public RHIShaderConstant::Setup {
	virtual void setup(RHIShaderConstant* C)
	{
		Fvector&		V	= RDEVICE.vCameraTop;
		RCache.set_c	(C,V.x,V.y,V.z,0);
	}
};
static cl_eye_N		binder_eye_N;

// eye-params
class cl_hud_project : public RHIShaderConstant::Setup {
	virtual void setup(RHIShaderConstant* C)
	{
		RCache.set_c (C, Device.mProject_hud);
	}
};
static cl_hud_project binder_hud_project;

#if 1 //ndef _EDITOR
// TAA Jiter
class cl_taa_jitter : public RHIShaderConstant::Setup {
	virtual void setup(RHIShaderConstant* C) {
		Fvector& V = ps_r_taa_jitter;
		RCache.set_c(C, V.x, V.y, V.z, float(RDEVICE.dwFrame));
	}
};
static cl_taa_jitter binder_taa_jitter;
#endif

#if 1 //ndef _EDITOR
// D-Light0
class cl_sun0_color : public RHIShaderConstant::Setup {
	u32 marker;
	Fvector4 result;
	virtual void setup(RHIShaderConstant* C) {
#ifdef _EDITOR
		if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv) {
			RCache.set_c(C, 0, 0, 0.0f, 0.0f);
			return;
		}
#endif
		if (marker != Device.dwFrame) {
			CEnvDescriptor& desc = *g_pGamePersistent->Environment().CurrentEnv;
#if defined(_EDITOR) || RENDER != R_R1
			result.set(desc.sun_color.x * ps_r2_sun_lumscale, desc.sun_color.y * ps_r2_sun_lumscale, desc.sun_color.z * ps_r2_sun_lumscale, 0);
#else
			result.set(desc.sun_color.x, desc.sun_color.y, desc.sun_color.z, 0);
#endif
		}
		RCache.set_c(C, result);
	}
};	static cl_sun0_color binder_sun0_color;

class cl_sun0_dir_w : public RHIShaderConstant::Setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(RHIShaderConstant* C) {
#ifdef _EDITOR
		if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv) {
			RCache.set_c(C, 0, 0, 0.0f, 0.0f);
			return;
		}
#endif
		if (marker != Device.dwFrame) {
			CEnvDescriptor& desc = *g_pGamePersistent->Environment().CurrentEnv;
			result.set(desc.sun_dir.x, desc.sun_dir.y, desc.sun_dir.z, 0);
		}
		RCache.set_c(C, result);
	}
};	static cl_sun0_dir_w binder_sun0_dir_w;

class cl_sun0_dir_e : public RHIShaderConstant::Setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(RHIShaderConstant* C) {
#ifdef _EDITOR
		if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv) {
			RCache.set_c(C, 0, 0, 0.0f, 0.0f);
			return;
		}
#endif
		if (marker != Device.dwFrame) {
			Fvector D;
			CEnvDescriptor& desc = *g_pGamePersistent->Environment().CurrentEnv;
			Device.mView.transform_dir(D, desc.sun_dir);
			D.normalize();
			result.set(D.x, D.y, D.z, 0);
		}
		RCache.set_c(C, result);
	}
};	static cl_sun0_dir_e binder_sun0_dir_e;

class cl_amb_color : public RHIShaderConstant::Setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(RHIShaderConstant* C) {
#ifdef _EDITOR
		if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv) {
			RCache.set_c(C, 0, 0, 0.0f, 0.0f);
			return;
		}
#endif
		if (marker != Device.dwFrame) {
			CEnvDescriptorMixer& desc = *g_pGamePersistent->Environment().CurrentEnv;

#if defined(_EDITOR) || RENDER != R_R1
			result.set(desc.ambient.x * ps_r2_sun_lumscale_amb * 2.0f,
				desc.ambient.y * ps_r2_sun_lumscale_amb * 2.0f, desc.ambient.z * ps_r2_sun_lumscale_amb * 2.0f, desc.weight);
#else
			result.set(desc.ambient.x, desc.ambient.y, desc.ambient.z, desc.weight);
#endif
		}
		RCache.set_c(C, result);
	}
};	static cl_amb_color binder_amb_color;

class cl_hemi_color : public RHIShaderConstant::Setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(RHIShaderConstant* C) {
#ifdef _EDITOR
		if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv) {
			RCache.set_c(C, 0, 0, 0.0f, 0.0f);
			return;
		}
#endif
		if (marker != Device.dwFrame) 
		{
			CEnvDescriptorMixer& desc = *g_pGamePersistent->Environment().CurrentEnv;
#if defined(_EDITOR) || RENDER != R_R1
			if (desc.old_style)
			{
				result.set(desc.sky_color.x * ps_r2_sun_lumscale_hemi * 4.0f,
					desc.sky_color.y * ps_r2_sun_lumscale_hemi * 4.0f, desc.sky_color.z * ps_r2_sun_lumscale_hemi * 4.0f, desc.weight);
			}
			else
			{
				result.set(desc.hemi_color.x * ps_r2_sun_lumscale_hemi * 4.0f,
					desc.hemi_color.y * ps_r2_sun_lumscale_hemi * 4.0f, desc.hemi_color.z * ps_r2_sun_lumscale_hemi * 4.0f, desc.weight);
			}
#else
			result.set(desc.hemi_color);
#endif
		}

		RCache.set_c(C, result);
	}
}; static cl_hemi_color binder_hemi_color;
#endif

class cl_sky_color : public RHIShaderConstant::Setup {
	u32 marker;
	Fvector4 result;
	virtual void setup(RHIShaderConstant* C) {
#ifdef _EDITOR
		if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv) {
			RCache.set_c(C, 0, 0, 0.0f, 0.0f);
			return;
		}
#endif
		if (marker != Device.dwFrame) {
			CEnvDescriptorMixer& desc = *g_pGamePersistent->Environment().CurrentEnv;
#if defined(_EDITOR) || RENDER != R_R1
			result.set(desc.sky_color.x * ps_r2_sun_lumscale_sky, 
				desc.sky_color.y * ps_r2_sun_lumscale_sky, desc.sky_color.z * ps_r2_sun_lumscale_sky, desc.sky_rotation);
#else
			result.set(desc.sky_color.x, desc.sky_color.y, desc.sky_color.z, desc.sky_rotation);
#endif
		}
		RCache.set_c(C, result);
	}
}; static cl_sky_color binder_sky_color;

static class cl_screen_res : public RHIShaderConstant::Setup		
{	
	virtual void setup	(RHIShaderConstant* C)
	{
		RCache.set_c(C, RCache.get_target_width(), RCache.get_target_height(), 1.0f / RCache.get_target_width(), 1.0f / RCache.get_target_height());
	}
}	binder_screen_res;

static class cl_scaled_screen_res : public RHIShaderConstant::Setup 
{
	virtual void setup(RHIShaderConstant* C) 
	{
		RCache.set_c(C, RCache.get_width(), RCache.get_height(), 1.0f / RCache.get_width(), 1.0f / RCache.get_height());
	}
}	binder_scaled_screen_res;

static class cl_target_screen_res : public RHIShaderConstant::Setup 
{
	virtual void setup(RHIShaderConstant* C) 
	{
		RCache.set_c(C, (float)::Render->getTarget()->get_width(), (float)::Render->getTarget()->get_height(),
			1.0f / (float)::Render->getTarget()->get_width(), 1.0f / (float)::Render->getTarget()->get_height());
	}
}	binder_target_screen_res;

static class cl_screen_scale : public RHIShaderConstant::Setup 
{
	virtual void setup(RHIShaderConstant* C) 
	{
		RCache.set_c(C, GRHI->DevicePtr->RenderScale, 0.0f, 0.0f, 0.0f);
	}
} binder_screen_scale;

static class cl_def_aref : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C) override 
	{
#ifdef _EDITOR
		float def_aref_cmd = 100 / 255.0f;
#else
		float def_aref_cmd = ps_r2_def_aref_quality / 255.0f;
#endif
	#ifdef USE_DX11
		RCache.set_c(C, def_aref_cmd);
	#else
		RCache.set_c(C, def_aref_cmd, 0.0f, 0.0f, 0.0f);
	#endif
	}
} binder_def_aref;

static class cl_rain_params : public RHIShaderConstant::Setup {
	u32 marker;
	Fvector4 result;

	virtual void setup(RHIShaderConstant* C)
	{
#ifdef _EDITOR
		if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv) {
			RCache.set_c(C, 0, 0, 0.0f, 0.0f);
			return;
		}
#endif
		float rainDensity = g_pGamePersistent->Environment().CurrentEnv->rain_density;
		float rainWetness = g_pGamePersistent->Environment().wetness_factor;
		RCache.set_c(C, rainDensity, rainWetness, 0.0f, (float)g_pGameLevel->UseSnowmask);
	}
} binder_rain_params;

static class cl_inv_v : public RHIShaderConstant::Setup
{
	u32	marker;
	Fmatrix	result;

	virtual void setup(RHIShaderConstant* C)
	{
		result.invert(Device.mView);

		RCache.set_c(C, result);
	}
} binder_inv_v;

static class cl_env_wind : public RHIShaderConstant::Setup
{
	u32	marker;
	Fmatrix	result;

	virtual void setup(RHIShaderConstant* C)
	{
#ifdef _EDITOR
		if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv) {
			RCache.set_c(C, 0, 0, 0.0f, 0.0f);
			return;
		}
#endif
		const Fvector& WindDir = g_pGamePersistent->Environment().wind_blast_direction;
		RCache.set_c(C, WindDir.x, WindDir.y, WindDir.z, g_pGamePersistent->Environment().wind_strength_factor);
	}
} binder_wind;

static class cl_m_hud_params : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C) {
		RCache.set_c(C, RDEVICE.hudViewportData.isRenderProcess, RDEVICE.hudViewportData.isRenderActive, 0.0f, RDEVICE.hudViewportData.renderZoomRotateFactor);
	}
}    binder_m_hud_params;

static class cl_m_zoom_deviation : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C) {
		RCache.set_c(C, 0.0f, 0.0f, RDEVICE.hudViewportData.renderScopeBrightnessValue, RDEVICE.hudViewportData.renderScopeBrightnessJitterValue);
	}
} binder_m_zoom_deviation;

static class cl_affects : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		float decr = 0.0f;

		if (RDEVICE.hudViewportData.IsElectronicsProblemsDecreasing)
			decr = 1.0f;

		RCache.set_c(C, RDEVICE.hudViewportData.CurrentElectronicsProblemsCnt/10.0f, ::Random.randF(0.0f, 1.0f), RDEVICE.hudViewportData.TargetElectronicsProblemsCnt/10.0f, decr);
	}
} binder_affects;

static class cl_actor_params : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		RCache.set_c(C, RDEVICE.hudViewportData.ActorHealth, RDEVICE.hudViewportData.ActorOutfitCondition, RDEVICE.hudViewportData.ActorWeaponCondition, RDEVICE.hudViewportData.ActorWeaponLoading);
	}
} binder_actor_states;

static class cl_m_timearrow : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
		split_time(g_pGameLevel->GetGameTime(), year, month, day, hours, mins, secs, milisecs);

		float s_f = secs / 60.f;
		float s_angle = PI_MUL_2 * s_f;

		float m_f = (s_f + float(mins)) / 60.f;
		float m_angle = PI_MUL_2 * m_f;

		float h_f = (m_f + float(hours)) / 12.f;
		float h_angle = PI_MUL_2 * h_f;

		RCache.set_c(C, sin(h_angle), cos(h_angle), sin(m_angle), cos(m_angle));
	}
} binder_m_timearrow;

static class cl_m_timearrow2 : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
		split_time(g_pGameLevel->GetGameTime(), year, month, day, hours, mins, secs, milisecs);

		float s_f = secs / 60.f;
		float s_angle = PI_MUL_2 * s_f;

		float h, p;
		RDEVICE.vCameraDirection.getHP(h, p);

		RCache.set_c(C, sin(s_angle), cos(s_angle), sin(h), cos(h));
	}
} binder_m_timearrow2;

static class cl_digiclock : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
		split_time(g_pGameLevel->GetGameTime(), year, month, day, hours, mins, secs, milisecs);

		float hh = (hours / 10) / 10.0f;
		float hl = (hours % 10) / 10.0f;
		float mh = (month / 10) / 10.0f;
		float ml = (month % 10) / 10.0f;

		RCache.set_c(C, hh, hl, mh, ml);
	}
} binder_digiclock;

static void SetWristwatchFontGlyph(RHIShaderConstant* C, const Fvector4& glyph, bool fontReady)
{
	if (!fontReady)
	{
		RCache.set_c(C, 0.0f, 0.0f, 0.0f, 0.0f);
		return;
	}

	RCache.set_c(C, glyph.x, glyph.y, glyph.z, glyph.w);
}

static class cl_m_wristwatch_time : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		const auto& wristwatch = RDEVICE.hudViewportData.wristwatch;
		if (!wristwatch.isActive)
		{
			RCache.set_c(C, 0.0f, 1.0f, 0.0f, 1.0f);
			return;
		}

		if (!wristwatch.showAnalogHands && wristwatch.displayType == static_cast<u8>(EWristwatchDisplayType::Digital))
		{
			RCache.set_c(C, wristwatch.lcdLayout.x, wristwatch.lcdLayout.y, wristwatch.lcdLayout.z, wristwatch.lcdLayout.w);
			return;
		}

		u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
		split_time(wristwatch.displayGameTime, year, month, day, hours, mins, secs, milisecs);

		const float sF = secs / 60.f;
		const float mF = (sF + static_cast<float>(mins)) / 60.f;
		const float hF = (mF + static_cast<float>(hours)) / 12.f;

		RCache.set_c(
			C,
			sin(PI_MUL_2 * hF),
			cos(PI_MUL_2 * hF),
			sin(PI_MUL_2 * mF),
			cos(PI_MUL_2 * mF));
	}
} binder_m_wristwatch_time;

static class cl_m_wristwatch_time2 : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		const auto& wristwatch = RDEVICE.hudViewportData.wristwatch;
		if (!wristwatch.isActive)
		{
			RCache.set_c(C, 0.0f, 1.0f, 0.0f, 0.0f);
			return;
		}

		u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
		split_time(wristwatch.displayGameTime, year, month, day, hours, mins, secs, milisecs);

		const float sF = secs / 60.f;
		const float colonBlink = (!wristwatch.showAnalogHands && wristwatch.showLcd && (secs & 1) == 0) ? 1.0f : 0.0f;
		RCache.set_c(
			C,
			sin(PI_MUL_2 * sF),
			cos(PI_MUL_2 * sF),
			wristwatch.showAnalogHands ? 1.0f : colonBlink,
			wristwatch.showLcd ? 1.0f : 0.0f);
	}
} binder_m_wristwatch_time2;

static class cl_m_wristwatch_debug : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		const auto& settings = GetWristwatchRuntimeSettings();
		RCache.set_c(
			C,
			static_cast<float>(settings.debugLcdPass),
			0.0f,
			0.0f,
			0.0f);
	}
} binder_m_wristwatch_debug;

static class cl_m_wristwatch_lcd : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		const auto& wristwatch = RDEVICE.hudViewportData.wristwatch;
		if (!wristwatch.isActive || !wristwatch.showLcd)
		{
			RCache.set_c(C, 0.0f, 0.0f, 0.0f, 0.0f);
			return;
		}

		RCache.set_c(C, wristwatch.lcdDigits.x, wristwatch.lcdDigits.y, wristwatch.lcdDigits.z, wristwatch.lcdDigits.w);
	}
} binder_m_wristwatch_lcd;

static class cl_m_wristwatch_fx : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		const auto& wristwatch = RDEVICE.hudViewportData.wristwatch;
		if (!wristwatch.isActive)
		{
			RCache.set_c(C, 0.0f, 0.0f, 0.0f, 0.0f);
			return;
		}

		RCache.set_c(
			C,
			static_cast<float>(wristwatch.displayType),
			static_cast<float>(wristwatch.surgeMode),
			wristwatch.motionIconLuminosity,
			wristwatch.glitchStrength);
	}
} binder_m_wristwatch_fx;

static class cl_m_wristwatch_font_d0 : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		const auto& wristwatch = RDEVICE.hudViewportData.wristwatch;
		SetWristwatchFontGlyph(C, wristwatch.fontGlyph0, wristwatch.fontReady);
	}
} binder_m_wristwatch_font_d0;

static class cl_m_wristwatch_font_d1 : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		const auto& wristwatch = RDEVICE.hudViewportData.wristwatch;
		SetWristwatchFontGlyph(C, wristwatch.fontGlyph1, wristwatch.fontReady);
	}
} binder_m_wristwatch_font_d1;

static class cl_m_wristwatch_font_d2 : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		const auto& wristwatch = RDEVICE.hudViewportData.wristwatch;
		SetWristwatchFontGlyph(C, wristwatch.fontGlyph2, wristwatch.fontReady);
	}
} binder_m_wristwatch_font_d2;

static class cl_m_wristwatch_font_d3 : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		const auto& wristwatch = RDEVICE.hudViewportData.wristwatch;
		SetWristwatchFontGlyph(C, wristwatch.fontGlyph3, wristwatch.fontReady);
	}
} binder_m_wristwatch_font_d3;

static class cl_m_wristwatch_font_colon : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		const auto& wristwatch = RDEVICE.hudViewportData.wristwatch;
		SetWristwatchFontGlyph(C, wristwatch.fontGlyphColon, wristwatch.fontReady);
	}
} binder_m_wristwatch_font_colon;

static class cl_m_wristwatch_font_eight : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		const auto& wristwatch = RDEVICE.hudViewportData.wristwatch;
		SetWristwatchFontGlyph(C, wristwatch.fontGlyphEight, wristwatch.fontReady);
	}
} binder_m_wristwatch_font_eight;

extern ENGINE_API Fcolor nvg_color;
// night-vision color
class cl_NVG_Color : public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		RCache.set_c(C, nvg_color.r, nvg_color.g, nvg_color.b, nvg_color.a);
	}
};
static cl_NVG_Color binder_NVG_Color;

// Standart constant-binding
void	CBlender_Compile::SetMapping()
{
	// matrices
	r_Constant("m_W", &binder_w);
	r_Constant("m_invW", &binder_invw);
	r_Constant("m_V", &binder_v);
	r_Constant("m_P", &binder_p);
	r_Constant("m_WV", &binder_wv);
	r_Constant("m_VP", &binder_vp);
	r_Constant("m_WVP", &binder_wvp);
	r_Constant("m_invV", &binder_invv);

	r_Constant("m_P_hud", &binder_hud_project);

#ifdef USE_DX11
	r_Constant("m_W_old", &binder_w_old);
	r_Constant("m_V_old", &binder_v_old);
	r_Constant("m_P_old", &binder_p_old);
	r_Constant("m_WV_old", &binder_wv_old);
	r_Constant("m_VP_old", &binder_vp_old);
	r_Constant("m_WVP_old", &binder_wvp_old);
#endif

	r_Constant("m_xform_v", &tree_binder_m_xform_v);
	r_Constant("m_xform", &tree_binder_m_xform);

	r_Constant("consts", &tree_binder_consts);
	r_Constant("wave", &tree_binder_wave);
	r_Constant("wind", &tree_binder_wind);
	r_Constant("env_wind", &binder_wind);

#ifdef USE_DX11
	r_Constant("consts_old", &tree_binder_consts_old);
	r_Constant("wave_old", &tree_binder_wave_old);
	r_Constant("wind_old", &tree_binder_wind_old);
#endif

	r_Constant("c_scale", &tree_binder_c_scale);
	r_Constant("c_bias", &tree_binder_c_bias);
	r_Constant("c_sun", &tree_binder_c_sun);

	//hemi cube
	r_Constant("L_material", &binder_material);
	r_Constant("hemi_cube_pos_faces", &binder_hemi_cube_pos_faces);
	r_Constant("hemi_cube_neg_faces", &binder_hemi_cube_neg_faces);

	r_Constant("L_model_light_color", &binder_lit_color);
	r_Constant("L_model_light_dir", &binder_lit_dir);

	r_Constant("m_texgen", &binder_texgen);
	r_Constant("mVPTexgen", &binder_VPtexgen);

	// fog-params
	r_Constant("fog_plane", &binder_fog_plane);
	r_Constant("fog_params", &binder_fog_params);
	r_Constant("fog_color", &binder_fog_color);

	r_Constant("timers", &binder_times);

	// eye-params
	r_Constant("eye_position", &binder_eye_P);
	r_Constant("eye_direction", &binder_eye_D);
	r_Constant("eye_normal", &binder_eye_N);

	r_Constant("L_sun_color", &binder_sun0_color);
	r_Constant("L_sun_dir_w", &binder_sun0_dir_w);
	r_Constant("L_sun_dir_e", &binder_sun0_dir_e);

	r_Constant("m_taa_jitter", &binder_taa_jitter);

	r_Constant("L_sky_color", &binder_sky_color);

	r_Constant("L_hemi_color", &binder_hemi_color);
	r_Constant("L_ambient", &binder_amb_color);

	r_Constant("screen_res", &binder_screen_res);
	r_Constant("def_aref", &binder_def_aref);

	r_Constant("scaled_screen_res", &binder_scaled_screen_res);
	r_Constant("target_screen_res", &binder_target_screen_res);

	r_Constant("screen_scale", &binder_screen_scale);

	r_Constant("rain_params", &binder_rain_params);

	//LVutner: Gunslinger...
	r_Constant("m_hud_params", &binder_m_hud_params);
	r_Constant("m_zoom_deviation", &binder_m_zoom_deviation);
	r_Constant("m_affects", &binder_affects);
	r_Constant("m_actor_params", &binder_actor_states);
	r_Constant("m_timearrow", &binder_m_timearrow);
	r_Constant("m_timearrow2", &binder_m_timearrow2);
	r_Constant("m_digiclock", &binder_digiclock);
	r_Constant("m_wristwatch_time", &binder_m_wristwatch_time);
	r_Constant("m_wristwatch_time2", &binder_m_wristwatch_time2);
	r_Constant("m_wristwatch_debug", &binder_m_wristwatch_debug);
	r_Constant("m_wristwatch_lcd", &binder_m_wristwatch_lcd);
	r_Constant("m_wristwatch_fx", &binder_m_wristwatch_fx);
	r_Constant("m_wristwatch_font_d0", &binder_m_wristwatch_font_d0);
	r_Constant("m_wristwatch_font_d1", &binder_m_wristwatch_font_d1);
	r_Constant("m_wristwatch_font_d2", &binder_m_wristwatch_font_d2);
	r_Constant("m_wristwatch_font_d3", &binder_m_wristwatch_font_d3);
	r_Constant("m_wristwatch_font_colon", &binder_m_wristwatch_font_colon);
	r_Constant("m_wristwatch_font_eight", &binder_m_wristwatch_font_eight);
	r_Constant("nvg_color", &binder_NVG_Color);

	if (detail_scaler)
	{
		r_Constant("dt_params", detail_scaler);
	}

	for (const auto& [Name, Setup] : DEV->v_constant_setup)
	{
		r_Constant(*Name, Setup);
	}
}

#include "stdafx.h"
#pragma hdrstop

#include "ResourceManager.h"
#include "blenders\Blender_Recorder.h"
#include "blenders\Blender.h"

#include "../../xrEngine/igame_persistent.h"
#include "../../xrEngine/environment.h"

#include "dxRenderDeviceRender.h"

// matrices
#define	BIND_DECLARE(xf)	\
class cl_xform_##xf	: public R_constant_setup {	virtual void setup (R_constant* C) { RCache.xforms.set_c_##xf (C); } }; \
	static cl_xform_##xf	binder_##xf

BIND_DECLARE(invw);

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
	class cl_tree_##c: public R_constant_setup	{virtual void setup(R_constant* C) {RCache.tree.set_c_##c(C);} };	\
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

class cl_hemi_cube_pos_faces: public R_constant_setup
{
	virtual void setup(R_constant* C) {RCache.hemi.set_c_pos_faces(C);}
};

static cl_hemi_cube_pos_faces binder_hemi_cube_pos_faces;

class cl_hemi_cube_neg_faces: public R_constant_setup
{
	virtual void setup(R_constant* C) {RCache.hemi.set_c_neg_faces(C);}
};

static cl_hemi_cube_neg_faces binder_hemi_cube_neg_faces;

class cl_material: public R_constant_setup
{
	virtual void setup(R_constant* C) {RCache.hemi.set_c_material(C);}
};

static cl_material binder_material;

class cl_texgen : public R_constant_setup
{
	virtual void setup(R_constant* C)
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

class cl_invV : public R_constant_setup
{
	virtual void setup(R_constant* C)
	{
		Fmatrix mInvV = Fmatrix().invert(RCache.xforms.m_v);

		RCache.set_c(C, mInvV);
	}
};
static cl_invV binder_invv;

class cl_VPtexgen : public R_constant_setup
{
	virtual void setup(R_constant* C)
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
#ifndef _EDITOR
class cl_fog_plane	: public R_constant_setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(R_constant* C)
	{
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
class cl_fog_params	: public R_constant_setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(R_constant* C)
	{
		if (marker!=Device.dwFrame)
		{
			// Near/Far
			float	n		= g_pGamePersistent->Environment().CurrentEnv->fog_near;
			float	f		= g_pGamePersistent->Environment().CurrentEnv->fog_far;
			float	r		= 1/(f-n);
			result.set		(-n*r, r, r, r);
		}
		RCache.set_c	(C,result);
	}
};	static cl_fog_params	binder_fog_params;

// fog-color
class cl_fog_color	: public R_constant_setup {
	u32			marker;
	Fvector4	result;
	virtual void setup	(R_constant* C)	{
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
class cl_times		: public R_constant_setup {
	virtual void setup(R_constant* C)
	{
		float 		t	= RDEVICE.fTimeGlobal;
		RCache.set_c	(C,t,t*10,t/10,_sin(t))	;
	}
};
static cl_times		binder_times;

// eye-params
class cl_eye_P		: public R_constant_setup {
	virtual void setup(R_constant* C)
	{
		Fvector&		V	= RDEVICE.vCameraPosition;
		RCache.set_c	(C,V.x,V.y,V.z,1);
	}
};
static cl_eye_P		binder_eye_P;

// eye-params
class cl_eye_D		: public R_constant_setup {
	virtual void setup(R_constant* C)
	{
		Fvector&		V	= RDEVICE.vCameraDirection;
		RCache.set_c	(C,V.x,V.y,V.z,0);
	}
};
static cl_eye_D		binder_eye_D;

// eye-params
class cl_eye_N		: public R_constant_setup {
	virtual void setup(R_constant* C)
	{
		Fvector&		V	= RDEVICE.vCameraTop;
		RCache.set_c	(C,V.x,V.y,V.z,0);
	}
};
static cl_eye_N		binder_eye_N;

// eye-params
class cl_hud_project : public R_constant_setup {
	virtual void setup(R_constant* C)
	{
		RCache.set_c (C, Device.mProject_hud);
	}
};
static cl_hud_project binder_hud_project;

#ifndef _EDITOR
// TAA Jiter
class cl_taa_jitter : public R_constant_setup {
	virtual void setup(R_constant* C) {
		Fvector& V = ps_r_taa_jitter;
		RCache.set_c(C, V.x, V.y, V.z, float(RDEVICE.dwFrame));
	}
};
static cl_taa_jitter binder_taa_jitter;
#endif

#ifndef _EDITOR
// D-Light0
class cl_sun0_color : public R_constant_setup {
	u32 marker;
	Fvector4 result;
	virtual void setup(R_constant* C) {
		if (marker != Device.dwFrame) {
			CEnvDescriptor& desc = *g_pGamePersistent->Environment().CurrentEnv;
#if RENDER != R_R1
			result.set(desc.sun_color.x * ps_r2_sun_lumscale, desc.sun_color.y * ps_r2_sun_lumscale, desc.sun_color.z * ps_r2_sun_lumscale, 0);
#else
			result.set(desc.sun_color.x, desc.sun_color.y, desc.sun_color.z, 0);
#endif
		}
		RCache.set_c(C, result);
	}
};	static cl_sun0_color binder_sun0_color;

class cl_sun0_dir_w : public R_constant_setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(R_constant* C) {
		if (marker != Device.dwFrame) {
			CEnvDescriptor& desc = *g_pGamePersistent->Environment().CurrentEnv;
			result.set(desc.sun_dir.x, desc.sun_dir.y, desc.sun_dir.z, 0);
		}
		RCache.set_c(C, result);
	}
};	static cl_sun0_dir_w binder_sun0_dir_w;

class cl_sun0_dir_e : public R_constant_setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(R_constant* C) {
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

class cl_amb_color : public R_constant_setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(R_constant* C) {
		if (marker != Device.dwFrame) {
			CEnvDescriptorMixer& desc = *g_pGamePersistent->Environment().CurrentEnv;

#if RENDER != R_R1
			result.set(desc.ambient.x * ps_r2_sun_lumscale_amb * 2.0f,
				desc.ambient.y * ps_r2_sun_lumscale_amb * 2.0f, desc.ambient.z * ps_r2_sun_lumscale_amb * 2.0f, desc.weight);
#else
			result.set(desc.ambient.x, desc.ambient.y, desc.ambient.z, desc.weight);
#endif
		}
		RCache.set_c(C, result);
	}
};	static cl_amb_color binder_amb_color;

class cl_hemi_color : public R_constant_setup {
	u32			marker;
	Fvector4	result;
	virtual void setup(R_constant* C) {
		if (marker != Device.dwFrame) {
			CEnvDescriptorMixer& desc = *g_pGamePersistent->Environment().CurrentEnv;
#if RENDER != R_R1
			result.set(desc.hemi_color.x * ps_r2_sun_lumscale_hemi * 4.0f,
				desc.hemi_color.y * ps_r2_sun_lumscale_hemi * 4.0f, desc.hemi_color.z * ps_r2_sun_lumscale_hemi * 4.0f, desc.weight);
#else
			result.set(desc.hemi_color);
#endif
		}
		RCache.set_c(C, result);
	}
}; static cl_hemi_color binder_hemi_color;
#endif

class cl_sky_color : public R_constant_setup {
	u32 marker;
	Fvector4 result;
	virtual void setup(R_constant* C) {
		if (marker != Device.dwFrame) {
			CEnvDescriptorMixer& desc = *g_pGamePersistent->Environment().CurrentEnv;
#if RENDER != R_R1
			result.set(desc.sky_color.x * ps_r2_sun_lumscale_sky, 
				desc.sky_color.y * ps_r2_sun_lumscale_sky, desc.sky_color.z * ps_r2_sun_lumscale_sky, desc.sky_rotation);
#else
			result.set(desc.sky_color.x, desc.sky_color.y, desc.sky_color.z, desc.sky_rotation);
#endif
		}
		RCache.set_c(C, result);
	}
}; static cl_sky_color binder_sky_color;

static class cl_screen_res : public R_constant_setup		
{	
	virtual void setup	(R_constant* C)
	{
		RCache.set_c(C, RCache.get_target_width(), RCache.get_target_height(), 1.0f / RCache.get_target_width(), 1.0f / RCache.get_target_height());
	}
}	binder_screen_res;

static class cl_scaled_screen_res : public R_constant_setup {
	virtual void setup(R_constant* C) {
		RCache.set_c(C, RCache.get_width(), RCache.get_height(), 1.0f / RCache.get_width(), 1.0f / RCache.get_height());
	}
}	binder_scaled_screen_res;

static class cl_target_screen_res : public R_constant_setup {
	virtual void setup(R_constant* C) {
		RCache.set_c(C, (float)::Render->getTarget()->get_width(), (float)::Render->getTarget()->get_height(),
			1.0f / (float)::Render->getTarget()->get_width(), 1.0f / (float)::Render->getTarget()->get_height());
	}
}	binder_target_screen_res;

static class cl_screen_scale : public R_constant_setup {
	virtual void setup(R_constant* C) {
		RCache.set_c(C, RDEVICE.RenderScale, 0.0f, 0.0f, 0.0f);
	}
} binder_screen_scale;

static class cl_def_aref : public R_constant_setup
{
	virtual void setup(R_constant* C) override {
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

static class cl_rain_params : public R_constant_setup {
	u32 marker;
	Fvector4 result;

	virtual void setup(R_constant* C)
	{
#ifndef _EDITOR
		float rainDensity = g_pGamePersistent->Environment().CurrentEnv->rain_density;
		float rainWetness = g_pGamePersistent->Environment().wetness_factor;
		RCache.set_c(C, rainDensity, rainWetness, 0.0f, 0.0f);
#else
		RCache.set_c(C, 0, 0, 0.0f, 0.0f);
#endif
	}
} binder_rain_params;

static class cl_inv_v : public R_constant_setup
{
	u32	marker;
	Fmatrix	result;

	virtual void setup(R_constant* C)
	{
		result.invert(Device.mView);

		RCache.set_c(C, result);
	}
} binder_inv_v;

// Standart constant-binding
void	CBlender_Compile::SetMapping	()
{
	// matrices
	r_Constant				("m_W",				&binder_w);
	r_Constant				("m_invW",			&binder_invw);
	r_Constant				("m_V",				&binder_v);
	r_Constant				("m_P",				&binder_p);
	r_Constant				("m_WV",			&binder_wv);
	r_Constant				("m_VP",			&binder_vp);
	r_Constant				("m_WVP",			&binder_wvp);
	r_Constant				("m_inv_V",			&binder_inv_v);

	r_Constant("m_P_hud", &binder_hud_project);

#ifdef USE_DX11
	r_Constant("m_W_old", &binder_w_old);
	r_Constant("m_V_old", &binder_v_old);
	r_Constant("m_P_old", &binder_p_old);
	r_Constant("m_WV_old", &binder_wv_old);
	r_Constant("m_VP_old", &binder_vp_old);
	r_Constant("m_WVP_old", &binder_wvp_old);
#endif
	
	r_Constant				("m_xform_v",		&tree_binder_m_xform_v);
	r_Constant				("m_xform",			&tree_binder_m_xform);

	r_Constant				("consts",			&tree_binder_consts);
	r_Constant				("wave",			&tree_binder_wave);
	r_Constant				("wind",			&tree_binder_wind);

#ifdef USE_DX11
	r_Constant				("consts_old",		&tree_binder_consts_old);
	r_Constant				("wave_old",  		&tree_binder_wave_old);
	r_Constant				("wind_old",  		&tree_binder_wind_old);
#endif

	r_Constant				("c_scale",			&tree_binder_c_scale);
	r_Constant				("c_bias",			&tree_binder_c_bias);
	r_Constant				("c_sun",			&tree_binder_c_sun);

	//hemi cube
	r_Constant				("L_material",			&binder_material);
	r_Constant				("hemi_cube_pos_faces",			&binder_hemi_cube_pos_faces);
	r_Constant				("hemi_cube_neg_faces",			&binder_hemi_cube_neg_faces);

	//	Igor	temp solution for the texgen functionality in the shader
	r_Constant				("m_invV",				&binder_invv);
	r_Constant				("m_texgen",			&binder_texgen);
	r_Constant				("mVPTexgen",			&binder_VPtexgen);

#ifndef _EDITOR
	// fog-params
	r_Constant				("fog_plane",		&binder_fog_plane);
	r_Constant				("fog_params",		&binder_fog_params);
	r_Constant				("fog_color",		&binder_fog_color);
#endif
	// time
	r_Constant				("timers",			&binder_times);

	// eye-params
	r_Constant				("eye_position",	&binder_eye_P);
	r_Constant				("eye_direction",	&binder_eye_D);
	r_Constant				("eye_normal",		&binder_eye_N);

#ifndef _EDITOR
	// global-lighting (env params)
	r_Constant				("L_sun_color",		&binder_sun0_color);
	r_Constant				("L_sun_dir_w",		&binder_sun0_dir_w);
	r_Constant				("L_sun_dir_e",		&binder_sun0_dir_e);
	
	r_Constant				("m_taa_jitter",	&binder_taa_jitter);

	r_Constant				("L_sky_color",		&binder_sky_color);

	r_Constant				("L_hemi_color",	&binder_hemi_color);
	r_Constant				("L_ambient",		&binder_amb_color);
#endif
	r_Constant				("screen_res",		&binder_screen_res);
	r_Constant				("def_aref",		&binder_def_aref);

	r_Constant("scaled_screen_res", &binder_scaled_screen_res);
	r_Constant("target_screen_res", &binder_target_screen_res);

	r_Constant("screen_scale", &binder_screen_scale);
	
	// Rain
	r_Constant				("rain_params",		&binder_rain_params);

	// detail
	//if (bDetail	&& detail_scaler)
	//	Igor: bDetail can be overridden by no_detail_texture option.
	//	But shader can be deatiled implicitly, so try to set this parameter
	//	anyway.
	if (detail_scaler)
		r_Constant			("dt_params",		detail_scaler);

	// other common
	for (u32 it=0; it<DEV->v_constant_setup.size(); it++)
	{
		std::pair<shared_str,R_constant_setup*>	cs	= DEV->v_constant_setup[it];
		r_Constant			(*cs.first,cs.second);
	}
}

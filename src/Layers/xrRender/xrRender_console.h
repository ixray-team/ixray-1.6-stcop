#ifndef xrRender_consoleH
#define xrRender_consoleH
#pragma once

// Common
extern ECORE_API	u32			ps_r_sun_shafts;	//=	0;
extern ECORE_API	xr_token	qsun_shafts_token[];

extern ECORE_API	u32			ps_r2_smapsize;
extern ECORE_API	xr_token	qsmapsize_token[];

extern ENGINE_API u32 ps_r_scale_mode;
extern ENGINE_API u32 ps_proxy_r_scale_mode;

extern ECORE_API	u32			ps_r_ssao_mode;
extern ECORE_API	xr_token	qssao_mode_token[];

extern ECORE_API	u32			ps_r_sun_quality;	//	=	0;
extern ECORE_API	xr_token	qsun_quality_token[];

extern ECORE_API	u32			ps_r2_aa_type;//	=	0;
extern ECORE_API	xr_token	aa_type_token[];

extern ECORE_API	u32			ps_screenshot_format;//	=	0;
extern ECORE_API	xr_token	screenshot_format_token[];

extern ECORE_API	int			ps_r__LightSleepFrames;
extern ECORE_API float ps_r__tf_Mipbias;
extern ECORE_API	float		ps_r__Detail_l_ambient;
extern ECORE_API	float		ps_r__Detail_l_aniso;
extern ECORE_API	float		ps_r__Detail_density;

extern ECORE_API	float		ps_r__Tree_SBC;		// scale bias correct

extern ECORE_API	float		ps_r__WallmarkTTL		;
extern ECORE_API	float		ps_r__WallmarkSHIFT		;
extern ECORE_API	float		ps_r__WallmarkSHIFT_V	;

extern ECORE_API	float		ps_r__GLOD_ssa_start;
extern ECORE_API	float		ps_r__GLOD_ssa_end	;
extern ECORE_API	float		ps_r__LOD			;
extern ECORE_API	float		ps_r__ssaDISCARD	;
extern ECORE_API	float		ps_r__ssaDONTSORT	;
extern ECORE_API	float		ps_r__ssaHZBvsTEX	;
extern ECORE_API	int			ps_r__tf_Anisotropic;

// R1
extern ECORE_API	float		ps_r1_ssaLOD_A;
extern ECORE_API	float		ps_r1_ssaLOD_B;
extern ECORE_API	float		ps_r1_lmodel_lerp;
extern ECORE_API	float		ps_r1_dlights_clip;
extern ECORE_API	float		ps_r1_pps_u;
extern ECORE_API	float		ps_r1_pps_v;

// R1-specific
extern ECORE_API	int			ps_r1_GlowsPerFrame;	// r1-only
extern ECORE_API	Flags32		ps_r1_flags;			// r1-only

extern ECORE_API	float		ps_r1_fog_luminance;	//1.f r1-only
extern ECORE_API	int			ps_r1_use_terrain_mask;

extern ECORE_API	Fvector3	ps_r_taa_jitter;
extern ECORE_API	Fvector3	ps_r_taa_jitter_scale;

enum
{
	R1FLAG_DLIGHTS		= (1 << 0),
	R1FLAG_TERRAIN_MASK	= (1 << 1),
};

extern ECORE_API Flags32		ps_r__common_flags;

// R2
extern ECORE_API	float		ps_r2_ssaLOD_A;
extern ECORE_API	float		ps_r2_ssaLOD_B;
extern ECORE_API	bool		ps_r2_particle_dt;
// R2-specific
extern ECORE_API Flags32		ps_r2_ls_flags;				// r2-only
extern ECORE_API Flags32		ps_r2_ls_flags_ext;
extern ECORE_API float			ps_r2_df_parallax_h;		// r2-only
extern ECORE_API float			ps_r2_df_parallax_range;	// r2-only
extern ECORE_API float			ps_r2_gmaterial;			// r2-only
extern ECORE_API float			ps_r2_tonemap_middlegray;	// r2-only
extern ECORE_API float			ps_r2_tonemap_adaptation;	// r2-only
extern ECORE_API float			ps_r2_tonemap_low_lum;		// r2-only
extern ECORE_API float			ps_r2_tonemap_amount;		// r2-only
extern ECORE_API float			ps_r2_ls_bloom_kernel_scale;// r2-only	// gauss
extern ECORE_API float			ps_r2_ls_bloom_kernel_g;	// r2-only	// gauss
extern ECORE_API float			ps_r2_ls_bloom_kernel_b;	// r2-only	// bilinear
extern ECORE_API float			ps_r2_ls_bloom_threshold;	// r2-only
extern ECORE_API float			ps_r2_ls_bloom_speed;		// r2-only
extern ECORE_API float			ps_r2_ls_depth_scale;		// 1.0f
extern ECORE_API float			ps_r2_ls_depth_bias;		// -0.0001f
extern ECORE_API float			ps_r2_ls_squality;			// 1.0f
extern ECORE_API float			ps_r2_sun_far;
extern ECORE_API float			ps_r2_sun_near;
extern ECORE_API float			ps_r2_sun_bias;			// 0.0001f
extern ECORE_API float			ps_r2_sun_depth_far_scale;	// 1.00001f
extern ECORE_API float			ps_r2_sun_depth_near_scale;	// 1.00001f
extern ECORE_API float			ps_r2_sun_lumscale;			// 0.5f
extern ECORE_API float			ps_r2_sun_lumscale_hemi;	// 1.0f
extern ECORE_API float			ps_r2_sun_lumscale_amb;		// 1.0f
extern ECORE_API float			ps_r2_sun_lumscale_sky;		// 1.0f
extern ECORE_API float			ps_r2_zfill;				// .1f

extern ECORE_API float			ps_r2_dhemi_sky_scale;		// 1.5f
extern ECORE_API float			ps_r2_dhemi_light_scale;	// 1.f
extern ECORE_API float			ps_r2_dhemi_light_flow;		// .1f
extern ECORE_API int			ps_r2_dhemi_count;			// 5
extern ECORE_API float			ps_r2_slight_fade;			// 1.f
extern ECORE_API float			ps_r4_vslr_distance;		// 1.f
extern ECORE_API int			ps_r2_wait_sleep;

//	x - min (0), y - focus (1.4), z - max (100)
extern ECORE_API Fvector3		ps_r2_dof;
extern ECORE_API float			ps_r2_dof_sky;				//	distance to sky
extern ECORE_API float			ps_r2_dof_kernel_size;		//	7.0f
extern ECORE_API float			ps_r2_def_aref_quality;

extern ECORE_API float			ps_r3_dyn_wet_surf_near;	// 10.0f
extern ECORE_API float			ps_r3_dyn_wet_surf_far;		// 30.0f
extern ECORE_API int			ps_r3_dyn_wet_surf_sm_res;	// 256

extern ECORE_API float			ps_r4_cas_sharpening;

// Test float exported to shaders for development
extern  float					ps_r__test_exp_to_shaders_1;
extern  float					ps_r__test_exp_to_shaders_2;
extern  float					ps_r__test_exp_to_shaders_3;
extern  float					ps_r__test_exp_to_shaders_4;

// papa_doenitz: these are now used by the new tonemapping/adaptation/bloom code.
extern ECORE_API bool		ps_r2_new_autoexposure;						// auto exposure (0 - disable, 1 - enable)
extern ECORE_API float		ps_r2_autoexposure_key;					// papa_doenitz - middlegray/key, default 0.18f, 0.148f for "unreal like"
extern ECORE_API float		ps_r2_autoexposure_min;					// exposure minimum, f-stop (for auto exposure only, make it higher if brightness is too low in bright areas)
extern ECORE_API float		ps_r2_autoexposure_max;					// exposure maximum, f-stop (for auto exposure only, make it lower if brightness is too high in lowlight areas)
extern ECORE_API float      ps_r2_autoexposure_bias;				// exposure bias, s-stop (for auto exposure only, make it higher if brightness is too low in general, or lower if brightness is too high in general)
extern ECORE_API float		ps_r2_autoexposure_speed;				// autoexposure adaptation speed (for auto exposure only, make it higher if brightness changes too slowly, or lower if brightness changes too quickly)
extern ECORE_API bool 		ps_r2_autoexposure_center_weight ;		// autoexposure center weighted (for auto exposure only, make it true if you want to give more weight to the center of the screen for auto exposure, or false to give equal weight to the whole screen)
extern ECORE_API float		ps_r2_autoexposure_min_weight;			// autoexposure minimum weight (for center weighted auto exposure)
extern ECORE_API float		ps_r2_autoexposure_gaussian;			// autoexposure gaussian factor (for center weighted auto exposure)
extern ECORE_API bool		ps_r2_autoexposure_soft_log;			// autoexposure soft log (for auto exposure only, make it true to use soft log for auto exposure, or false to use simple log2 auto exposure)
extern ECORE_API float		ps_r2_autoexposure_soft_log_k;			// autoexposure soft log acceptance in EV (for soft log)
extern ECORE_API float		ps_r2_autoexposure_soft_limiter;		// autoexposure soft limiter in EV (for soft log)
extern ECORE_API float		ps_r2_autoexposure_sensitivity;			// autoexposure sensitivity (for soft log)
extern ECORE_API float		ps_r2_bloom_amount;						// bloom amount, exposure independant (0.04f)
extern ECORE_API float		ps_r2_bloom_desaturation;				// bloom desaturation (0.1f)
extern ECORE_API float		ps_r2_bloom_tint_amount;				// bloom tint amount (0.1f)
extern ECORE_API Fvector3   ps_r2_bloom_tint_color;					// bloom tint color 
extern ECORE_API bool		ps_r2_new_bloom_tonemap;				// enable new bloom/tonemap code (0 - disable, 1 - enable)
extern ECORE_API float		ps_r2_tonemap_compression;				// tonemap compression, point where liear part ends (0.8 - 0.04)
extern ECORE_API float 		ps_r2_tonemap_desaturation;				// tonemap highlights desaturation (0.15)
extern ECORE_API bool		ps_r2_crossfeed;						// rbg crossfeed enable
extern ECORE_API float		ps_r2_tonemap_crossfeed;				// rbg crossfeed (optional, 0.01 - 0.5)
extern ECORE_API bool		ps_r2_vibrance;						// tonemap vibrance enable
extern ECORE_API float 		ps_r2_tonemap_vibrance;					// tonemap vibrance (optional, 0.0 - 1.0)
// new tonemapping/adaptation/bloom end

extern bool UseGasmak;
extern bool UseRainDrops;

extern ECORE_API int opt_static;
extern ECORE_API int opt_dynamic;

extern ECORE_API int r_debug_render_depth;

extern ECORE_API u32 ps_r4_mblur_quality;
extern ECORE_API float ps_r4_mblur_power;

enum
{
	R2FLAG_SUN					= (1<<0),
	R2FLAG_SUN_DETAILS			= (1<<3),
	R2FLAG_TONEMAP				= (1<<4),
	R2FLAG_FASTBLOOM			= (1<<7),
	R2FLAG_GLOBALMATERIAL		= (1<<8),
	R2FLAG_ZFILL				= (1<<9),
	R2FLAG_R1LIGHTS				= (1<<10),
	
	R2FLAG_EXP_SPLIT_SCENE					= (1<<13),
	R2FLAG_EXP_DONT_TEST_UNSHADOWED			= (1<<14),
	R2FLAG_EXP_DONT_TEST_SHADOWED			= (1<<15),

	R2FLAG_USE_NVDBT			= (1<<16),
	R2FLAG_USE_NVSTENCIL		= (1<<17),

	R2FLAG_EXP_MT_CALC			= (1<<18),

	R2FLAG_SOFT_WATER			= (1<<19),	//	Igor: need restart
	R2FLAG_SOFT_PARTICLES		= (1<<20),	//	Igor: need restart
	R2FLAG_VOLUMETRIC_LIGHTS	= (1<<21),
	R2FLAG_STEEP_PARALLAX		= (1<<22),
	R2FLAG_DOF					= (1<<23),

	R1FLAG_DETAIL_TEXTURES		= (1<<24),

	R2FLAG_DETAIL_BUMP			= (1<<25),

	R3FLAG_DYN_WET_SURF			= (1<<26),
	R3FLAG_VOLUMETRIC_SMOKE		= (1<<27),
	R2FLAG_LIGHTS_DETAILS		= (1<<28),
	R2FLAG_FAST_DETAILS_UPDATE	= (1<<29)
};

enum
{
	R2FLAGEXT_ENABLE_TESSELLATION	= (1<<0),
	R2FLAGEXT_WIREFRAME				= (1<<1),
	R_FLAGEXT_HOM_DEPTH_DRAW		= (1<<2),
	R2FLAGEXT_SUN_ZCULLING			= (1<<3),
	R2FLAG_USE_BUMP					= (1<<4),
	RFLAG_USE_CACHE					= (1<<5),
	RFLAG_CLOUD_SHADOWS				= (1<<6),
	RFLAG_NO_RAM_TEXTURES			= (1<<7),
	RFLAG_MT_TEX_LOAD				= (1<<8),
	RFLAG_OPT_SHAD_GEOM				= (1<<9),
	R4FLAG_SCREEN_SPACE_HUD_SHADOWS = (1<<10),
	R4FLAG_HASHED_ALPHA_TEST		= (1<<11),
	R4FLAG_SSLR_ON_WATER			= (1<<12),
	R2FLAG_SPP_VIGNETTE				= (1<<13),
	R2FLAG_SPP_ABERRATION			= (1<<14),
	R2FLAG_SPP_SATURATION			= (1<<15),
	R4FLAG_PUDDLES					= (1<<16),
	R4FLAG_OFFSCREEN_REFLECTIONS	= (1<<17),
	R4FLAG_SSLR_ON_WORLD = (1<<18),
};

extern void						xrRender_initconsole	();
extern void						xrRender_apply_tf		();

#endif

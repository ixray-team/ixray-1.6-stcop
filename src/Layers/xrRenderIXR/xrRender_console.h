#ifndef xrRender_consoleH
#define xrRender_consoleH
#pragma once

#include "RenderAPI.h"

// Common
extern RENDER_API u32 g_RainShading;
extern RENDER_API float g_RainDropsOpacity;
extern RENDER_API float g_RainDropsColorBlend;
extern RENDER_API float g_RainDropsRadius;
extern RENDER_API float g_RainDropsImpactRadius;
extern RENDER_API float g_RainDropsNear;
extern RENDER_API float g_RainDropsRefraction;
extern RENDER_API float g_RainDropsTransmission;
extern RENDER_API float g_RainDropsFeather;
extern RENDER_API int g_TextureTargetResolution;
extern RENDER_API int ps_r_texture_budget;
// The error, in pixels, a level mesh may show before the scene draws its finer clusters; 0: always the mesh as built
extern RENDER_API float ps_r_lod_error;
extern RENDER_API	u32			ps_r_sun_shafts;	//=	0;
extern RENDER_API	xr_token	qsun_shafts_token[];
extern RENDER_API bool g_SunShaftsAlternative;

extern RENDER_API	u32			ps_r2_smapsize;
extern RENDER_API	xr_token	qsmapsize_token[];

extern ENGINE_API u32 ps_r_scale_mode;
extern ENGINE_API u32 ps_proxy_r_scale_mode;

// The variables the video settings drive are written by CRender::ApplyVideoSettings alone, from the effective values
// of xrEngine/VideoSettings.h: set those, not these
extern RENDER_API	u32			ps_r_ssao_mode;

// What the scene resolve spends on a pixel: 0 low, 1 medium, 2 high, 3 ultra
extern RENDER_API u32 ps_r_shading_quality;
// Where the forward level frame takes its light from. Low: the level's lightmaps and its baked sun alone. Medium: the
// lightmaps with the sun's cascades and the lamps' shadows over them. High: nothing baked but the sky's reach, and the
// weather's sun, as the frame that shades a pixel once lights
extern RENDER_API u32 ps_r_lighting_quality;
// How the forward level frame filters a shadow map. Low: a 3x3 filter. Medium: 5x5. High: the sun's filter widens
// with the distance from the caster (PCSS), the lamps keep 5x5
extern RENDER_API u32 ps_r_shadow_quality;
// How much what is culled for speed may show. Nothing reads it yet
extern RENDER_API u32 ps_r_culling_quality;
// How far the grass is kept whole: low keeps every blade to 24 m and the whole mesh of a blade to 20 m, medium to 48
// and 40 m, high to 96 and 80 m
extern RENDER_API u32 ps_r_grass_quality;
extern RENDER_API bool ps_r_lamp_shadows;
// Experimental: the depth of what is cut out by its texture hides the grass from the cull. 1: where it covers a
// texel of the pyramid whole; 2: where it covers most of it, which may hide grass a hole would show; 3: the frame's
// one pyramid is the loose one, for the clusters as well as the grass
extern RENDER_API int ps_r_aref_occlusion;
extern RENDER_API int ps_r_scene_models;
// Shadow view batches are recorded on the task pool (D3D12)
extern RENDER_API int ps_r_parallel_views;
// How the level's opaque surfaces reach the frame. 0: the preset's choice (RenderingPath); 1: every one keeps its
// surface in the depth pass and is lit once a pixel; 2: the level as ids, the terrain and the models kept as in 1;
// 3: forward. 1 and 2 fall back to forward where the settings rule them out (GpuScene::IsVisibilityFrame)
extern RENDER_API int ps_r_rendering_path;
u32 RenderingPath();
// From medium shading quality the level is lit by the weather's sun and its lamps, nothing a lightmap holds of them;
// ps_r_lighting_quality is then only how many cascades shadow that sun (0 none, 1 the widest, 2 all)
bool ForwardDynamicLight();
extern RENDER_API int ps_r_mesh_shaders;
// The settings the frame being drawn was collected with: a command may change one in the middle of a frame, after
// the targets and shaders that depend on it were made ready
extern u32 g_ShadingQualityFrame;
extern u32 g_OcclusionModeFrame;
extern u32 g_ForwardFlagsFrame;
extern RENDER_API float ps_r_lambert_factor;
extern RENDER_API u32 ps_r_msaa;
extern u32 ps_r_reflections_update_rate;
extern float ps_r_reflections_cull_reach;
extern float ps_r_reflections_lod_error;
extern RENDER_API int ps_r_variable_shading;
// The forward level frame computes ambient occlusion: its opaque surfaces are drawn depth first
bool ForwardOcclusion();
// ... with the normals its depth pass writes (high shading quality), and with contact shadows on the HUD, which
// need those normals
bool ForwardOcclusionNormals();
bool ForwardHudShadows();
bool ForwardReflections();
// ... and weighs its blended surfaces order-independently (WBOIT), from ultra shading quality
bool ForwardWboit();
bool ForwardVelocityPass();
bool WantObjectVelocity();
extern u32 ps_r_motion_vectors_quality;
extern u32 g_MotionVectorsFrame;

extern RENDER_API	u32			ps_r2_aa_type;//	=	0;

extern RENDER_API	u32			ps_screenshot_format;//	=	0;
extern RENDER_API	xr_token	screenshot_format_token[];

extern RENDER_API	int			ps_r__LightSleepFrames;
extern RENDER_API	int			g_AnimCubicInterpolation;
extern RENDER_API float ps_r__tf_Mipbias;
extern RENDER_API	float		ps_r__Detail_l_ambient;
extern RENDER_API	float		ps_r__Detail_l_aniso;
extern RENDER_API	float		ps_r__Detail_density;

extern RENDER_API bool ps_r__detail_use_alternative_tree_assets;
extern RENDER_API bool ps_r__detail_use_cluster_mix_tree_assets;
extern RENDER_API float ps_r__detail_cluster_seed;
extern RENDER_API float ps_r__detail_cluster_patch_size_min;
extern RENDER_API float ps_r__detail_cluster_patch_size_max;
extern RENDER_API float ps_r__detail_cluster_sharpness;
extern RENDER_API float ps_r__detail_cluster_warp_min;
extern RENDER_API float ps_r__detail_cluster_warp_max;

extern RENDER_API bool ps_r__detail_fmb_use_layer_1;
extern RENDER_API float ps_r__detail_fmb_layer_1_frequency;
extern RENDER_API float ps_r__detail_fmb_layer_1_amplitude;
extern RENDER_API float ps_r__detail_fmb_layer_1_seed;
extern RENDER_API float ps_r__detail_fmb_layer_1_power;

extern RENDER_API bool ps_r__detail_fmb_use_layer_2;
extern RENDER_API float ps_r__detail_fmb_layer_2_frequency;
extern RENDER_API float ps_r__detail_fmb_layer_2_amplitude;
extern RENDER_API float ps_r__detail_fmb_layer_2_seed;
extern RENDER_API float ps_r__detail_fmb_layer_2_power;

extern RENDER_API bool ps_r__detail_fmb_use_layer_3;
extern RENDER_API float ps_r__detail_fmb_layer_3_frequency;
extern RENDER_API float ps_r__detail_fmb_layer_3_amplitude;
extern RENDER_API float ps_r__detail_fmb_layer_3_seed;
extern RENDER_API float ps_r__detail_fmb_layer_3_power;

extern RENDER_API	float		ps_r__Tree_SBC;		// scale bias correct

extern RENDER_API	float		ps_r__WallmarkTTL		;
extern RENDER_API	float		ps_r__WallmarkSHIFT		;
extern RENDER_API	float		ps_r__WallmarkSHIFT_V	;

extern RENDER_API	float		ps_r__GLOD_ssa_start;
extern RENDER_API	float		ps_r__GLOD_ssa_end	;
extern RENDER_API	float		ps_r__LOD			;
extern RENDER_API	float		ps_r__LOD_MU_X		;
extern RENDER_API	float		ps_r__LOD_MU4_discard;
extern RENDER_API	float		ps_r__ssaDISCARD	;
extern RENDER_API	float		ps_r__ssaDONTSORT	;
extern RENDER_API	float		ps_r__ssaHZBvsTEX	;
extern RENDER_API	int			ps_r__tf_Anisotropic;

// R1
extern RENDER_API	float		ps_r1_ssaLOD_A;
extern RENDER_API	float		ps_r1_ssaLOD_B;
extern RENDER_API	float		ps_r1_pps_u;
extern RENDER_API	float		ps_r1_pps_v;

// R1-specific
extern RENDER_API	int			ps_r1_GlowsPerFrame;	// r1-only
extern RENDER_API	Flags32		ps_r1_flags;			// r1-only

extern RENDER_API	float		ps_r1_fog_luminance;	//1.f r1-only
extern RENDER_API	int			ps_r1_use_terrain_mask;

extern RENDER_API	Fvector3	ps_r_taa_jitter;
extern RENDER_API	Fvector3	ps_r_taa_jitter_full;
extern RENDER_API	Fvector3	ps_r_taa_jitter_scale;

enum
{
	R1FLAG_TERRAIN_MASK	= (1 << 1),
};

extern RENDER_API Flags32		ps_r__common_flags;

// R2
extern RENDER_API	float		ps_r2_ssaLOD_A;
extern RENDER_API	float		ps_r2_ssaLOD_B;
extern RENDER_API	bool		ps_r2_particle_dt;
// R2-specific
extern RENDER_API Flags32		ps_r2_ls_flags;				// r2-only
extern RENDER_API Flags32		ps_r2_ls_flags_ext;
extern RENDER_API float			ps_r2_df_parallax_h;		// r2-only
extern RENDER_API float			ps_r2_df_parallax_range;	// r2-only
extern RENDER_API float			ps_r2_gmaterial;			// r2-only
extern RENDER_API float			ps_r2_ls_depth_scale;		// 1.0f
extern RENDER_API float			ps_r2_ls_depth_bias;		// -0.0001f
extern RENDER_API float			ps_r2_ls_squality;			// 1.0f
extern RENDER_API float			ps_r2_sun_far;
extern RENDER_API float			ps_r2_sun_near;
extern RENDER_API float			ps_r2_sun_bias;			// 0.0001f
extern RENDER_API int			ps_r2_sun_details_cascades;
extern RENDER_API float			ps_r2_sun_depth_far_scale;	// 1.00001f
extern RENDER_API float			ps_r2_sun_depth_near_scale;	// 1.00001f
extern RENDER_API float			ps_r2_sun_lumscale;			// 0.5f
extern RENDER_API float			ps_r2_sun_lumscale_hemi;	// 1.0f
extern RENDER_API float			ps_r2_sun_lumscale_amb;		// 1.0f

extern RENDER_API float			ps_r2_dhemi_sky_scale;		// 1.5f
extern RENDER_API int			ps_r2_dhemi_count;			// 5
extern RENDER_API float			ps_r2_slight_fade;			// 1.f
extern RENDER_API float			ps_r4_vslr_distance;		// 1.f
extern RENDER_API u32			g_SunContactQuality;
extern RENDER_API float			g_SunShaftsFalloff;
extern RENDER_API int g_HeightfieldShadows;
extern RENDER_API float g_HeightfieldBias;
extern RENDER_API u32 g_HeightfieldFiltering;
extern RENDER_API int			ps_r2_wait_sleep;

//	x - min (0), y - focus (1.4), z - max (100)
extern RENDER_API Fvector3		ps_r2_dof;
extern RENDER_API float			ps_r2_dof_sky;				//	distance to sky
extern RENDER_API float			ps_r2_dof_kernel_size;		//	7.0f
inline constexpr float DofSensorHeight = 0.024f, DofLensKernelScale = 2.0f;
inline float DofMaxRadius(float Kernel) { return clampr(4.0f * Kernel, 1.0f, 40.0f); }
extern RENDER_API bool g_UseManualDofFocalLength;
extern RENDER_API float g_NewDofFocalLength;
extern RENDER_API float g_NewDofFNumber;
extern RENDER_API float g_NewDofFocusTime;
extern RENDER_API float g_DofFocusDistance;
extern RENDER_API float g_DofTransitionTime;
extern RENDER_API u32 g_DofQuality;
extern RENDER_API float			ps_r2_def_aref_quality;
extern RENDER_API float			r__dtex_range;

extern RENDER_API float			ps_r3_dyn_wet_surf_near;	// 10.0f
extern RENDER_API float			ps_r3_dyn_wet_surf_far;		// 30.0f
extern RENDER_API int			ps_r3_dyn_wet_surf_sm_res;	// 256

extern RENDER_API float			ps_r4_cas_sharpening;
extern RENDER_API float			g_RainRefraction;
extern RENDER_API float			g_RainWidthScale;
extern RENDER_API u32			ps_r4_sharpening_mode;
extern RENDER_API xr_token		sharpening_mode_token[];

// Test float exported to shaders for development
extern  float					ps_r__test_exp_to_shaders_1;
extern u32 ps_r_debug_view; // DebugView_ of shaders/common/debug_view.hlsli
extern xr_token debug_view_token[];
extern  float					ps_r__test_exp_to_shaders_2;
extern  float					ps_r__test_exp_to_shaders_3;
extern  float					ps_r__test_exp_to_shaders_4;

// The exposure bias in stops of display values and the adaptation time
extern RENDER_API float		ps_r2_autoexposure_bias;
extern RENDER_API float		ps_r2_autoexposure_speed;
// r_post_processing_quality from medium: the bloom chain is drawn and added in the final pass
extern bool					g_Bloom;
extern RENDER_API float		ps_r2_bloom_amount;
// Where the tone curve starts to compress, where its shoulder reaches white, and the highlights' desaturation at white
extern RENDER_API float		ps_r2_tonemap_compression;
extern RENDER_API float		ps_r2_tonemap_white;
extern RENDER_API float		ps_r2_tonemap_desaturation;
extern RENDER_API float		ps_r2_saturation;

extern bool UseGasmak;
extern bool UseRainDrops;
extern u32 g_RainDropsMode;
extern bool UseWeaponRainDrops;

extern RENDER_API int opt_static;
extern RENDER_API int opt_dynamic;

extern RENDER_API int r_debug_render_depth;

extern RENDER_API u32 ps_r4_mblur_quality;
extern RENDER_API float ps_r4_mblur_power;
extern RENDER_API float ps_r1_full_detail_distance_scale;

enum
{
	R2FLAG_SUN_DETAILS			= (1<<3),
	R2FLAG_GLOBALMATERIAL		= (1<<8),
	
	R2FLAG_EXP_DONT_TEST_UNSHADOWED			= (1<<14),
	R2FLAG_EXP_DONT_TEST_SHADOWED			= (1<<15),

	R2FLAG_USE_NVDBT			= (1<<16),
	R2FLAG_USE_NVSTENCIL		= (1<<17),

	R2FLAG_SOFT_WATER			= (1<<19),	//	Igor: need restart
	R2FLAG_SOFT_PARTICLES		= (1<<20),	//	Igor: need restart
	R2FLAG_VOLUMETRIC_LIGHTS	= (1<<21),
	R2FLAG_STEEP_PARALLAX		= (1<<22),
	R2FLAG_DOF					= (1<<23),


	R2FLAG_DETAIL_BUMP			= (1<<25),

	R3FLAG_DYN_WET_SURF			= (1<<26),
	R2FLAG_LIGHTS_DETAILS		= (1<<28)
};

enum
{
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
	RFLAG_DD_TEX_LOAD = (1<<19),
	R4FLAG_TSHDOWS = (1 << 20),
	R4FLAG_RAIN_PUDDLES = (1 << 21),
	RFLAG_MT_MODEL_LOAD = (1 << 23),
	R4FLAG_GTAO_HQ = (1 << 24),
	R4FLAG_SSLR_HQ = (1 << 25),
};

// Registers the renderer console commands. The game calls it when the renderer is loaded;
// the editors load it before their console exists and call it after creating the console.
extern RENDER_API void			xrRender_initconsole	();
extern void						xrRender_apply_tf		();

#endif

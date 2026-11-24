#include "stdafx.h"
#include "xrRender_console.h"
#include "dxRenderDeviceRender.h"

bool UseWeaponRainDrops = true;

u32 g_RainShading = 2;
float g_RainDropsOpacity = 1.f;
float g_RainDropsColorBlend = 0.01f;
float g_RainDropsRadius = 0.1f;
float g_RainDropsImpactRadius = 1.f;
float g_RainDropsNear = 1.f;
float g_RainDropsRefraction = 12.f;
float g_RainDropsTransmission = 0.8f;
float g_RainDropsFeather = 0.15f;
static xr_token RainShadingTokens[] =
{
	{"auto", 0},
	{"simplified", 1},
	{"pbr", 2},
	{nullptr, 0}
};

u32 g_RainDropsMode = 1;
static xr_token RainDropsModeTokens[] =
	{
		{"texture", 0},
		{"procedural", 1},
		{nullptr, 0}
};

int g_TextureTargetResolution = 8192;
int ps_r_texture_budget = 512;
float ps_r_lod_error = 1.0f;

bool ps_r__detail_use_alternative_tree_assets = false;
bool ps_r__detail_use_cluster_mix_tree_assets = false;
float ps_r__detail_cluster_seed = 2790.817f;
float ps_r__detail_cluster_patch_size_min = 31.808f;
float ps_r__detail_cluster_patch_size_max = 36.098f;
float ps_r__detail_cluster_sharpness = 16.063f;
float ps_r__detail_cluster_warp_min = 0.062f;
float ps_r__detail_cluster_warp_max = 0.057f;

bool ps_r__detail_fmb_use_layer_1 = false;
float ps_r__detail_fmb_layer_1_frequency = 0.076f;
float ps_r__detail_fmb_layer_1_amplitude = 1.414f;
float ps_r__detail_fmb_layer_1_seed = 2752.25f;
float ps_r__detail_fmb_layer_1_power = 0.831f;

bool ps_r__detail_fmb_use_layer_2 = false;
float ps_r__detail_fmb_layer_2_frequency = 0.313f;
float ps_r__detail_fmb_layer_2_amplitude = 0.783f;
float ps_r__detail_fmb_layer_2_seed = 1515.0f;
float ps_r__detail_fmb_layer_2_power = 0.745f;

bool ps_r__detail_fmb_use_layer_3 = false;
float ps_r__detail_fmb_layer_3_frequency = 0.593f;
float ps_r__detail_fmb_layer_3_amplitude = 1.288f;
float ps_r__detail_fmb_layer_3_seed = 4671.25f;
float ps_r__detail_fmb_layer_3_power = 0.417f;

u32 ps_r2_smapsize = 2048;
xr_token qsmapsize_token[] = {
	{ "1024", 1024 },
	{ "2048", 2048 },
	{ "3072", 3072 },
	{ "4096", 4096 },
	{ nullptr, 0   }
};

u32			ps_r_ssao_mode			=	1;

u32			ps_r_sun_shafts				=	2;
bool g_SunShaftsAlternative = true;
xr_token							qsun_shafts_token							[ ]={
	{ "st_opt_off",					0												},
	{ "st_opt_low",					1												},
	{ "st_opt_medium",				2												},
	{ "st_opt_high",				3												},
	{ nullptr,							0												}
};

u32 ps_r_shading_quality = 2;
u32 ps_r_lighting_quality = 1;
u32 ps_r_shadow_quality = 1;
u32 ps_r_culling_quality = 1;
u32 ps_r_grass_quality = 2;
bool ps_r_lamp_shadows = true;
int ps_r_aref_occlusion = 0;
int ps_r_scene_models = 1;
int ps_r_parallel_views = 0;
int ps_r_rendering_path = 0;
int ps_r_mesh_shaders = 1;
u32 g_ShadingQualityFrame = u32(-1);
u32 g_OcclusionModeFrame = 0;
u32 g_ForwardFlagsFrame = 0;
u32 g_MotionVectorsFrame = 0;
u32 ps_r_motion_vectors_quality = 1;
float ps_r_lambert_factor = 1.0f;
u32 ps_r_msaa = 0;
u32 ps_r_reflections_update_rate = 1;
float ps_r_reflections_cull_reach = 1.0f;
float ps_r_reflections_lod_error = 8.0f;
xr_token reflections_update_rate_token[] = { { "low", 0 }, { "medium", 1 }, { "high", 2 }, { nullptr, 0 } };
int ps_r_variable_shading = 0;

// Every preset takes the ids with the kept surfaces
u32 RenderingPath()
{
	return ps_r_rendering_path ? u32(ps_r_rendering_path) : 2u;
}

bool ForwardDynamicLight()
{
	return ps_r_shading_quality >= 1;
}

// The old SSAO has no forward version: the forward level frame runs GTAO for either choice
bool ForwardOcclusion()
{
	return (g_ShadingQualityFrame == u32(-1) ? ps_r_ssao_mode : g_OcclusionModeFrame) != 0;
}

bool ForwardOcclusionNormals()
{
	return ForwardOcclusion() && (g_ShadingQualityFrame == u32(-1) ? ps_r_shading_quality : g_ShadingQualityFrame) >= 2;
}

bool ForwardHudShadows()
{
	return ForwardOcclusionNormals() && ((g_ShadingQualityFrame == u32(-1) ? ps_r2_ls_flags_ext.get() : g_ForwardFlagsFrame) & R4FLAG_SCREEN_SPACE_HUD_SHADOWS);
}

bool ForwardReflections()
{
	return ForwardOcclusionNormals() && ((g_ShadingQualityFrame == u32(-1) ? ps_r2_ls_flags_ext.get() : g_ForwardFlagsFrame) & R4FLAG_SSLR_ON_WORLD);
}

// From ultra shading quality, and without MSAA, whose samples the weighed targets do not have
bool ForwardWboit()
{
	return (g_ShadingQualityFrame == u32(-1) ? ps_r_shading_quality : g_ShadingQualityFrame) >= 3 && ps_r_msaa == 0;
}

// r_motion_vectors_quality: low - every pixel moves as the camera does, found from the depth; high - models and the
// grass move by themselves as well. DLSS is given no less than high
bool WantObjectVelocity()
{
	return RImplementation.NeedMotionVectors() && (ps_r_motion_vectors_quality >= 1 || (g_RenderFeatures.Upscaling && ps_r_scale_mode == 2));
}

// The depth pass writes how its models and its grass have moved, for what keeps a history of the frame and for
// reflections; the thin target beside it tells them from what stands still, which takes the camera's motion
bool ForwardVelocityPass()
{
	return ForwardOcclusionNormals() && (ForwardReflections() || (g_ShadingQualityFrame == u32(-1) ? u32(WantObjectVelocity()) : g_MotionVectorsFrame));
}

u32 ps_r_debug_view = 0;
xr_token debug_view_token[] = { { "off", 0 }, { "overdraw_raster", 1 }, { "overdraw_shaded", 2 }, { "clusters", 3 }, { "triangles", 4 }, { "instances", 5 }, { "materials", 6 }, { "lights", 7 },
	{ "base", 8 }, { "normal", 9 }, { "roughness", 10 }, { "hemi", 11 }, { "lmap", 12 }, { "streaming", 13 }, { "lit", 14 }, { "highlight", 15 }, { "mirror", 16 }, { "heightfield", 17 }, { nullptr, 0 } };

u32			ps_r2_aa_type			= 0;			//	=	0;

u32 ps_r4_mblur_quality = 0;

u32			ps_screenshot_format = 2;			//	=	0;
xr_token screenshot_format_token[] = { { "jpg", 0 }, { "png", 2 }, { "tga", 1 }, { nullptr, 0 } };

// Common
extern int			psSkeletonUpdate;
float			r__dtex_range = 50;

int			ps_r__LightSleepFrames		= 10	;
int			g_AnimCubicInterpolation	= 1	;

float		ps_r__Detail_l_ambient		= 0.9f	;
float		ps_r__Detail_l_aniso		= 0.25f	;
float		ps_r__Detail_density		= 0.3f	;
float		ps_r__Detail_rainbow_hemi	= 0.75f	;

float		ps_r__Tree_SBC				= 1.5f	;	// scale bias correct

float		ps_r__WallmarkTTL			= 50.f	;
float		ps_r__WallmarkSHIFT			= 0.001f;
float		ps_r__WallmarkSHIFT_V		= 0.001f;
bool		ps_r__WallmarkDyn			= true;

float		ps_r__GLOD_ssa_start		= 256.f	;
float		ps_r__GLOD_ssa_end			=  64.f	;
float		ps_r__LOD					=  0.75f	;
float		ps_r__LOD_MU_X				= 1.0f;
float		ps_r__LOD_MU4_discard		= 0.001f;
float		ps_r__ssaDISCARD			=  3.5f	;					//RO
float		ps_r__ssaDONTSORT			=  32.f	;					//RO
float		ps_r__ssaHZBvsTEX			=  96.f	;					//RO

int			ps_r__tf_Anisotropic		= 16		;

// R1
float		ps_r1_ssaLOD_A				= 64.f	;
float		ps_r1_ssaLOD_B				= 48.f	;
float ps_r__tf_Mipbias = -0.1f;
Flags32		ps_r1_flags					= { R1FLAG_TERRAIN_MASK };		// r1-only
float		ps_r1_pps_u					= 0.f	;
float		ps_r1_pps_v					= 0.f	;

// R1-specific
int			ps_r1_GlowsPerFrame			= 16	;					// r1-only
float		ps_r1_fog_luminance			= 1.1f	;					// r1-only
int			ps_r1_use_terrain_mask		= 0;
// R2
float		ps_r2_ssaLOD_A				= 48.f	;
float		ps_r2_ssaLOD_B				= 32.f	;

Fvector3	ps_r_taa_jitter = { 0,0,0 };
Fvector3	ps_r_taa_jitter_full = { 0,0,0 };
Fvector3	ps_r_taa_jitter_scale = { 1,1,0 };

// R2-specific
Flags32		ps_r2_ls_flags = {
	R2FLAG_EXP_DONT_TEST_UNSHADOWED
	| R2FLAG_USE_NVSTENCIL
	| R3FLAG_DYN_WET_SURF
	| R2FLAG_DETAIL_BUMP
	| R2FLAG_SOFT_PARTICLES
	| R2FLAG_SOFT_WATER
	| R2FLAG_STEEP_PARALLAX
	| R2FLAG_VOLUMETRIC_LIGHTS
};	// r2-only

Flags32 ps_r2_ls_flags_ext =
{
	RFLAG_CLOUD_SHADOWS |
	R4FLAG_SCREEN_SPACE_HUD_SHADOWS |
	R4FLAG_HASHED_ALPHA_TEST
};


Flags32 ps_r__common_flags = 
{ 
	R2FLAG_USE_BUMP | RFLAG_USE_CACHE | RFLAG_NO_RAM_TEXTURES | RFLAG_MT_TEX_LOAD | RFLAG_MT_MODEL_LOAD | RFLAG_DD_TEX_LOAD
};

int opt_static = 0;
int opt_dynamic = 0;

float		ps_r2_df_parallax_h			= 0.02f;
float		ps_r2_df_parallax_range		= 60.f;
float		ps_r2_autoexposure_bias		= 0.0f;
float		ps_r2_autoexposure_speed	= 1.f;
bool		g_Bloom						= true;
float		ps_r2_bloom_amount			= 0.04f;
float		ps_r2_tonemap_compression	= 0.7f;
float		ps_r2_tonemap_white			= 4.0f;
float		ps_r2_saturation			= 1.0f;
float		ps_r2_tonemap_desaturation	= 0.2f;
float		ps_r2_ls_depth_scale = 0.9999f; // 1.00001f
float		ps_r2_ls_depth_bias = 0.00001f; // -0.0001f
float		ps_r2_ls_squality			= 10.0f;				// 1.00f

float		ps_r2_sun_bias				= -0.01f;			// 
float		ps_r2_sun_far				= 160.f;
float		ps_r2_sun_near				= 15.f;
int			ps_r2_sun_details_cascades	= 2;
float		ps_r2_sun_depth_far_scale	= 1.00000f;			// 1.00001f
float		ps_r2_sun_depth_near_scale	= 1.0000f;			// 1.00001f
float		ps_r2_sun_lumscale			= 1.1f;				// 1.0f
float		ps_r2_sun_lumscale_hemi		= 0.95f;				// 1.0f
float		ps_r2_sun_lumscale_amb		= 0.6f;
float		ps_r2_gmaterial				= 2.2f;				// 

float		ps_r2_dhemi_sky_scale		= 0.08f;				// 1.5f
int			ps_r2_dhemi_count			= 5;				// 5
int			ps_r2_wait_sleep			= 0;

float		ps_r4_mblur_power = 0.25f;
float		ps_r1_full_detail_distance_scale = 0.5f;

float		ps_r2_lt_smooth				= 1.f;				// 1.f
float		ps_r2_slight_fade			= 0.6f;				// 1.f

float		ps_r4_vslr_distance			= 0.7f;				// 1.f

u32			g_SunContactQuality			= 2;
float		g_SunShaftsFalloff			= 0.01f;
int g_HeightfieldShadows = 0;
float g_HeightfieldBias = 0.75f;
u32 g_HeightfieldFiltering = 0;
static xr_token HeightfieldFilteringTokens[] = { { "default", 0 }, { "3x3", 1 }, { "5x5", 2 }, { "7x7", 3 }, { nullptr, 0 } };

//	x - min (0), y - focus (1.4), z - max (100)
Fvector3	ps_r2_dof					= Fvector3().set(-1.25f, 1.4f, 600.f);
float		ps_r2_dof_sky				= 30;				//	distance to sky
float		ps_r2_dof_kernel_size		= 5.0f;						//	7.0f
bool g_UseManualDofFocalLength = false;
float g_NewDofFocalLength = 10.0f;
float g_NewDofFNumber = 4.0f;
float g_NewDofFocusTime = 0.1f;
float g_DofFocusDistance = 1.4f;
float g_DofTransitionTime = 0.1f;
u32 g_DofQuality = 1;
static xr_token DofQualityTokens[] =
{
	{"low", 0},
	{"medium", 1},
	{"high", 2},
	{nullptr, 0}
};

float		ps_r2_def_aref_quality = 115.0f;

float		ps_r3_dyn_wet_surf_near		= 10.f;				// 10.0f
float		ps_r3_dyn_wet_surf_far		= 30.f;				// 30.0f
int			ps_r3_dyn_wet_surf_sm_res	= 512;				// 512
float		ps_r2_gloss_factor = 3.14f;

int			ps_r__detail_radius = 300;
float		ps_r4_cas_sharpening = 0.0f;
float		g_RainRefraction = 1.0f;
float		g_RainWidthScale = 0.1f;
u32			ps_r4_sharpening_mode = 0;

xr_token sharpening_mode_token[] =
{
	{ "tiny_sharpening", 0},
	{ "amd_cas", 1},
	{ nullptr, 0}
};

float		ps_r__detail_rnd_scale_min = 0.3f;
float		ps_r__detail_rnd_scale_max = 0.9f;

float		ps_trample_bend = 12.94f;
float		ps_trample_squash = 3.64f;
float		ps_trample_trail_min = 0.23f;
float		ps_trample_trail_max = 3.25f;
float		ps_trample_obj_radius_scale = 0.67f;
float		ps_trample_actor_radius_scale = 0.8f;
int			ps_trample_enabled = 0;
float		ps_trample_cooltime = 30.0f;
float		ps_trample_draw_radius = 63.0f;
float		ps_trample_brush_fill = 0.82f;
float		ps_trample_press_speed = 0.8f;

int			ps_wind_enabled = 0;
int			ps_wind_mode = 0;
float		ps_wind_blend = 0.5f;
float		ps_wind_blend_current = 0.5f;
float		ps_wind_noise_scale = 0.5f;
float		ps_wind_noise_speed = 0.3f;
float		ps_wind_noise_angle = 45.0f;
int			ps_wind_xz_enabled = 1;
int			ps_wind_swirl_enabled = 1;
int			ps_wind_xz1_on = 1;
float		ps_wind_xz1_scale_min = 0.2f;
float		ps_wind_xz1_scale_max = 0.5f;
float		ps_wind_xz1_int_min = 0.1f;
float		ps_wind_xz1_int_max = 0.6f;
float		ps_wind_xz1_con_min = 0.2f;
float		ps_wind_xz1_con_max = 0.6f;
float		ps_wind_xz1_spd_min = 0.1f;
float		ps_wind_xz1_spd_max = 0.5f;
float		ps_wind_xz1_ang_min = 30.0f;
float		ps_wind_xz1_ang_max = 60.0f;
int			ps_wind_xz2_on = 1;
float		ps_wind_xz2_scale_min = 0.5f;
float		ps_wind_xz2_scale_max = 1.0f;
float		ps_wind_xz2_int_min = 0.08f;
float		ps_wind_xz2_int_max = 0.5f;
float		ps_wind_xz2_con_min = 0.2f;
float		ps_wind_xz2_con_max = 0.6f;
float		ps_wind_xz2_spd_min = 0.3f;
float		ps_wind_xz2_spd_max = 1.2f;
float		ps_wind_xz2_ang_min = 45.0f;
float		ps_wind_xz2_ang_max = 75.0f;
int			ps_wind_xz3_on = 1;
float		ps_wind_xz3_scale_min = 1.0f;
float		ps_wind_xz3_scale_max = 2.0f;
float		ps_wind_xz3_int_min = 0.05f;
float		ps_wind_xz3_int_max = 0.35f;
float		ps_wind_xz3_con_min = 0.2f;
float		ps_wind_xz3_con_max = 0.6f;
float		ps_wind_xz3_spd_min = 0.8f;
float		ps_wind_xz3_spd_max = 3.0f;
float		ps_wind_xz3_ang_min = 15.0f;
float		ps_wind_xz3_ang_max = 45.0f;
float		ps_wind_sw_scale_min = 0.3f;
float		ps_wind_sw_scale_max = 0.8f;
float		ps_wind_sw_int_min = 0.05f;
float		ps_wind_sw_int_max = 0.4f;
float		ps_wind_sw_con_min = 0.2f;
float		ps_wind_sw_con_max = 0.6f;
float		ps_wind_sw_spd_min = 0.3f;
float		ps_wind_sw_spd_max = 1.5f;
float		ps_wind_sw_ang_min = 75.0f;
float		ps_wind_sw_ang_max = 105.0f;

// Test float exported to shaders for development
float		ps_r__test_exp_to_shaders_1	= 1.0f;
float		ps_r__test_exp_to_shaders_2	= 1.0f;
float		ps_r__test_exp_to_shaders_3	= 1.0f;
float		ps_r__test_exp_to_shaders_4	= 1.0f;

bool		ps_r2_particle_dt			= false;

int			r_debug_render_depth		= 0;

#include "../../xrEngine/XR_IOConsole.h"
#include	"../../xrEngine/xr_ioc_cmd.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/MonsterLogicTelemetry.h"
#include "../../xrCore/Kernel/EngineExternal.h"
#include "StaticTraversalStats.h"

class CCC_MonsterLogic : public IConsole_Command
{
public:
	CCC_MonsterLogic(const char* Name) : IConsole_Command(Name) {}
	void Execute(const char* Args) override
	{
		int Value;
		if (!xr_strcmp(Args, "on") || !xr_strcmp(Args, "1"))
		{
			Value = 1;
		}
		else if (!xr_strcmp(Args, "off") || !xr_strcmp(Args, "0"))
		{
			Value = 0;
		}
		else if (!xr_strcmp(Args, "config"))
		{
			Value = -1;
		}
		else
		{
			InvalidSyntax();
			return;
		}
		EngineExternal().SetMonsterLogicOverride(Value);
		const u32 Count = g_pGameLevel ? g_pGameLevel->ReloadMonsterLogicOptions() : 0;
		g_MonsterLogicTelemetry.Reset();
		Msg("Monster logic: %s; refreshed %u monsters", EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ? "ON" : "VANILLA", Count);
	}
	void Status(TStatus& S) override
	{
		const int Value = EngineExternal().GetMonsterLogicOverride();
		xr_strcpy(S, Value < 0 ? "config" : (Value ? "on" : "off"));
	}
	void Info(TInfo& I) override { xr_strcpy(I, "on/off/config"); }
	void Save(IWriter*) override {}
};

class CCC_MonsterLogicDraw : public IConsole_Command
{
public:
	CCC_MonsterLogicDraw(const char* Name) : IConsole_Command(Name) {}
	void Execute(const char* Args) override
	{
		const bool On = !xr_strcmp(Args, "on") || !xr_strcmp(Args, "1");
		if (!On && xr_strcmp(Args, "off") && xr_strcmp(Args, "0"))
		{
			InvalidSyntax();
			return;
		}
		g_MonsterLogicTelemetry.DrawEnabled.store(On, std::memory_order_relaxed);
		if (g_pGameLevel) g_pGameLevel->SetMonsterLogicDraw(On);
		if (On)
		{
			Msg("* Player sharing: green=own sight, white=close detection, yellow=radio, cyan=wireless, magenta=call; transfer arrows last 5s; yellow cross/up arrow=last known point while memory is valid");
		}
	}
	void Status(TStatus& S) override { xr_strcpy(S, g_MonsterLogicTelemetry.DrawEnabled.load(std::memory_order_relaxed) ? "on" : "off"); }
	void Info(TInfo& I) override { xr_strcpy(I, "on/off: player enemy sharing spheres and arrows"); }
	void Save(IWriter*) override {}
};

class CCC_MonsterLogicStats : public IConsole_Command
{
public:
	CCC_MonsterLogicStats(const char* Name) : IConsole_Command(Name) {}
	void Execute(const char* Args) override
	{
		if (!xr_strcmp(Args, "reset"))
		{
			g_MonsterLogicTelemetry.Reset();
			return;
		}
		const bool Full = !xr_strcmp(Args, "full") || !xr_strcmp(Args, "on") || !xr_strcmp(Args, "1");
		const bool On = Full || !xr_strcmp(Args, "brief") || !xr_strcmp(Args, "on") || !xr_strcmp(Args, "1");
		if (!On && xr_strcmp(Args, "off") && xr_strcmp(Args, "0"))
		{
			InvalidSyntax();
			return;
		}
		g_MonsterLogicTelemetry.Detailed.store(Full, std::memory_order_relaxed);
		g_MonsterLogicTelemetry.Enabled.store(On, std::memory_order_relaxed);
		g_MonsterLogicTelemetry.Reset();
		if (On)
		{
			Console->Execute("rs_fps_show on");
		}
	}
	void Status(TStatus& S) override
	{
		xr_strcpy(S, !g_MonsterLogicTelemetry.Enabled.load(std::memory_order_relaxed) ? "off" :
			g_MonsterLogicTelemetry.Detailed.load(std::memory_order_relaxed) ? "full" : "brief");
	}
	void Info(TInfo& I) override { xr_strcpy(I, "on/off/brief/full/reset"); }
	void Save(IWriter*) override {}
};



#	include "StateManager/dx10SamplerStateCache.h"

//-----------------------------------------------------------------------
class CCC_tf_MipBias: public CCC_Float {
public:
	CCC_tf_MipBias(const char* N, float* v) : CCC_Float(N, v, -3.0f, 3.0f) {};
	void apply() {
		if (0 == GRHI->DevicePtr) {
			return;
		}
		float val = *value;
		clamp(val, -3.0f, 3.0f);
		RenderThread::WaitIdle();
		SSManager.SetMipLodBias(val);
	}

	virtual void Execute(const char* args) override
	{
		CCC_Float::Execute(args);
		apply();
	}

	virtual void Status(TStatus& S) override
	{
		CCC_Float::Status(S);
		apply();
	}
};

class CCC_R2GM : public CCC_Float
{
public:
	CCC_R2GM(const char* N, float*	v) : CCC_Float(N, v, 0.f, 4.f) { *v = 0; };

	virtual void	Execute	(const char* args) override
	{
		if (0==xr_strcmp(args,"on"))	{
			ps_r2_ls_flags.set	(R2FLAG_GLOBALMATERIAL,true);
		} else if (0==xr_strcmp(args,"off"))	{
			ps_r2_ls_flags.set	(R2FLAG_GLOBALMATERIAL,false);
		} else {
			CCC_Float::Execute	(args);
			if (ps_r2_ls_flags.test(R2FLAG_GLOBALMATERIAL))	{
				static const char*	name[4]	=	{ "oren", "blin", "phong", "metal" };
				float	mid		= *value	;
				int		m0		= iFloor(mid)	% 4;
				int		m1		= (m0+1)		% 4;
				float	frc		= mid - float(iFloor(mid));
				Msg		("* material set to [%s]-[%s], with lerp of [%f]",name[m0],name[m1],frc);
			}
		}
	}
};
class CCC_Screenshot : public IConsole_Command
{
public:
	CCC_Screenshot(const char* N) : IConsole_Command(N)  { };

	virtual void Execute(const char* args) override
	{
		if (g_dedicated_server)
			return;

		string_path	name;	name[0]=0;
		sscanf		(args,"%s",	name);
		const char*		image	= xr_strlen(name)?name:0;
		::Render->Screenshot(IRender_interface::SM_NORMAL,image);
	}
};

//-----------------------------------------------------------------------
class CCC_memory_stats : public IConsole_Command
{
protected	:

public		:

	CCC_memory_stats(const char* N) :	IConsole_Command(N)	{ bEmptyArgsHandled = true; };

	virtual void	Execute	(const char* args) override
	{
		u32 m_base = 0;
		u32 c_base = 0;
		u32 m_lmaps = 0; 
		u32 c_lmaps = 0;

		dxRenderDeviceRender::Instance().ResourcesGetMemoryUsage( m_base, c_base, m_lmaps, c_lmaps );
		dxRenderDeviceRender::Instance().Resources->LogVideoMemory();
		RImplementation.Scene.LogTexturePools();
		RImplementation.Pacer.LogLatency();
	}

};

class CCC_DofFar : public CCC_Float
{
public:
	CCC_DofFar(const char* N, float* V, float _min=0.0f, float _max=10000.0f) 
		: CCC_Float( N, V, _min, _max){}

	virtual void Execute(const char* args) override
	{
		float v = float(atof(args));

		if (v<ps_r2_dof.y+0.1f)
		{
			char	pBuf[256];
			snprintf(pBuf, sizeof(pBuf) / sizeof(pBuf[0]), "float value greater or equal to r2_dof_focus+0.1");
			Msg("~ Invalid syntax in call to '%s'",cName);
			Msg("~ Valid arguments: %s", pBuf);
			Console->Execute("r2_dof_focus");
		}
		else
		{
			CCC_Float::Execute(args);
			if(g_pGamePersistent)
				g_pGamePersistent->SetBaseDof(ps_r2_dof);
		}
	}

	//	CCC_Dof should save all data as well as load from config
	virtual void	Save	(IWriter *F) override {;}
};

class CCC_DofNear : public CCC_Float
{
public:
	CCC_DofNear(const char* N, float* V, float _min=0.0f, float _max=10000.0f) 
		: CCC_Float( N, V, _min, _max){}

	virtual void Execute(const char* args) override
	{
		float v = float(atof(args));

		if (v>ps_r2_dof.y-0.1f)
		{
			char	pBuf[256];
			snprintf(pBuf, sizeof(pBuf) / sizeof(pBuf[0]), "float value less or equal to r2_dof_focus-0.1");
			Msg("~ Invalid syntax in call to '%s'",cName);
			Msg("~ Valid arguments: %s", pBuf);
			Console->Execute("r2_dof_focus");
		}
		else
		{
			CCC_Float::Execute(args);
			if(g_pGamePersistent)
				g_pGamePersistent->SetBaseDof(ps_r2_dof);
		}
	}

	//	CCC_Dof should save all data as well as load from config
	virtual void	Save	(IWriter *F) override {;}
};

class CCC_DofFocus : public CCC_Float
{
public:
	CCC_DofFocus(const char* N, float* V, float _min=0.0f, float _max=10000.0f) 
		: CCC_Float( N, V, _min, _max){}

	virtual void Execute(const char* args) override
	{
		float v = float(atof(args));

		if (v>ps_r2_dof.z-0.1f)
		{
			char	pBuf[256];
			snprintf(pBuf, sizeof(pBuf) / sizeof(pBuf[0]), "float value less or equal to r2_dof_far-0.1");
			Msg("~ Invalid syntax in call to '%s'",cName);
			Msg("~ Valid arguments: %s", pBuf);
			Console->Execute("r2_dof_far");
		}
		else if (v<ps_r2_dof.x+0.1f)
		{
			char	pBuf[256];
			snprintf(pBuf, sizeof(pBuf) / sizeof(pBuf[0]), "float value greater or equal to r2_dof_far-0.1");
			Msg("~ Invalid syntax in call to '%s'",cName);
			Msg("~ Valid arguments: %s", pBuf);
			Console->Execute("r2_dof_near");
		}
		else{
			CCC_Float::Execute(args);
			if(g_pGamePersistent)
				g_pGamePersistent->SetBaseDof(ps_r2_dof);
			}
	}

	//	CCC_Dof should save all data as well as load from config
	virtual void	Save	(IWriter *F) override {;}
};

class CCC_Dof : public CCC_Vector3
{
public:
	CCC_Dof(const char* N, Fvector* V, const Fvector _min, const Fvector _max) : 
	  CCC_Vector3(N, V, _min, _max) {;}

	virtual void	Execute	(const char* args) override
	{
		Fvector v;
		if (3!=sscanf(args,"%f,%f,%f",&v.x,&v.y,&v.z))	
			InvalidSyntax(); 
		else if ( (v.x > v.y-0.1f) || (v.z < v.y+0.1f))
		{
			InvalidSyntax();
			Msg("x <= y - 0.1");
			Msg("y <= z - 0.1");
		}
		else
		{
			CCC_Vector3::Execute(args);
			if(g_pGamePersistent)
				g_pGamePersistent->SetBaseDof(ps_r2_dof);
		}
	}

	virtual void	Status	(TStatus& S) override
	{	
		xr_sprintf	(S,"%f,%f,%f",value->x,value->y,value->z);
	}

	virtual void	Info	(TInfo& I) override
	{	
		xr_sprintf(I,"vector3 in range [%f,%f,%f]-[%f,%f,%f]",min.x,min.y,min.z,max.x,max.y,max.z);
	}

};

class CCC_DumpResources : public IConsole_Command
{
public:
	CCC_DumpResources(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; };

	virtual void Execute(const char* args) override
	{
		dxRenderDeviceRender::Instance().Resources->Dump(false);
	}
};

class CCC_RecompileShaders : public IConsole_Command
{
public:
	CCC_RecompileShaders(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }
	virtual void Execute(const char* args)
	{
		(void)args;
		dxRenderDeviceRender::Instance().Resources->RecompileDX12Shaders();
	}
};

class CCC_DetailRadius : public CCC_Integer
{
public:
	CCC_DetailRadius(const char* N, int* V, int _min = 0, int _max = 999) : CCC_Integer(N, V, _min, _max)
	{
	};

	virtual void Execute(const char* args) override
	{
		CCC_Integer::Execute(args);
		SetDetailRadius(ps_r__detail_radius);

		if (RImplementation.GraphMain.b_loaded && (dm_current_size != dm_size))
		{
			Device.DetailsTask.wait();
			RenderThread::WaitIdle();
			RImplementation.Details->cache_ReInitialize();
		}
	}

	virtual void Status(TStatus& S) override
	{
		CCC_Integer::Status(S);
	}
};

class CCC_DetailDensity : public CCC_Float
{
public:
	CCC_DetailDensity(const char* N, float* V, float _min = 0.0f, float _max = 10000.0f)
		: CCC_Float(N, V, _min, _max) {
	}

	virtual void Execute(const char* args) override
	{
		CCC_Float::Execute(args);

		if (RImplementation.GraphMain.b_loaded)
		{
			Device.DetailsTask.wait();
			RenderThread::WaitIdle();
			RImplementation.Details->cache_ReInitialize();
		}
	}

	virtual void Status(TStatus& S) override
	{
		CCC_Float::Status(S);
	}
};

class CCC_DetailReloadDetails : public CCC_Float
{
public:
	CCC_DetailReloadDetails(const char* N, float* V, float _min = 0.0f, float _max = 10000.0f)
		: CCC_Float(N, V, _min, _max) {
	}

	virtual void Execute(const char* args) override
	{
		CCC_Float::Execute(args);
		RImplementation.Details->RequestCacheRebuild();
	}

	virtual void Status(TStatus& S) override
	{
		CCC_Float::Status(S);
	}
};

class CCC_DetailReloadDetails_Boolean : public CCC_Boolean
{
public:
	CCC_DetailReloadDetails_Boolean(const char* N, bool* V)
		: CCC_Boolean(N, V) {}

	virtual void Execute(const char* args) override
	{
		CCC_Boolean::Execute(args);
		RImplementation.Details->RequestCacheRebuild();
	}
};

// Manual bake: regenerate the precomputed fields from the current console settings
// and persist them to disk immediately. Useful when tweaking r__detail_* via console
// and wanting the files updated without waiting for the next level load.
class CCC_DetailLayersForceBake : public IConsole_Command
{
public:
	CCC_DetailLayersForceBake(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; };

	virtual void Execute(const char* args) override
	{
		if (!RImplementation.Details || !RImplementation.GraphMain.b_loaded)
		{
			Msg("! detail_layers: no loaded level, nothing to bake");
			return;
		}
		Device.DetailsTask.wait();
		// Rebuild first so stale in-memory fields never hit the disk, then persist.
		RenderThread::WaitIdle();
		RImplementation.Details->cache_ReInitialize();
		RImplementation.Details->DetailLayers_SaveToBake();
	}
};

// Rollback: restore the r__detail_* console state from the baked settings.txt and load
// the last bake from disk. Restoring the settings first keeps crc consistent - otherwise
// a later console flag flip or level restart would recompute/overwrite the bake.
class CCC_DetailLayersForceLoad : public IConsole_Command
{
public:
	CCC_DetailLayersForceLoad(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; };

	virtual void Execute(const char* args) override
	{
		if (!RImplementation.Details || !RImplementation.GraphMain.b_loaded)
		{
			Msg("! detail_layers: no loaded level, nothing to load");
			return;
		}
		Device.DetailsTask.wait();
		RImplementation.Details->DetailLayers_ApplySettingsFromBake();
		// crc now matches the baked settings -> fields come back from disk.
		RenderThread::WaitIdle();
		RImplementation.Details->cache_ReInitialize();
	}
};

extern Fvector3 ps_ssfx_volumetric;

class SunShadowCacheStats final : public IConsole_Command
{
public:
	explicit SunShadowCacheStats(const char* Name) : IConsole_Command(Name)
	{
		bEmptyArgsHandled = true;
	}
	~SunShadowCacheStats() override = default;

	void Execute(const char*) override
	{
		RImplementation.DumpSunShadowCache();
	}
};

class CCC_SpotShadowCacheClear : public IConsole_Command
{
public:
	explicit CCC_SpotShadowCacheClear(const char* Name) : IConsole_Command(Name)
	{
		bEmptyArgsHandled = true;
	}

	void Execute(const char*) override
	{
		if (RImplementation.Target)
		{
			RImplementation.Target->InvalidateSpotShadowCache();
		}
	}
};

class CCC_SpotShadowCacheStats : public IConsole_Command
{
public:
	explicit CCC_SpotShadowCacheStats(const char* Name) : IConsole_Command(Name)
	{
		bEmptyArgsHandled = true;
	}

	void Execute(const char*) override
	{
		if (RImplementation.Target)
		{
			RImplementation.Target->DumpSpotShadowCache();
		}
	}
};

class CCC_DumpStaticTraversal : public IConsole_Command
{
public:
	explicit CCC_DumpStaticTraversal(const char* Name) : IConsole_Command(Name)
	{
		bEmptyArgsHandled = true;
	}

	void Execute(const char*) override
	{
		g_StaticTraversalStats.Request();
	}

	void Info(TInfo& I) override
	{
		xr_strcpy(I, "log per-sector static traversal stats of the next main view frame");
	}
};
//-----------------------------------------------------------------------
RENDER_API void		xrRender_initconsole	()
{
	CMD1(SunShadowCacheStats, "r4_sun_shadow_cache_stats");
	CMD1(CCC_SpotShadowCacheClear, "r4_spot_shadow_cache_clear");
	CMD1(CCC_SpotShadowCacheStats, "r4_spot_shadow_cache_stats");
	CMD1(CCC_DumpStaticTraversal, "r_dump_static");
	CMD1(CCC_MonsterLogic, "monster_logic");
	CMD1(CCC_MonsterLogicStats, "monster_logic_stats");
	CMD1(CCC_MonsterLogicDraw, "monster_logic_draw");

	CMD4(CCC_Integer,	"rs_skeleton_update",	&psSkeletonUpdate,	2,		128	);
	CMD4(CCC_Integer,	"r__anim_cubic",		&g_AnimCubicInterpolation,	0,	1	);
	CMD4(CCC_Float,		"r__dtex_range",		&r__dtex_range,		5,		175	);

// Common
	CMD1(CCC_Screenshot,"screenshot"			);
	CMD4(CCC_Float, "r__wallmark_ttl", &ps_r__WallmarkTTL, 1.0f, 10.f * 60.f);

	CMD4(CCC_Float,		"r__geometry_lod",		&ps_r__LOD,					0.1f,	1.2f		);
	CMD4(CCC_Float,		"r__mu_lod",			&ps_r__LOD_MU_X,			0.1f,	3.0f);

	CMD2(CCC_Vector3, "r4_ssfx_volumetric", &ps_ssfx_volumetric);
	CMD3(CCC_Token, "r4.sharpening.mode", &ps_r4_sharpening_mode, sharpening_mode_token);

#ifdef DEBUG
	CMD4(CCC_Float,		"r__mu4_discard_lod",	&ps_r__LOD_MU4_discard,		0.001f, 10.0f);
	CMD4(CCC_Float,		"r__detail_l_ambient",	&ps_r__Detail_l_ambient,	.5f,	.95f	);
	CMD4(CCC_Float,		"r__detail_l_aniso",	&ps_r__Detail_l_aniso,		.1f,	.5f		);
#endif // DEBUG

	CMD2(CCC_tf_MipBias, "r__tf_mipbias", &ps_r__tf_Mipbias);//	{-3 +3}

	// R1
	CMD4(CCC_Float,		"r_pps_u",				&ps_r1_pps_u,				-1.f,	+1.f	);
	CMD4(CCC_Float,		"r_pps_v",				&ps_r1_pps_v,				-1.f,	+1.f	);

	// R1-specific
	CMD4(CCC_Integer,	"r_glows_per_frame",	&ps_r1_GlowsPerFrame,		2,		32		);

	CMD4(CCC_Float,		"r_fog_luminance",		&ps_r1_fog_luminance,		0.2f,	5.f	);

	// R2
	CMD4(CCC_Float,		"r2_ssa_lod_a",			&ps_r2_ssaLOD_A,			0,		512		);
	CMD4(CCC_Float,		"r2_ssa_lod_b",			&ps_r2_ssaLOD_B,			0,		512		);

	// R2-specific
	CMD4(CCC_Float,		"r2_gloss_factor",		&ps_r2_gloss_factor,		.0f,	10.f	);

#ifdef DEBUG
	CMD3(CCC_Mask32,		"r2_use_nvdbt",			&ps_r2_ls_flags,			R2FLAG_USE_NVDBT);
#endif // DEBUG

	CMD4(CCC_Integer,	"r2_sun_details_cascades",&ps_r2_sun_details_cascades,0,	3		);
	CMD3(CCC_Mask32,		"r2_exp_donttest_shad",	&ps_r2_ls_flags,			R2FLAG_EXP_DONT_TEST_SHADOWED);
	
	CMD4(CCC_Float,		"r2_sun_bias",			&ps_r2_sun_bias,			-0.5,	+0.5	);
	CMD4(CCC_Float,		"r2_sun_near",			&ps_r2_sun_near,			1.f,	50.f	);
	CMD4(CCC_Float,		"r2_sun_far",			&ps_r2_sun_far,				51.f,	1000.f	);
	CMD4(CCC_Float,		"r2_sun_depth_far_scale",&ps_r2_sun_depth_far_scale,0.5,	1.5		);
	CMD4(CCC_Float,		"r2_sun_depth_near_scale",&ps_r2_sun_depth_near_scale,0.5,	1.5		);

	CMD4(CCC_Float,		"r2_sun_lumscale",		&ps_r2_sun_lumscale,		-1.0,	+3.0	);
	CMD4(CCC_Float,		"r2_sun_lumscale_hemi",	&ps_r2_sun_lumscale_hemi,	0.0,	+3.0	);
	CMD4(CCC_Float,		"r2_sun_lumscale_amb",	&ps_r2_sun_lumscale_amb,	0.0,	+3.0	);

	CMD4(CCC_Float,		"r2_ls_depth_scale",	&ps_r2_ls_depth_scale,		0.5,	1.5		);
	CMD4(CCC_Float,		"r2_ls_depth_bias",		&ps_r2_ls_depth_bias,		-0.5,	+0.5	);

	CMD4(CCC_Float,		"r2_parallax_h",		&ps_r2_df_parallax_h,		.0f,	.5f		);
	CMD4(CCC_Float,		"r2_parallax_range",	&ps_r2_df_parallax_range,	5.0f,	175.0f	);

	CMD4(CCC_Float,		"r2_slight_fade",		&ps_r2_slight_fade,			.2f,	1.f		);

	//	Igor: Depth of field
	Fvector	tw_min = {}, tw_max = {};
	tw_min.set			(-10000,-10000,0);	tw_max.set	(10000,10000,10000);
	CMD4( CCC_Dof,		"r2_dof",		&ps_r2_dof, tw_min, tw_max);
	CMD4( CCC_DofNear,	"r2_dof_near",	&ps_r2_dof.x, tw_min.x, tw_max.x);
	CMD4( CCC_DofFocus,	"r2_dof_focus", &ps_r2_dof.y, tw_min.y, tw_max.y);
	CMD4( CCC_DofFar,	"r2_dof_far",	&ps_r2_dof.z, tw_min.z, tw_max.z);
	CMD4(CCC_Float,		"r2_dof_kernel",&ps_r2_dof_kernel_size,				.0f,	80.f);
	CMD2(CCC_Boolean, "r4_ndof_manual_focal_length", &g_UseManualDofFocalLength);
	CMD4(CCC_Float, "r4_ndof_focal_length", &g_NewDofFocalLength, 1.0f, 1000.0f);
	CMD4(CCC_Float, "r4_ndof_f_number", &g_NewDofFNumber, 0.7f, 64.0f);
	CMD4(CCC_Float, "r4_ndof_focus_time", &g_NewDofFocusTime, 0.01f, 10.0f);
	CMD4(CCC_Float, "r4_ndof_focus_distance", &g_DofFocusDistance, 0.01f, 10000.0f);
	CMD4(CCC_Float, "r4_ndof_transition_time", &g_DofTransitionTime, 0.01f, 10.0f);
	CMD3(CCC_Token, "r4_ndof_quality", &g_DofQuality, DofQualityTokens);
	CMD4(CCC_Float,		"r2_dof_sky",	&ps_r2_dof_sky,						-10000.f,	10000.f);

	CMD4(CCC_Float,		"r2_sun_shafts_falloff",		&g_SunShaftsFalloff,		0.0f, 0.2f);
	CMD4(CCC_Integer, "r4_heightfield_shadows", &g_HeightfieldShadows, 0, 1);
	CMD4(CCC_Float, "r4_heightfield_bias", &g_HeightfieldBias, 0.0f, 5.0f);
	CMD3(CCC_Token, "heighfield_filtering", &g_HeightfieldFiltering, HeightfieldFilteringTokens);
	CMD4(CCC_Integer, "r_aref_occlusion", &ps_r_aref_occlusion, 0, 4);
	CMD4(CCC_Integer, "r_scene_models", &ps_r_scene_models, 0, 1);
	CMD4(CCC_Integer, "r_parallel_views", &ps_r_parallel_views, 0, 1);
	CMD4(CCC_Integer, "r_rendering_path", &ps_r_rendering_path, 0, 3);
	CMD4(CCC_Integer, "r_mesh_shaders", &ps_r_mesh_shaders, 0, 1);
	CMD3(CCC_Token, "r_debug_view", &ps_r_debug_view, debug_view_token);
	CMD4(CCC_Float, "r_lambert_factor", &ps_r_lambert_factor, 0.5f, 1.0f);
	CMD3(CCC_Token, "r_reflections_update_rate", &ps_r_reflections_update_rate, reflections_update_rate_token);
	CMD4(CCC_Float, "r_reflections_cull_reach", &ps_r_reflections_cull_reach, 0.0f, 4.0f);
	CMD4(CCC_Float, "r_reflections_lod_error", &ps_r_reflections_lod_error, 1.0f, 32.0f);

	CMD4(CCC_Float,		"r3_dynamic_wet_surfaces_near",	&ps_r3_dyn_wet_surf_near,	10,	70		);
	CMD4(CCC_Float,		"r3_dynamic_wet_surfaces_far",	&ps_r3_dyn_wet_surf_far,	30,	100		);
	CMD4(CCC_Integer,	"r3_dynamic_wet_surfaces_sm_res",&ps_r3_dyn_wet_surf_sm_res,64,	2048	);

	CMD4(CCC_Float, "r4_vslr_distance", &ps_r4_vslr_distance, 0.4f, 1.f);

	// IX-Ray
	CMD4(CCC_DetailRadius, "r_grass_distance", &ps_r__detail_radius, 50, 500);

	CMD2(CCC_DetailReloadDetails_Boolean, "r__detail_use_alternative_tree_assets", &ps_r__detail_use_alternative_tree_assets);
	CMD2(CCC_DetailReloadDetails_Boolean, "r__detail_use_cluster_mix_tree_assets", &ps_r__detail_use_cluster_mix_tree_assets);
	CMD4(CCC_DetailReloadDetails, "r__detail_cluster_seed", &ps_r__detail_cluster_seed, 0, 9999);
	CMD4(CCC_DetailReloadDetails, "r__detail_cluster_patch_size_min", &ps_r__detail_cluster_patch_size_min, 1.0f, 200.0f);
	CMD4(CCC_DetailReloadDetails, "r__detail_cluster_patch_size_max", &ps_r__detail_cluster_patch_size_max, 1.0f, 200.0f);
	CMD4(CCC_DetailReloadDetails, "r__detail_cluster_sharpness", &ps_r__detail_cluster_sharpness, 1.0f, 20.0f);
	CMD4(CCC_DetailReloadDetails, "r__detail_cluster_warp_min", &ps_r__detail_cluster_warp_min, 0.0f, 3.0f);
	CMD4(CCC_DetailReloadDetails, "r__detail_cluster_warp_max", &ps_r__detail_cluster_warp_max, 0.0f, 3.0f);


	CMD2(CCC_DetailReloadDetails_Boolean, "r__detail_fmb_use_layer_1", &ps_r__detail_fmb_use_layer_1);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_1_frequency", &ps_r__detail_fmb_layer_1_frequency, 0.0f, 1.0f);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_1_amplitude", &ps_r__detail_fmb_layer_1_amplitude, 0.0f, 10.0f);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_1_seed", &ps_r__detail_fmb_layer_1_seed, 0, 9999);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_1_power", &ps_r__detail_fmb_layer_1_power, 0.0f, 1.0f);

	CMD2(CCC_DetailReloadDetails_Boolean, "r__detail_fmb_use_layer_2", &ps_r__detail_fmb_use_layer_2);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_2_frequency", &ps_r__detail_fmb_layer_2_frequency, 0.0f, 1.0f);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_2_amplitude", &ps_r__detail_fmb_layer_2_amplitude, 0.0f, 10.0f);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_2_seed", &ps_r__detail_fmb_layer_2_seed, 0, 9999);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_2_power", &ps_r__detail_fmb_layer_2_power, 0.0f, 1.0f);

	CMD2(CCC_DetailReloadDetails_Boolean, "r__detail_fmb_use_layer_3", &ps_r__detail_fmb_use_layer_3);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_3_frequency", &ps_r__detail_fmb_layer_3_frequency, 0.0f, 1.0f);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_3_amplitude", &ps_r__detail_fmb_layer_3_amplitude, 0.0f, 10.0f);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_3_seed", &ps_r__detail_fmb_layer_3_seed, 0, 9999);
	CMD4(CCC_DetailReloadDetails, "r__detail_fmb_layer_3_power", &ps_r__detail_fmb_layer_3_power, 0.0f, 1.0f);

	CMD4(CCC_DetailReloadDetails, "r__detail_rnd_scale_min", &ps_r__detail_rnd_scale_min, 0.0f, 100.0f);
	CMD4(CCC_DetailReloadDetails, "r__detail_rnd_scale_max", &ps_r__detail_rnd_scale_max, 0.0f, 100.0f);

	CMD4(CCC_Float, "r__detail_trample_bend", &ps_trample_bend, 0.0f, 20.0f);
	CMD4(CCC_Float, "r__detail_trample_squash", &ps_trample_squash, 0.0f, 20.0f);
	CMD4(CCC_Float, "r__detail_trample_trail_min", &ps_trample_trail_min, 0.05f, 10.0f);
	CMD4(CCC_Float, "r__detail_trample_trail_max", &ps_trample_trail_max, 0.1f, 50.0f);
	CMD4(CCC_Float, "r__detail_trample_obj_radius_scale", &ps_trample_obj_radius_scale, 0.0f, 2.0f);
	CMD4(CCC_Float, "r__detail_trample_actor_radius_scale", &ps_trample_actor_radius_scale, 0.0f, 2.0f);
	CMD2(CCC_Boolean, "r__detail_trample_enabled", (bool*)&ps_trample_enabled);
	CMD4(CCC_Float, "r__detail_trample_cooltime", &ps_trample_cooltime, 0.5f, 600.0f);
	CMD4(CCC_Float, "r__detail_trample_draw_radius", &ps_trample_draw_radius, 25.0f, 200.0f);
	CMD4(CCC_Float, "r__detail_trample_brush_fill", &ps_trample_brush_fill, 0.1f, 1.0f);
	CMD4(CCC_Float, "r__detail_trample_press_speed", &ps_trample_press_speed, 0.1f, 50.0f);

	CMD2(CCC_Boolean, "r__detail_use_procedural_wind", (bool*)&ps_wind_enabled);
	CMD4(CCC_Integer, "r__detail_wind_mode", &ps_wind_mode, 0, 1);
	CMD4(CCC_Float, "r__detail_wind_blend", &ps_wind_blend, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_noise_scale", &ps_wind_noise_scale, 0.0f, 20.0f);
	CMD4(CCC_Float, "r__detail_wind_noise_speed", &ps_wind_noise_speed, 0.001f, 5.0f);
	CMD4(CCC_Float, "r__detail_wind_noise_angle", &ps_wind_noise_angle, 0.0f, 360.0f);
	CMD2(CCC_Boolean, "r__detail_wind_xz_enabled", (bool*)&ps_wind_xz_enabled);
	CMD2(CCC_Boolean, "r__detail_wind_swirl_enabled", (bool*)&ps_wind_swirl_enabled);
	CMD2(CCC_Boolean, "r__detail_wind_xz1_on", (bool*)&ps_wind_xz1_on);
	CMD4(CCC_Float, "r__detail_wind_xz1_scale_min", &ps_wind_xz1_scale_min, 0.0f, 2.0f);
	CMD4(CCC_Float, "r__detail_wind_xz1_scale_max", &ps_wind_xz1_scale_max, 0.0f, 2.0f);
	CMD4(CCC_Float, "r__detail_wind_xz1_int_min", &ps_wind_xz1_int_min, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz1_int_max", &ps_wind_xz1_int_max, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz1_con_min", &ps_wind_xz1_con_min, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz1_con_max", &ps_wind_xz1_con_max, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz1_spd_min", &ps_wind_xz1_spd_min, 0.0f, 5.0f);
	CMD4(CCC_Float, "r__detail_wind_xz1_spd_max", &ps_wind_xz1_spd_max, 0.0f, 5.0f);
	CMD4(CCC_Float, "r__detail_wind_xz1_ang_min", &ps_wind_xz1_ang_min, 0.0f, 360.0f);
	CMD4(CCC_Float, "r__detail_wind_xz1_ang_max", &ps_wind_xz1_ang_max, 0.0f, 360.0f);
	CMD2(CCC_Boolean, "r__detail_wind_xz2_on", (bool*)&ps_wind_xz2_on);
	CMD4(CCC_Float, "r__detail_wind_xz2_scale_min", &ps_wind_xz2_scale_min, 0.0f, 2.0f);
	CMD4(CCC_Float, "r__detail_wind_xz2_scale_max", &ps_wind_xz2_scale_max, 0.0f, 2.0f);
	CMD4(CCC_Float, "r__detail_wind_xz2_int_min", &ps_wind_xz2_int_min, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz2_int_max", &ps_wind_xz2_int_max, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz2_con_min", &ps_wind_xz2_con_min, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz2_con_max", &ps_wind_xz2_con_max, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz2_spd_min", &ps_wind_xz2_spd_min, 0.0f, 5.0f);
	CMD4(CCC_Float, "r__detail_wind_xz2_spd_max", &ps_wind_xz2_spd_max, 0.0f, 5.0f);
	CMD4(CCC_Float, "r__detail_wind_xz2_ang_min", &ps_wind_xz2_ang_min, 0.0f, 360.0f);
	CMD4(CCC_Float, "r__detail_wind_xz2_ang_max", &ps_wind_xz2_ang_max, 0.0f, 360.0f);
	CMD2(CCC_Boolean, "r__detail_wind_xz3_on", (bool*)&ps_wind_xz3_on);
	CMD4(CCC_Float, "r__detail_wind_xz3_scale_min", &ps_wind_xz3_scale_min, 0.0f, 2.0f);
	CMD4(CCC_Float, "r__detail_wind_xz3_scale_max", &ps_wind_xz3_scale_max, 0.0f, 2.0f);
	CMD4(CCC_Float, "r__detail_wind_xz3_int_min", &ps_wind_xz3_int_min, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz3_int_max", &ps_wind_xz3_int_max, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz3_con_min", &ps_wind_xz3_con_min, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz3_con_max", &ps_wind_xz3_con_max, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_xz3_spd_min", &ps_wind_xz3_spd_min, 0.0f, 5.0f);
	CMD4(CCC_Float, "r__detail_wind_xz3_spd_max", &ps_wind_xz3_spd_max, 0.0f, 5.0f);
	CMD4(CCC_Float, "r__detail_wind_xz3_ang_min", &ps_wind_xz3_ang_min, 0.0f, 360.0f);
	CMD4(CCC_Float, "r__detail_wind_xz3_ang_max", &ps_wind_xz3_ang_max, 0.0f, 360.0f);
	CMD4(CCC_Float, "r__detail_wind_sw_scale_min", &ps_wind_sw_scale_min, 0.0f, 2.0f);
	CMD4(CCC_Float, "r__detail_wind_sw_scale_max", &ps_wind_sw_scale_max, 0.0f, 2.0f);
	CMD4(CCC_Float, "r__detail_wind_sw_int_min", &ps_wind_sw_int_min, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_sw_int_max", &ps_wind_sw_int_max, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_sw_con_min", &ps_wind_sw_con_min, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_sw_con_max", &ps_wind_sw_con_max, 0.0f, 1.0f);
	CMD4(CCC_Float, "r__detail_wind_sw_spd_min", &ps_wind_sw_spd_min, 0.0f, 5.0f);
	CMD4(CCC_Float, "r__detail_wind_sw_spd_max", &ps_wind_sw_spd_max, 0.0f, 5.0f);
	CMD4(CCC_Float, "r__detail_wind_sw_ang_min", &ps_wind_sw_ang_min, 0.0f, 360.0f);
	CMD4(CCC_Float, "r__detail_wind_sw_ang_max", &ps_wind_sw_ang_max, 0.0f, 360.0f);

	CMD1(CCC_DetailLayersForceBake, "r__detail_bake_force");
	CMD1(CCC_DetailLayersForceLoad, "r__detail_bake_force_load");


	CMD3(CCC_Mask32, "r__no_ram_textures", &ps_r__common_flags, RFLAG_NO_RAM_TEXTURES);
	CMD3(CCC_Mask32, "r__mt_texture_load", &ps_r__common_flags, RFLAG_MT_TEX_LOAD);
	CMD3(CCC_Mask32, "r__mt_model_load", &ps_r__common_flags, RFLAG_MT_MODEL_LOAD);
	CMD4(CCC_Integer, "render.experemental.target_res", &g_TextureTargetResolution, 256, RHI_REQ_TEXTURE2D_U_OR_V_DIMENSION);
	CMD4(CCC_Integer, "r_texture_budget", &ps_r_texture_budget, 256, 16536);
	CMD3(CCC_Mask32, "r__deferred_texture_load", &ps_r__common_flags, RFLAG_DD_TEX_LOAD);
	CMD4(CCC_Integer, "r__optimize_static_geom", &opt_static, 0, 2);
	CMD4(CCC_Integer, "r__optimize_dynamic_geom", &opt_dynamic, 0, 2);
	CMD3(CCC_Mask32, "r__optimize_shadow_geom", &ps_r__common_flags, RFLAG_OPT_SHAD_GEOM);
	CMD3(CCC_Mask32, "r__shader_cache", &ps_r__common_flags, RFLAG_USE_CACHE);
	CMD1(CCC_RecompileShaders, "r_recompile_shaders");
	
	CMD3(CCC_Token, "r_screenshot_format", &ps_screenshot_format, screenshot_format_token);
	CMD4(CCC_Float, "r4_mblur_power", &ps_r4_mblur_power, 0.0f, 1.0f);
	CMD4(CCC_Float, "r_full_detail_distance_scale", &ps_r1_full_detail_distance_scale, 0.1f, 1.0f);

	CMD3(CCC_Mask32, "r_terrain_mask", &ps_r1_flags, R1FLAG_TERRAIN_MASK);

	CMD4(CCC_Float, "r2_aref_quality", &ps_r2_def_aref_quality, 70.0f, 200.0f);
	CMD3(CCC_Mask32, "r2_use_bump", &ps_r__common_flags, R2FLAG_USE_BUMP);
	CMD3(CCC_Token, "r2_smap_size", &ps_r2_smapsize, qsmapsize_token);
	CMD3(CCC_Token, "r4_sun_contact_quality", &g_SunContactQuality, qsun_shafts_token);
	CMD3(CCC_Mask32, "r4_translucent_shadows", &ps_r2_ls_flags_ext, R4FLAG_TSHDOWS);
	CMD4(CCC_Float, "r4_rain_refraction", &g_RainRefraction, 0.0f, 4.0f);
	CMD4(CCC_Float, "r4_rain_width_scale", &g_RainWidthScale, 0.01f, 1.0f);

	CMD3(CCC_Token, "r2_rain_drops_mode", &g_RainDropsMode, RainDropsModeTokens);
	CMD3(CCC_Token, "r_rain_shading", &g_RainShading, RainShadingTokens);
	CMD4(CCC_Float, "r_rain_drops_opacity", &g_RainDropsOpacity, 0.f, 8.f);
	CMD4(CCC_Float, "r_rain_drops_color_blend", &g_RainDropsColorBlend, 0.f, 1.f);
	CMD4(CCC_Float, "r_rain_drops_radius", &g_RainDropsRadius, 0.01f, 2.f);
	CMD4(CCC_Float, "r_rain_drops_impact_radius", &g_RainDropsImpactRadius, 0.01f, 2.f);
	CMD4(CCC_Float, "r_rain_drops_near", &g_RainDropsNear, 0.f, 10.f);
	CMD4(CCC_Float, "r_rain_drops_refraction", &g_RainDropsRefraction, 0.f, 32.f);
	CMD4(CCC_Float, "r_rain_drops_transmission", &g_RainDropsTransmission, 0.f, 1.f);
	CMD4(CCC_Float, "r_rain_drops_feather", &g_RainDropsFeather, 0.f, 1.f);

	CMD4(CCC_Float, "r_exposure_bias", &ps_r2_autoexposure_bias, -10.0f, 10.0f);
	CMD4(CCC_Float, "r_exposure_speed", &ps_r2_autoexposure_speed, 0.0f, 100.0f);
	CMD4(CCC_Float, "r_tonemap_white", &ps_r2_tonemap_white, 1.0f, 16.0f);
	CMD4(CCC_Float, "r_tonemap_compression", &ps_r2_tonemap_compression, 0.0f, 10.0f);
	CMD4(CCC_Float, "r_tonemap_desaturation", &ps_r2_tonemap_desaturation, 0.0f, 1.0f);
	CMD4(CCC_Float, "r_bloom_amount", &ps_r2_bloom_amount, 0.0f, 3.0f);
	CMD4(CCC_Float, "r_saturation", &ps_r2_saturation, 0.5f, 2.0f);
	CMD4(CCC_Float, "r_sharpening", &ps_r4_cas_sharpening, 0.0f, 1.0f);

#ifdef DEBUG_DRAW
	// Until the next change of a video setting, which sets it by r_distance_quality
	CMD4(CCC_Float, "r_lod_error", &ps_r_lod_error, 0.0f, 16.0f);
	CMD4(CCC_Integer, "r__lsleep_frames", &ps_r__LightSleepFrames, 4, 30);
	CMD4(CCC_Float, "r__ssa_glod_start", &ps_r__GLOD_ssa_start, 128, 512);
	CMD4(CCC_Float, "r__ssa_glod_end", &ps_r__GLOD_ssa_end, 16, 96);
	CMD4(CCC_Float, "r__wallmark_shift_pp", &ps_r__WallmarkSHIFT, 0.0f, 1.f);
	CMD4(CCC_Float, "r__wallmark_shift_v", &ps_r__WallmarkSHIFT_V, 0.0f, 1.f);
	CMD2(CCC_Boolean, "r__wallmark_dyn", &ps_r__WallmarkDyn);

	CMD3(CCC_Mask32, "r4_wireframe", &ps_r2_ls_flags_ext, R2FLAGEXT_WIREFRAME);//Need restart
	CMD2(CCC_R2GM, "r2em", &ps_r2_gmaterial);

	CMD2(CCC_Boolean, "ui_dbg_graphic", &Engine.External.EditorStates[(int)EditorUI::Shaders]);
	CMD1(CCC_DumpResources, "dump_resources");

	CMD2(CCC_Boolean, "r_particles_real_dt", &ps_r2_particle_dt);
	tw_min.set(-10, -10, -EPS_S);	tw_max.set(10, 10, EPS_S);
	CMD4(CCC_Vector3, "r_taa_jitter_scale", &ps_r_taa_jitter_scale, tw_min, tw_max);

	// test
	CMD4(CCC_Float, "r_developer_float_1", &ps_r__test_exp_to_shaders_1, -10000000.0f, 10000000.0f);
	CMD4(CCC_Float, "r_developer_float_2", &ps_r__test_exp_to_shaders_2, -10000000.0f, 10000000.0f);
	CMD4(CCC_Float, "r_developer_float_3", &ps_r__test_exp_to_shaders_3, -10000000.0f, 10000000.0f);
	CMD4(CCC_Float, "r_developer_float_4", &ps_r__test_exp_to_shaders_4, -10000000.0f, 10000000.0f);
	CMD1(CCC_memory_stats, "render_memory_stats");

	CMD4(CCC_Integer, "r2_wait_sleep", &ps_r2_wait_sleep, 0, 1);
	CMD4(CCC_Integer, "r2_dhemi_count", &ps_r2_dhemi_count, 4, 25);
	CMD4(CCC_Float, "r2_dhemi_sky_scale", &ps_r2_dhemi_sky_scale, 0.0f, 100.f);
	CMD4(CCC_Float, "r2_dhemi_smooth", &ps_r2_lt_smooth, 0.f, 10.f);
	CMD3(CCC_Mask32, "rs_hom_depth_draw", &ps_r2_ls_flags_ext, R_FLAGEXT_HOM_DEPTH_DRAW);
	CMD3(CCC_Mask32, "r2_shadow_cascede_zcul", &ps_r2_ls_flags_ext, R2FLAGEXT_SUN_ZCULLING);
	CMD3(CCC_Mask32, "r2_exp_donttest_uns", &ps_r2_ls_flags, R2FLAG_EXP_DONT_TEST_UNSHADOWED);

	CMD4(CCC_Integer, "rs_dbg_draw_depth", &r_debug_render_depth, 0, 1);
#endif
}

void xrRender_apply_tf()
{
	if (GRHI->DevicePtr)
	{
		SSManager.SetMaxAnisotropy(ps_r__tf_Anisotropic);
	}

	Console->Execute("r__tf_mipbias");
}

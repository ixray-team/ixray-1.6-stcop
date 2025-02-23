#include	"stdafx.h"
#pragma		hdrstop

#include	"xrRender_console.h"
#include	"dxRenderDeviceRender.h"

u32			ps_Preset				=	2	;
xr_token							qpreset_token							[ ]={
	{ "Minimum",					0											},
	{ "Low",						1											},
	{ "Default",					2											},
	{ "High",						3											},
	{ "Extreme",					4											},
	{ "Ultra",						5											},
	{ 0,							0											}
};

u32 ps_r2_smapsize = 2048;
xr_token qsmapsize_token[] = {
	{ "1024", 1024 },
	{ "2048", 2048 },
	{ "3072", 3072 },
	{ "4096", 4096 },
	{ nullptr, 0   }
};

u32			ps_r_ssao_mode			=	2;
xr_token							qssao_mode_token						[ ]={
	{ "st_opt_off",					0											},
	{ "ui_mm_ssao",					1											},
#ifdef USE_DX11
	{ "ui_mm_gtao",					2											},
#endif
	{ 0,							0											}
};

u32			ps_r_sun_shafts				=	2;
xr_token							qsun_shafts_token							[ ]={
	{ "st_opt_off",					0												},
	{ "st_opt_low",					1												},
	{ "st_opt_medium",				2												},
	{ "st_opt_high",				3												},
	{ 0,							0												}
};

u32			ps_r_sun_quality		=	1;			//	=	0;
xr_token							qsun_quality_token							[ ]={
	{ "st_opt_low",					0												},
	{ "st_opt_medium",				1												},
	{ "st_opt_high",				2												},
#ifdef USE_DX11
	{ "st_opt_ultra",				3												},
	{ "st_opt_extreme",				4												},
#endif //USE_DX11
	{ 0,							0												}
};

u32			ps_r2_aa_type			= 0;			//	=	0;
xr_token							aa_type_token[] = {
	{ "st_opt_off",						0												},
	{ "fxaa",						1												},
#if RENDER != R_R1
	{ "smaa",						2												},
#endif // DEBUG
	{ 0,							0												}
};

u32			ps_screenshot_format = 0;			//	=	0;
xr_token							screenshot_format_token[] = {
	{ "ss_jpg",						0												},
	{ "ss_tga",						1												},
	{ "ss_png",						2												},
	{ 0,							0												}
};

// Common
extern int			psSkeletonUpdate;
extern float		r__dtex_range;

int			ps_r__LightSleepFrames		= 10	;

float		ps_r__Detail_l_ambient		= 0.9f	;
float		ps_r__Detail_l_aniso		= 0.25f	;
float		ps_r__Detail_density		= 0.3f	;
float		ps_r__Detail_rainbow_hemi	= 0.75f	;

float		ps_r__Tree_SBC				= 1.5f	;	// scale bias correct

float		ps_r__WallmarkTTL			= 50.f	;
float		ps_r__WallmarkSHIFT			= 0.0001f;
float		ps_r__WallmarkSHIFT_V		= 0.0001f;

float		ps_r__GLOD_ssa_start		= 256.f	;
float		ps_r__GLOD_ssa_end			=  64.f	;
float		ps_r__LOD					=  0.75f	;
float		ps_r__ssaDISCARD			=  3.5f	;					//RO
float		ps_r__ssaDONTSORT			=  32.f	;					//RO
float		ps_r__ssaHZBvsTEX			=  96.f	;					//RO

int			ps_r__tf_Anisotropic		= 16		;

// R1
float		ps_r1_ssaLOD_A				= 64.f	;
float		ps_r1_ssaLOD_B				= 48.f	;
float ps_r__tf_Mipbias = -0.1f;
Flags32		ps_r1_flags					= { R1FLAG_DLIGHTS | R1FLAG_TERRAIN_MASK };		// r1-only
float		ps_r1_lmodel_lerp			= 0.1f	;
float		ps_r1_dlights_clip			= 50.f	;
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
Fvector3	ps_r_taa_jitter_scale = { 1,1,0 };

// R2-specific
Flags32		ps_r2_ls_flags = {
	R2FLAG_SUN
	| R2FLAG_EXP_DONT_TEST_UNSHADOWED
	| R2FLAG_USE_NVSTENCIL | R2FLAG_EXP_SPLIT_SCENE
	| R2FLAG_EXP_MT_CALC | R3FLAG_DYN_WET_SURF
	| R3FLAG_VOLUMETRIC_SMOKE
	| R2FLAG_DETAIL_BUMP
	| R2FLAG_SOFT_PARTICLES
	| R2FLAG_SOFT_WATER
	| R2FLAG_STEEP_PARALLAX
	| R2FLAG_TONEMAP
	| R2FLAG_VOLUMETRIC_LIGHTS
};	// r2-only

Flags32 ps_r2_ls_flags_ext =
{
	RFLAG_CLOUD_SHADOWS |
	R4FLAG_SCREEN_SPACE_HUD_SHADOWS |
	R4FLAG_HASHED_ALPHA_TEST
};

Flags32 ps_r__common_flags = { R2FLAG_USE_BUMP | RFLAG_USE_CACHE | RFLAG_NO_RAM_TEXTURES | RFLAG_MT_TEX_LOAD };

int opt_static = 0;
int opt_dynamic = 0;

float		ps_r2_df_parallax_h			= 0.02f;
float		ps_r2_df_parallax_range		= 60.f;
float		ps_r2_tonemap_middlegray	= 1.f;			// r2-only
float		ps_r2_tonemap_adaptation	= 3.f;				// r2-only
float		ps_r2_tonemap_low_lum		= 0.01f;			// r2-only
float		ps_r2_tonemap_amount		= 0.7f;				// r2-only
float		ps_r2_ls_bloom_kernel_g		= 5.f;				// r2-only
float		ps_r2_ls_bloom_kernel_b		= .7f;				// r2-only
float		ps_r2_ls_bloom_speed		= 100.f;				// r2-only
float		ps_r2_ls_bloom_kernel_scale	= .7f;				// r2-only	// gauss
float		ps_r2_ls_bloom_threshold	= 0.1f;				// r2-only
float		ps_r2_ls_depth_scale = 0.9999f; // 1.00001f
float		ps_r2_ls_depth_bias = 0.00001f; // -0.0001f
float		ps_r2_ls_squality			= 1.0f;				// 1.00f

float		ps_r2_sun_bias				= -0.01f;			// 
float		ps_r2_sun_far				= 160.f;
float		ps_r2_sun_near				= 20.f;
float		ps_r2_sun_depth_far_scale	= 1.00000f;			// 1.00001f
float		ps_r2_sun_depth_near_scale	= 1.0000f;			// 1.00001f
float		ps_r2_sun_lumscale			= 1.1f;				// 1.0f
float		ps_r2_sun_lumscale_hemi		= 0.95f;				// 1.0f
float		ps_r2_sun_lumscale_amb		= 0.6f;
float		ps_r2_sun_lumscale_sky		= 1.2f;
float		ps_r2_gmaterial				= 2.2f;				// 
float		ps_r2_zfill					= 0.25f;				// .1f

float		ps_r2_dhemi_sky_scale		= 0.08f;				// 1.5f
float		ps_r2_dhemi_light_scale     = 0.2f	;
float		ps_r2_dhemi_light_flow      = 0.1f	;
int			ps_r2_dhemi_count			= 5;				// 5
int			ps_r2_wait_sleep			= 0;

float		ps_r2_lt_smooth				= 1.f;				// 1.f
float		ps_r2_slight_fade			= 0.6f;				// 1.f

//	x - min (0), y - focus (1.4), z - max (100)
Fvector3	ps_r2_dof					= Fvector3().set(-1.25f, 1.4f, 600.f);
float		ps_r2_dof_sky				= 30;				//	distance to sky
float		ps_r2_dof_kernel_size		= 5.0f;						//	7.0f

float		ps_r2_def_aref_quality = 115.0f;

float		ps_r3_dyn_wet_surf_near		= 10.f;				// 10.0f
float		ps_r3_dyn_wet_surf_far		= 30.f;				// 30.0f
int			ps_r3_dyn_wet_surf_sm_res	= 256;				// 256
float		ps_r2_gloss_factor = 3.14f;

int			ps_r__detail_radius = 49;
float		ps_r4_cas_sharpening = 0.0f;

// Test float exported to shaders for development
float		ps_r__test_exp_to_shaders_1	= 1.0f;
float		ps_r__test_exp_to_shaders_2	= 1.0f;
float		ps_r__test_exp_to_shaders_3	= 1.0f;
float		ps_r__test_exp_to_shaders_4	= 1.0f;

BOOL		ps_r2_particle_dt			= FALSE;

#ifndef _EDITOR
#include "../../xrEngine/XR_IOConsole.h"
#include	"../../xrEngine/xr_ioc_cmd.h"

#ifdef USE_DX11
#include "../xrRenderDX10/StateManager/dx10SamplerStateCache.h"
#endif //USE_DX11

//-----------------------------------------------------------------------
class CCC_tf_Aniso		: public CCC_Integer
{
public:
	void	apply	()	{
		if (0==RDevice)	return	;
		int	val = *value;	clamp(val,1,16);
#ifdef USE_DX11
		SSManager.SetMaxAnisotropy(val);
#else //USE_DX11
		for (u32 i=0; i<Caps.raster.dwStages; i++)
			CHK_DX(RDevice->SetSamplerState( i, D3DSAMP_MAXANISOTROPY, val	));
#endif //USE_DX11
	}
	CCC_tf_Aniso(LPCSTR N, int*	v) : CCC_Integer(N, v, 1, 16)		{ };
	virtual void Execute	(LPCSTR args)
	{
		CCC_Integer::Execute	(args);
		apply					();
	}
	virtual void	Status	(TStatus& S)
	{	
		CCC_Integer::Status		(S);
		apply					();
	}
};

class CCC_tf_MipBias: public CCC_Float {
public:
	CCC_tf_MipBias(LPCSTR N, float* v) : CCC_Float(N, v, -3.0f, 3.0f) {};
	void apply() {
		if (0 == RDevice) {
			return;
		}
		float val = *value;
		clamp(val, -3.0f, 3.0f);
#ifdef USE_DX11
		SSManager.SetMipLodBias(val);
#else //USE_DX11
		for (u32 i = 0; i < Caps.raster.dwStages; i++) {
			CHK_DX(RDevice->SetSamplerState(i, D3DSAMP_MIPMAPLODBIAS, val));
		}
#endif
	}
	virtual void Execute(LPCSTR args) {
		CCC_Float::Execute(args);
		apply();
	}

	virtual void Status(TStatus& S) {
		CCC_Float::Status(S);
		apply();
	}
};

class CCC_R2GM		: public CCC_Float
{
public:
	CCC_R2GM(LPCSTR N, float*	v) : CCC_Float(N, v, 0.f, 4.f) { *v = 0; };
	virtual void	Execute	(LPCSTR args)
	{
		if (0==xr_strcmp(args,"on"))	{
			ps_r2_ls_flags.set	(R2FLAG_GLOBALMATERIAL,TRUE);
		} else if (0==xr_strcmp(args,"off"))	{
			ps_r2_ls_flags.set	(R2FLAG_GLOBALMATERIAL,FALSE);
		} else {
			CCC_Float::Execute	(args);
			if (ps_r2_ls_flags.test(R2FLAG_GLOBALMATERIAL))	{
				static LPCSTR	name[4]	=	{ "oren", "blin", "phong", "metal" };
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
	CCC_Screenshot(LPCSTR N) : IConsole_Command(N)  { };
	virtual void Execute(LPCSTR args) {
		if (g_dedicated_server)
			return;

		string_path	name;	name[0]=0;
		sscanf		(args,"%s",	name);
		LPCSTR		image	= xr_strlen(name)?name:0;
		::Render->Screenshot(IRender_interface::SM_NORMAL,image);
	}
};

class CCC_RestoreQuadIBData : public IConsole_Command
{
public:
	CCC_RestoreQuadIBData(LPCSTR N) : IConsole_Command(N)  { };
	virtual void Execute(LPCSTR args) {
		RCache.RestoreQuadIBData();
	}
};

class CCC_ModelPoolStat : public IConsole_Command
{
public:
	CCC_ModelPoolStat(LPCSTR N) : IConsole_Command(N)  { bEmptyArgsHandled = TRUE; };
	virtual void Execute(LPCSTR args) {
		RImplementation.Models->dump();
	}
};

//-----------------------------------------------------------------------
class	CCC_Preset		: public CCC_Token
{
public:
	CCC_Preset(LPCSTR N, u32* V, xr_token* T) : CCC_Token(N,V,T)	{}	;

	virtual void	Execute	(LPCSTR args)	{
		CCC_Token::Execute	(args);
		string_path		_cfg;
		string_path		cmd;

		auto setConfig = [&](const char* basePath)
		{
			xr_string path = xr_string("ixray_settings\\") + basePath;
			if (FS.exist("$game_config$", path.c_str()))
			{
				xr_strcpy(_cfg, path.c_str());
			}
			else
			{
				xr_strcpy(_cfg, basePath);
			}
		};

		switch	(*value)	{
			case 0: setConfig("rspec_minimum.ltx"); break;
			case 1: setConfig("rspec_low.ltx"); break;
			case 2: setConfig("rspec_default.ltx"); break;
			case 3: setConfig("rspec_high.ltx"); break;
			case 4: setConfig("rspec_extreme.ltx"); break;
			case 5: setConfig("rspec_ultra.ltx"); break;
		}
		FS.update_path			(_cfg,"$game_config$",_cfg);
		xr_strconcat(cmd,"cfg_load", " ", _cfg);
		Console->Execute		(cmd);
	}
};

#ifdef USE_DX11
class CCC_RenderDocCaptureStart : public IConsole_Command {
public:
	CCC_RenderDocCaptureStart(LPCSTR N) : IConsole_Command(N) {
		bEmptyArgsHandled = true;
	};

	virtual void Execute(LPCSTR args) {
		if (Device.GetRenderDocAPI()) {
			HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(g_AppInfo.Window), "SDL.window.win32.hwnd", nullptr);
			Device.GetRenderDocAPI()->StartFrameCapture(RDevice, hwnd);
		}
	}
};

class CCC_RenderDocCaptureEnd : public IConsole_Command {
public:
	CCC_RenderDocCaptureEnd(LPCSTR N) : IConsole_Command(N) {
		bEmptyArgsHandled = true;
	};

	virtual void Execute(LPCSTR args) {
		if (Device.GetRenderDocAPI()) {
			HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(g_AppInfo.Window), "SDL.window.win32.hwnd", nullptr);
			Device.GetRenderDocAPI()->EndFrameCapture(RDevice, hwnd);
		}
	}
};
#endif

class CCC_memory_stats : public IConsole_Command
{
protected	:

public		:

	CCC_memory_stats(LPCSTR N) :	IConsole_Command(N)	{ bEmptyArgsHandled = true; };

	virtual void	Execute	(LPCSTR args)
	{
		u32 m_base = 0;
		u32 c_base = 0;
		u32 m_lmaps = 0; 
		u32 c_lmaps = 0;

		dxRenderDeviceRender::Instance().ResourcesGetMemoryUsage( m_base, c_base, m_lmaps, c_lmaps );
	}

};


#if RENDER!=R_R1
#include "r__pixel_calculator.h"
class CCC_BuildSSA : public IConsole_Command
{
public:
	CCC_BuildSSA(LPCSTR N) : IConsole_Command(N)  { bEmptyArgsHandled = TRUE; };
	virtual void Execute(LPCSTR args) 
	{
#ifndef USE_DX11
		//	TODO: DX10: Implement pixel calculator
		r_pixel_calculator	c;
		c.run				();
#endif //USE_DX11
	}
};
#endif

class CCC_DofFar : public CCC_Float
{
public:
	CCC_DofFar(LPCSTR N, float* V, float _min=0.0f, float _max=10000.0f) 
		: CCC_Float( N, V, _min, _max){}

	virtual void Execute(LPCSTR args) 
	{
		float v = float(atof(args));

		if (v<ps_r2_dof.y+0.1f)
		{
			char	pBuf[256];
			_snprintf_s(pBuf, sizeof(pBuf) / sizeof(pBuf[0]), "float value greater or equal to r2_dof_focus+0.1");
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
	virtual void	Save	(IWriter *F)	{;}
};

class CCC_DofNear : public CCC_Float
{
public:
	CCC_DofNear(LPCSTR N, float* V, float _min=0.0f, float _max=10000.0f) 
		: CCC_Float( N, V, _min, _max){}

	virtual void Execute(LPCSTR args) 
	{
		float v = float(atof(args));

		if (v>ps_r2_dof.y-0.1f)
		{
			char	pBuf[256];
			_snprintf_s(pBuf, sizeof(pBuf) / sizeof(pBuf[0]), "float value less or equal to r2_dof_focus-0.1");
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
	virtual void	Save	(IWriter *F)	{;}
};

class CCC_DofFocus : public CCC_Float
{
public:
	CCC_DofFocus(LPCSTR N, float* V, float _min=0.0f, float _max=10000.0f) 
		: CCC_Float( N, V, _min, _max){}

	virtual void Execute(LPCSTR args) 
	{
		float v = float(atof(args));

		if (v>ps_r2_dof.z-0.1f)
		{
			char	pBuf[256];
			_snprintf_s(pBuf, sizeof(pBuf) / sizeof(pBuf[0]), "float value less or equal to r2_dof_far-0.1");
			Msg("~ Invalid syntax in call to '%s'",cName);
			Msg("~ Valid arguments: %s", pBuf);
			Console->Execute("r2_dof_far");
		}
		else if (v<ps_r2_dof.x+0.1f)
		{
			char	pBuf[256];
			_snprintf_s(pBuf, sizeof(pBuf) / sizeof(pBuf[0]), "float value greater or equal to r2_dof_far-0.1");
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
	virtual void	Save	(IWriter *F)	{;}
};

class CCC_Dof : public CCC_Vector3
{
public:
	CCC_Dof(LPCSTR N, Fvector* V, const Fvector _min, const Fvector _max) : 
	  CCC_Vector3(N, V, _min, _max) {;}

	virtual void	Execute	(LPCSTR args)
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
	virtual void	Status	(TStatus& S)
	{	
		xr_sprintf	(S,"%f,%f,%f",value->x,value->y,value->z);
	}
	virtual void	Info	(TInfo& I)
	{	
		xr_sprintf(I,"vector3 in range [%f,%f,%f]-[%f,%f,%f]",min.x,min.y,min.z,max.x,max.y,max.z);
	}

};

class CCC_DumpResources : public IConsole_Command
{
public:
	CCC_DumpResources(LPCSTR N) : IConsole_Command(N) { bEmptyArgsHandled = TRUE; };
	virtual void Execute(LPCSTR args) {
		RImplementation.Models->dump();
		dxRenderDeviceRender::Instance().Resources->Dump(false);
	}
};

//	Allow real-time fog config reload
#if defined(USE_DX11) && defined(DEBUG_DRAW)
#include "../xrRenderDX10/3DFluid/dx103DFluidManager.h"

class CCC_Fog_Reload : public IConsole_Command
{
public:
	CCC_Fog_Reload(LPCSTR N) : IConsole_Command(N) { bEmptyArgsHandled = TRUE; };
	virtual void Execute(LPCSTR args) 
	{
		FluidManager.UpdateProfiles();
	}
};
#endif

class CCC_DetailRadius : public CCC_Integer
{
public:
	CCC_DetailRadius(LPCSTR N, int* V, int _min = 0, int _max = 999) : CCC_Integer(N, V, _min, _max)
	{
	};
	
	virtual void Execute(LPCSTR args) {
		CCC_Integer::Execute(args);

		dm_current_size				= iFloor((float)ps_r__detail_radius/4)*2;
		dm_current_cache1_line		= dm_current_size*2/4;		// assuming cache1_count = 4
		dm_current_cache_line		= dm_current_size+1+dm_current_size;
		dm_current_cache_size		= dm_current_cache_line*dm_current_cache_line;
		dm_current_fade				= float(2*dm_current_size)-.5f;

		if (RImplementation.b_loaded && (dm_current_size != dm_size))
		{
			RImplementation.Details->MT_CALC.cancel();

			RImplementation.Details->cache_task.clear();

			RImplementation.Details->cache_Free();
			RImplementation.Details->cache_Alloc();
			RImplementation.Details->cache_Initialize();
		}
	}
	
	virtual void Status(TStatus& S) {
		CCC_Integer::Status(S);
	}
};

//-----------------------------------------------------------------------
void		xrRender_initconsole	()
{
	CMD3(CCC_Preset,	"_preset",				&ps_Preset,	qpreset_token	);

	CMD4(CCC_Integer,	"rs_skeleton_update",	&psSkeletonUpdate,	2,		128	);
	CMD4(CCC_Float,		"r__dtex_range",		&r__dtex_range,		5,		175	);

// Common
	CMD1(CCC_Screenshot,"screenshot"			);
	CMD4(CCC_Float, "r__wallmark_ttl", &ps_r__WallmarkTTL, 1.0f, 10.f * 60.f);

	CMD4(CCC_Float,		"r__geometry_lod",		&ps_r__LOD,					0.1f,	1.2f		);
	CMD4(CCC_Float,		"r__detail_density",	&ps_current_detail_density,		0.1f,	0.8f	);

#ifdef DEBUG
	CMD4(CCC_Float,		"r__detail_l_ambient",	&ps_r__Detail_l_ambient,	.5f,	.95f	);
	CMD4(CCC_Float,		"r__detail_l_aniso",	&ps_r__Detail_l_aniso,		.1f,	.5f		);
#endif // DEBUG

	CMD2(CCC_tf_Aniso, "r__tf_aniso", &ps_r__tf_Anisotropic); //	{1..16}
	CMD2(CCC_tf_MipBias, "r__tf_mipbias", &ps_r__tf_Mipbias);//	{-3 +3}

	// R1
	CMD4(CCC_Float,		"r1_ssa_lod_a",			&ps_r1_ssaLOD_A,			16,		96		);
	CMD4(CCC_Float,		"r1_ssa_lod_b",			&ps_r1_ssaLOD_B,			16,		64		);
	CMD4(CCC_Float,		"r1_lmodel_lerp",		&ps_r1_lmodel_lerp,			0,		0.333f	);
	CMD3(CCC_Mask,		"r1_dlights",			&ps_r1_flags,				R1FLAG_DLIGHTS	);
	CMD4(CCC_Float,		"r1_dlights_clip",		&ps_r1_dlights_clip,		10.f,	150.f	);
	CMD4(CCC_Float,		"r1_pps_u",				&ps_r1_pps_u,				-1.f,	+1.f	);
	CMD4(CCC_Float,		"r1_pps_v",				&ps_r1_pps_v,				-1.f,	+1.f	);

	// R1-specific
	CMD4(CCC_Integer,	"r1_glows_per_frame",	&ps_r1_GlowsPerFrame,		2,		32		);
	CMD3(CCC_Mask,		"r1_detail_textures",	&ps_r2_ls_flags,			R1FLAG_DETAIL_TEXTURES);

	CMD4(CCC_Float,		"r1_fog_luminance",		&ps_r1_fog_luminance,		0.2f,	5.f	);

	// R2
	CMD4(CCC_Float,		"r2_ssa_lod_a",			&ps_r2_ssaLOD_A,			16,		96		);
	CMD4(CCC_Float,		"r2_ssa_lod_b",			&ps_r2_ssaLOD_B,			32,		64		);

	// R2-specific
	CMD3(CCC_Mask,		"r2_tonemap",			&ps_r2_ls_flags,			R2FLAG_TONEMAP	);
	CMD4(CCC_Float,		"r2_tonemap_middlegray",&ps_r2_tonemap_middlegray,	0.0f,	2.0f	);
	CMD4(CCC_Float,		"r2_tonemap_adaptation",&ps_r2_tonemap_adaptation,	0.01f,	10.0f	);
	CMD4(CCC_Float,		"r2_tonemap_lowlum",	&ps_r2_tonemap_low_lum,		0.0001f,1.0f	);
	CMD4(CCC_Float,		"r2_tonemap_amount",	&ps_r2_tonemap_amount,		0.0000f,1.0f	);

	CMD4(CCC_Float,		"r2_ls_bloom_kernel_scale",&ps_r2_ls_bloom_kernel_scale,	0.5f,	2.f);
	CMD4(CCC_Float,		"r2_ls_bloom_kernel_g",	&ps_r2_ls_bloom_kernel_g,	1.f,	7.f		);
	CMD4(CCC_Float,		"r2_ls_bloom_kernel_b",	&ps_r2_ls_bloom_kernel_b,	0.01f,	1.f		);
	CMD4(CCC_Float,		"r2_ls_bloom_threshold",&ps_r2_ls_bloom_threshold,	0.f,	1.f		);
	CMD4(CCC_Float,		"r2_ls_bloom_speed",	&ps_r2_ls_bloom_speed,		0.f,	100.f	);
	CMD3(CCC_Mask,		"r2_ls_bloom_fast",		&ps_r2_ls_flags,			R2FLAG_FASTBLOOM);
	CMD4(CCC_Float,		"r2_ls_squality",		&ps_r2_ls_squality,			.5f,	1.f		);

	CMD3(CCC_Mask,		"r2_zfill",				&ps_r2_ls_flags,			R2FLAG_ZFILL	);
	CMD4(CCC_Float,		"r2_zfill_depth",		&ps_r2_zfill,				.001f,	.5f		);
	CMD3(CCC_Mask,		"r2_allow_r1_lights",	&ps_r2_ls_flags,			R2FLAG_R1LIGHTS	);

	CMD4(CCC_Float,		"r2_gloss_factor",		&ps_r2_gloss_factor,		.0f,	10.f	);

#ifdef DEBUG
	CMD3(CCC_Mask,		"r2_use_nvdbt",			&ps_r2_ls_flags,			R2FLAG_USE_NVDBT);
	CMD3(CCC_Mask,		"r2_mt",				&ps_r2_ls_flags,			R2FLAG_EXP_MT_CALC);
#endif // DEBUG

	CMD3(CCC_Mask,		"r2_sun",				&ps_r2_ls_flags,			R2FLAG_SUN		);
	CMD3(CCC_Mask,		"r2_sun_details",		&ps_r2_ls_flags,			R2FLAG_SUN_DETAILS);
	CMD3(CCC_Mask,		"r2_lights_details",	&ps_r2_ls_flags,			R2FLAG_LIGHTS_DETAILS);
	CMD3(CCC_Mask,		"r2_exp_donttest_shad",	&ps_r2_ls_flags,			R2FLAG_EXP_DONT_TEST_SHADOWED);
	
	CMD4(CCC_Float,		"r2_sun_bias",			&ps_r2_sun_bias,			-0.5,	+0.5	);
	CMD4(CCC_Float,		"r2_sun_near",			&ps_r2_sun_near,			1.f,	50.f	);
	CMD4(CCC_Float,		"r2_sun_far",			&ps_r2_sun_far,				51.f,	180.f	);
	CMD4(CCC_Float,		"r2_sun_depth_far_scale",&ps_r2_sun_depth_far_scale,0.5,	1.5		);
	CMD4(CCC_Float,		"r2_sun_depth_near_scale",&ps_r2_sun_depth_near_scale,0.5,	1.5		);

	CMD4(CCC_Float,		"r2_sun_lumscale",		&ps_r2_sun_lumscale,		-1.0,	+3.0	);
	CMD4(CCC_Float,		"r2_sun_lumscale_hemi",	&ps_r2_sun_lumscale_hemi,	0.0,	+3.0	);
	CMD4(CCC_Float,		"r2_sun_lumscale_amb",	&ps_r2_sun_lumscale_amb,	0.0,	+3.0	);
	CMD4(CCC_Float,		"r2_sun_lumscale_sky",	&ps_r2_sun_lumscale_sky,	0.0,	+3.0	);

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
	CMD4(CCC_Float,		"r2_dof_kernel",&ps_r2_dof_kernel_size,				.0f,	10.f);
	CMD4(CCC_Float,		"r2_dof_sky",	&ps_r2_dof_sky,						-10000.f,	10000.f);
	CMD3(CCC_Mask,		"r2_dof_enable",&ps_r2_ls_flags,	R2FLAG_DOF);

	CMD3(CCC_Mask,		"r2_volumetric_lights",			&ps_r2_ls_flags,			R2FLAG_VOLUMETRIC_LIGHTS);
	CMD3(CCC_Token,		"r2_sun_shafts",				&ps_r_sun_shafts,			qsun_shafts_token);
	CMD3(CCC_Token,		"r2_ssao_mode",					&ps_r_ssao_mode,			qssao_mode_token);

	CMD3(CCC_Mask,		"r2_steep_parallax",			&ps_r2_ls_flags,			R2FLAG_STEEP_PARALLAX);
	CMD3(CCC_Mask,		"r2_detail_bump",				&ps_r2_ls_flags,			R2FLAG_DETAIL_BUMP);

	CMD3(CCC_Token,		"r2_sun_quality",				&ps_r_sun_quality,			qsun_quality_token);

	//	Igor: need restart
	CMD3(CCC_Mask,		"r2_soft_water",				&ps_r2_ls_flags,			R2FLAG_SOFT_WATER);
	CMD3(CCC_Mask,		"r2_soft_particles",			&ps_r2_ls_flags,			R2FLAG_SOFT_PARTICLES);

	CMD3(CCC_Mask,		"r3_dynamic_wet_surfaces",		&ps_r2_ls_flags,			R3FLAG_DYN_WET_SURF);
	CMD4(CCC_Float,		"r3_dynamic_wet_surfaces_near",	&ps_r3_dyn_wet_surf_near,	10,	70		);
	CMD4(CCC_Float,		"r3_dynamic_wet_surfaces_far",	&ps_r3_dyn_wet_surf_far,	30,	100		);
	CMD4(CCC_Integer,	"r3_dynamic_wet_surfaces_sm_res",&ps_r3_dyn_wet_surf_sm_res,64,	2048	);

	CMD3(CCC_Mask,		"r3_volumetric_smoke",			&ps_r2_ls_flags,			R3FLAG_VOLUMETRIC_SMOKE);
	CMD3(CCC_Mask, "r4_enable_tessellation", &ps_r2_ls_flags_ext, R2FLAGEXT_ENABLE_TESSELLATION);//Need restart

	// IX-Ray
	CMD3(CCC_Mask, "r__fast_details_update",&ps_r2_ls_flags, R2FLAG_FAST_DETAILS_UPDATE);
	CMD4(CCC_DetailRadius, "r__detail_radius", &ps_r__detail_radius, 10, 5000);
	CMD3(CCC_Mask, "r__no_ram_textures", &ps_r__common_flags, RFLAG_NO_RAM_TEXTURES);
	CMD3(CCC_Mask, "r__mt_texture_load", &ps_r__common_flags, RFLAG_MT_TEX_LOAD);
	CMD3(CCC_Token, "r_aa", &ps_r2_aa_type, aa_type_token);
	CMD4(CCC_Integer, "r__optimize_static_geom", &opt_static, 0, 2);
	CMD4(CCC_Integer, "r__optimize_dynamic_geom", &opt_dynamic, 0, 2);
	CMD3(CCC_Mask, "r__optimize_shadow_geom", &ps_r__common_flags, RFLAG_OPT_SHAD_GEOM);
	CMD3(CCC_Mask, "r__shader_cache", &ps_r__common_flags, RFLAG_USE_CACHE);
	CMD3(CCC_Token, "r__screenshot_format", &ps_screenshot_format, screenshot_format_token);

	CMD3(CCC_Mask, "r1_use_terrain_mask", &ps_r1_flags, R1FLAG_TERRAIN_MASK);

	CMD4(CCC_Float, "r2_aref_quality", &ps_r2_def_aref_quality, 70.0f, 200.0f);
	CMD3(CCC_Mask, "r2_use_bump", &ps_r__common_flags, R2FLAG_USE_BUMP);
	CMD3(CCC_Mask, "r2_vignette", &ps_r2_ls_flags_ext, R2FLAG_SPP_VIGNETTE);
	CMD3(CCC_Mask, "r2_aberration", &ps_r2_ls_flags_ext, R2FLAG_SPP_ABERRATION);
	CMD3(CCC_Mask, "r2_saturation", &ps_r2_ls_flags_ext, R2FLAG_SPP_SATURATION);
	CMD3(CCC_Token, "r2_smap_size", &ps_r2_smapsize, qsmapsize_token);
	CMD3(CCC_Mask, "r2_cloud_shadows", &ps_r2_ls_flags_ext, RFLAG_CLOUD_SHADOWS);	//Need restart

	CMD3(CCC_Mask, "r4_hud_shadows", &ps_r2_ls_flags_ext, R4FLAG_SCREEN_SPACE_HUD_SHADOWS);
	CMD3(CCC_Mask, "r4_hashed_alpha_test", &ps_r2_ls_flags_ext, R4FLAG_HASHED_ALPHA_TEST);
	CMD3(CCC_Mask, "r4_sslr_water", &ps_r2_ls_flags_ext, R4FLAG_SSLR_ON_WATER);
	CMD4(CCC_Float, "r4_cas_sharpening", &ps_r4_cas_sharpening, 0.0f, 1.0f);

	CMD3(CCC_Mask, "r4_puddles", &ps_r2_ls_flags_ext, R4FLAG_PUDDLES);

#ifdef DEBUG_DRAW
#if RENDER!=R_R1
	CMD1(CCC_BuildSSA, "build_ssa");
#endif
	CMD4(CCC_Integer, "r__lsleep_frames", &ps_r__LightSleepFrames, 4, 30);
	CMD4(CCC_Float, "r__ssa_glod_start", &ps_r__GLOD_ssa_start, 128, 512);
	CMD4(CCC_Float, "r__ssa_glod_end", &ps_r__GLOD_ssa_end, 16, 96);
	CMD4(CCC_Float, "r__wallmark_shift_pp", &ps_r__WallmarkSHIFT, 0.0f, 1.f);
	CMD4(CCC_Float, "r__wallmark_shift_v", &ps_r__WallmarkSHIFT_V, 0.0f, 1.f);
	CMD1(CCC_ModelPoolStat, "stat_models");

#ifdef USE_DX11
	CMD1(CCC_RenderDocCaptureStart, "rdoc_start");
	CMD1(CCC_RenderDocCaptureEnd, "rdoc_end");
	//	Allow real-time fog config reload
	CMD1(CCC_Fog_Reload, "r3_fog_reload");
#endif

	CMD3(CCC_Mask, "r4_wireframe", &ps_r2_ls_flags_ext, R2FLAGEXT_WIREFRAME);//Need restart
	CMD2(CCC_R2GM, "r2em", &ps_r2_gmaterial);

	CMD2(CCC_Boolean, "ui_dbg_graphic", &Engine.External.EditorStates[(int)EditorUI::Shaders]);
	CMD1(CCC_DumpResources, "dump_resources");
	//	Igor: just to test bug with rain/particles corruption
	CMD1(CCC_RestoreQuadIBData, "r_restore_quad_ib_data");
	CMD4(CCC_Integer, "r_particles_real_dt", &ps_r2_particle_dt, 0, 1);
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
	CMD4(CCC_Float, "r2_dhemi_light_scale", &ps_r2_dhemi_light_scale, 0, 100.f);
	CMD4(CCC_Float, "r2_dhemi_light_flow", &ps_r2_dhemi_light_flow, 0, 1.f);
	CMD4(CCC_Float, "r2_dhemi_smooth", &ps_r2_lt_smooth, 0.f, 10.f);
	CMD3(CCC_Mask, "rs_hom_depth_draw", &ps_r2_ls_flags_ext, R_FLAGEXT_HOM_DEPTH_DRAW);
	CMD3(CCC_Mask, "r2_shadow_cascede_zcul", &ps_r2_ls_flags_ext, R2FLAGEXT_SUN_ZCULLING);
	CMD3(CCC_Mask, "r2_exp_splitscene", &ps_r2_ls_flags, R2FLAG_EXP_SPLIT_SCENE);
	CMD3(CCC_Mask, "r2_exp_donttest_uns", &ps_r2_ls_flags, R2FLAG_EXP_DONT_TEST_UNSHADOWED);
#endif
}

void xrRender_apply_tf() {
	Console->Execute("r__tf_aniso");
	Console->Execute("r__tf_mipbias");
}

#endif

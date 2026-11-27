#pragma once

#include "ColorMapManager.h"
#include "ScreenPostProcessTypes.h"

class light;
class dxRender_Visual;
struct SPPInfo;
struct GpuTileView;

//#define DU_SPHERE_NUMVERTEX 92
//#define DU_SPHERE_NUMFACES	180
//#define DU_CONE_NUMVERTEX	18
//#define DU_CONE_NUMFACES	32
//	no less than 2
#define	VOLUMETRIC_SLICES	100

class CRenderTarget		: public IRender_Target
{
private:
	u32							dwWidth;
	u32							dwHeight;
	float NightVisionInterference = 0.0f;
	bool NightVisionInterferenceInitialized = false;

	struct SpotShadowCacheKey
	{
		Fmatrix View;
		Fmatrix Project;
		IRender_Sector* Sector = nullptr;
		u32 Size = 0;
		ERHI_FORMAT Format = ERHI_FORMAT::UNKNOWN;
		u64 Revision = 0;
		float MipBias = 0.f;
		int Anisotropy = 0;

		bool operator==(const SpotShadowCacheKey& Other) const;
		bool HasSameGeometry(const SpotShadowCacheKey& Other) const;
	};

	enum class SpotShadowCacheState
	{
		Pending,
		Ready
	};

	struct SpotShadowCacheEntry
	{
		SpotShadowCacheKey Key;
		u64 LightId = 0;
		ref_rt Depth;
		ref_shader CopyShader;
		SpotShadowCacheState State = SpotShadowCacheState::Pending;
		u32 LastSeenFrame = 0;
		u32 LastUsedFrame = 0;
		u32 StableObservations = 0;
		xr_vector<dxRender_Visual*> CachedCasters;
		xr_hash_set<dxRender_Visual*> CachedCasterSet;
		xr_vector<dxRender_Visual*> PerFrameCasters;
		xr_vector<std::pair<ref_texture, u64>> Textures;
		bool Classified = false;
		u32 NextTextureCheckFrame = 0;
		xr_vector<ref_texture> PendingTextures;
		u64 Bytes = 0;
		xr_string PerFrameReason;
		// What moves in the lamp's view, once it stopped changing, is drawn into the cached depth whole. Seen: the hash of
		// what the view casts out of the frame's table (GpuScene::TestTile) and the frames it stayed the same; Baked: the
		// one the depth holds, 0 for none
		u64 SeenCasters = 0;
		u32 SeenFrames = 0;
		u64 BakedCasters = 0;
	};

	struct SpotShadowCacheStatistics
	{
		u32 Lights = 0;
		u32 Hits = 0;
		u32 Builds = 0;
		u32 Restores = 0;
		u32 FirstSight = 0;
		u32 Moving = 0;
		u32 Uncached = 0;
		u32 Unseen = 0; // parts of lamps whose reach the frames before could not see
		u32 DepthWaits = 0;
		u32 Direct = 0;
		u32 Evictions = 0;
		u32 StaticCasters = 0;
		u32 PerFrameCasters = 0;
		u32 DynamicPasses = 0;
		u32 SpecialPasses = 0;
		u32 TexturePending = 0;
		u32 BudgetLimited = 0;
		u32 CapacityLimited = 0;
		u32 ResourceFailures = 0;
		u32 Classifications = 0;
		u32 Invalidations = 0;
		u32 CachedSkips = 0;
		u32 DuplicateSkips = 0;
		u32 DirtyGraphs = 0;
		u32 FrameStaticItems = 0;
		u32 FrameDynamicItems = 0;
		u32 TextureInvalidations = 0;
		u32 GeometryInvalidations = 0;
		u32 DepthInvalidations = 0;
		u32 CasterReclassifications = 0;
		u32 PendingTextureChecks = 0;
		u64 RestoredPixels = 0;
	};

	xr_vector<SpotShadowCacheEntry> SpotShadowCache;
	STextureList SpotShadowEmptyTextures;
	xr_atomic_u64 SpotShadowContentRevision = 1;
	u64 SpotShadowAppliedRevision = 0;
	u64 SpotShadowCacheBytes = 0;
	// The depth an entry let go, kept for the next entry that needs one of its size: a light that moves, or one whose
	// tile grows as the eye nears it, would have a texture made every frame otherwise
	struct SpotShadowFreeDepth
	{
		ref_rt Depth;
		ref_shader CopyShader;
		u32 Size;
		ERHI_FORMAT Format;
	};
	xr_vector<SpotShadowFreeDepth> SpotShadowFreeDepths;
	// Depths the frame's builds lacked, without their resources until CreateSpotShadowDepths
	xr_vector<SpotShadowFreeDepth> SpotShadowDepthRequests;
	u64 SpotShadowResourceSerial = 0;
	u32 SpotShadowStatisticsFrame = u32(-1);
	SpotShadowCacheStatistics SpotShadowStatistics;
	SpotShadowCacheStatistics SpotShadowLastStatistics;

	SpotShadowCacheKey MakeSpotShadowCacheKey(const light& Source) const;
	SpotShadowCacheEntry* FindSpotShadowCache(const light& Source, const SpotShadowCacheKey& Key);
	void ClassifySpotShadowCasters(light& Source, SpotShadowCacheEntry& Entry);
	bool HasSpotShadowTextureChanges(const SpotShadowCacheEntry& Entry) const;
	bool HasSpotShadowPendingTextureChanges(SpotShadowCacheEntry& Entry);
	bool BuildSpotShadowCache(light& Source, SpotShadowCacheEntry& Entry);
	void ReleaseSpotShadowDepth(SpotShadowCacheEntry& Entry);
	void TrimSpotShadowCache(u64 RequiredBytes = 0, SpotShadowCacheEntry* Keep = nullptr);
	u64 SpotShadowBudget() const;

public:
	// A lamp part of the frame, decided before any of it is drawn (PrepareSpotShadow). Direct: drawn whole, the level as a
	// batch and the statics the batch leaves (decor objects, first sight, moved since the frame before, no entry left);
	// else of an entry, whose depth is restored when it is used (UseCache and HasStatic), its statics drawn from its lists
	// when not. HasCasters: what moves is cast out of the frame's table (GpuScene::HitLamp, QueueTile); not for a cached
	// lamp far from the eye, nor once what moves is baked into its depth
	struct SpotShadowPart
	{
		light* Light = nullptr;
		SpotShadowCacheEntry* Entry = nullptr;
		u32 Lamp = 0; // of CRender::LampLights
		u32 Page = u32(-1); // of the atlas, none when the part did not fit
		bool IsDirect = false;
		bool IsDecor = false;
		bool IsEmpty = false;
		bool IsBuilt = false; // its entry's depth is drawn this frame (BuildSpotShadow)
		bool UseCache = false;
		bool HasStatic = false;
		bool HasCasters = false;
		bool IsUnseen = false; // its lamp's reach is behind what the frames before drew (LampSelection)
	};

	void BeginSpotShadowCacheFrame();
	bool CheckSpotShadowGraph();
	void PrepareSpotShadow(SpotShadowPart& Part);
	void CountUnseenSpotShadow() { ++SpotShadowStatistics.Unseen; }
	// The depth of an entry PrepareSpotShadow found ready to build; the part reads it from this frame on
	void BuildSpotShadow(SpotShadowPart& Part);
	// After the frame's table is collected: what moves of a cached part checked against what its depth holds, and drawn
	// into it once it stays the same
	void BakeSpotShadow(GfxCmdList& Cmd, SpotShadowPart& Part, const GpuTileView& Tile);
	// The cached depths of the parts of one page, after the page is cleared and before anything else is drawn into it
	void RestoreSpotShadows(GfxCmdList& Cmd, const SpotShadowPart* Parts, u32 Count);
	// What a cached part draws over its restored depth; false when it has no map (Index: of CRender::LampParts)
	bool DrawSpotShadow(GfxCmdList& Cmd, SpotShadowPart& Part, u32 Index);
	// After the present, out of the frame: the depths the next builds take
	void CreateSpotShadowDepths();
	void ClearSpotShadowCache();
	void InvalidateSpotShadowCache();
	void DumpSpotShadowCache() const;
	void DescribeSpotShadowCache(string512& Line) const;

	IBlender* b_bloom = nullptr;
	IBlender* b_luminance = nullptr;
	IBlender* b_combine = nullptr;

	IBlender* FlatGasMaskBlender = nullptr;

#ifdef DEBUG
	struct		dbg_line_t		{
		Fvector	P0,P1;
		u32		color;
	};
	xr_vector<std::pair<Fsphere,Fcolor> >		dbg_spheres;
	xr_vector<dbg_line_t>										dbg_lines;
	xr_vector<Fplane>												dbg_planes;
#endif
	
	ref_rt						rt_sslr;
	ref_rt						rt_sslr_temp;
	ref_rt						rt_sslr_old;
	bool						SslrFlip = false;
	ref_rt						rt_sslr_data;
	ref_rt						rt_sslr_hiz;

	// MRT-path
	ref_rt						rt_Generic;
	ref_rt						rt_Back_Buffer;
	ref_rt						rt_Back_Buffer_AA;
	ref_rt						rt_Position;		// 64bit,	fat	(x,y,z,?)				(eye-space)
	ref_rt						rt_Velocity;
	ref_rt						ReactiveMask;
	ref_rt						RainRefraction;
	ref_rt						RainDepth;
	ref_shader					RainLayers;
	ref_shader					RainRefractionBlur;
	ref_rt						rt_ui_pda;

	ref_rt						rt_ui_depth;
	ref_rt						rt_ui_color;

	// Editor mode: scene depth of the viewport size, replaces the window depth (RDepth)
	ref_rt						rt_EditorDepth;
	IRHIDepthStencilView*		EditorWindowDepth = nullptr;
	// r_msaa: what the forward level frame draws into, and the window depth while it does
	ref_rt						rt_ForwardMs;
	ref_rt						rt_ForwardDepthMs;
	ref_rt						rt_DistortMs;
	// The normals the depth pass of the forward level frame writes for the occlusion pass
	ref_rt						rt_Thin;
	ref_rt						rt_Ids;
	ref_rt						rt_Surface;
	ref_rt						rt_SkyBehind; // the sky and the clouds with nothing in front: what far surfaces of the forward level frame fade into
	ref_rt						rt_ThinMs;
	ref_rt						rt_VelocityMs;
	ref_rt						rt_LightTiles; // per screen tile the lights that reach it, a bit a light
	ref_rt						rt_HudContact; // how much of the sun reaches the HUD past itself
	ref_rt						rt_HudContactRaw; // ... as the trace leaves it, a yes or no a pixel
	IRHIDepthStencilView*		ForwardWindowDepth = nullptr;
	ref_rt						rt_EditorOverlayDepth;

	ref_rt						rt_upscaled_depth;
	ref_rt						rt_upscaled_depth_old;

	ref_rtc						rt_Reflection;
	ref_rtc						rt_Reflection_temp;
	ref_rt						rt_Reflection_forward;
	ref_rt						rt_Reflection_history;
	ref_rt						rt_Depth;
	
	ref_rt						rt_water_probe_prev;

	IRHISurface*				rt_smap_depth_sun_tex;
	// The sun's three cascades, then the rain's map: what the rain reaches, seen from above
	static constexpr u32 SunSlices = 4;
	static constexpr u32 RainSlice = 3;
	IRHIDepthStencilView*		rt_smap_depth_sun_dsv[SunSlices];
	ref_texture					rt_smap_depth_sun;

	// 
	ref_rt						rt_Accumulator;		// 64bit		(r,g,b,specular)

	ref_rt						rt_Forward;
	ref_rt						rt_Revealage;

	ref_rt						rt_Generic_0;		// 32bit		(r,g,b,a)				// post-process, intermidiate results, etc.
	ref_rt rt_DofScopeMask;
	float LensDofFactor = 0.0f;
	Fvector4 DofCameraLens = {}; // focal length (mm), f-number, sensor height (m), focus time (s)
	bool DofCameraInitialized = false;
	float DofRadiusLimit = 20.0f;
	ref_rt						rt_Generic_1;		// 32bit		(r,g,b,a)				// post-process, intermidiate results, etc.
	//	Igor: for volumetric lights
	ref_rt						rt_Generic_2;		// 32bit		(r,g,b,a)				// post-process, intermidiate results, etc.
	ref_rt						rt_BackbufferLUT;		// 32bit		(r,g,b,a)				// post-process, intermidiate results, etc.

	ref_rt						rt_Bloom_A;			// 32bit, dim		(r,g,b,?)
	ref_rt						rt_Bloom_B;			// 32bit, dim/2		(r,g,b,?)
	ref_rt						rt_Bloom_C;			// 32bit, dim/4		(r,g,b,?)
	ref_rt						rt_Bloom_D;			// 32bit, dim/8		(r,g,b,?)
	ref_rt						rt_Bloom_E;			// 32bit, dim/16	(r,g,b,?)
	ref_rt						rt_Bloom_F;			// 32bit, dim/32	(r,g,b,?)
	ref_rt						rt_Bloom_G;			// 32bit, dim/64
	ref_rt						rt_Bloom_A2;			// 32bit, dim		(r,g,b,?)
	ref_rt						rt_Bloom_B2;			// 32bit, dim/2		(r,g,b,?)
	ref_rt						rt_Bloom_C2;			// 32bit, dim/4		(r,g,b,?)
	ref_rt						rt_Bloom_D2;			// 32bit, dim/8		(r,g,b,?)
	ref_rt						rt_Bloom_E2;			// 32bit, dim/16	(r,g,b,?)
	ref_rt						rt_Bloom_F2;			// 32bit, dim/32	(r,g,b,?)

	ref_rt						rt_LUM_A;			// 32bit, 1024x1024,log-average in all components
	ref_rt						rt_LUM_B;			// 32bit, 128x128,	log-average in all components
	ref_rt						rt_LUM_C;			// 32bit, 16x16,	log-average in all components
	ref_rt						rt_LUM_D;			// 64bit, 1x1,		log-average in all components
	//==================================================

	//============== new Depth of Field ==============
	ref_rt						rt_dof_focus;			// R32F, 1x1,		current focus distance
	ref_rt						rt_dof_focus_prev;		// R32F, 1x1,		prev frame focus distance
	ref_rt						rt_dof_coc;				// R32F, full resolution,		signed CoC in output pixels
	ref_rt						rt_dof_coc_prev;		// R32F, full resolution,		signed CoC in output pixels
	ref_rt						rt_dof_blur1;			// RGBA16F, component 0 real RGBA
	ref_rt DofDownsample;							// RGBA16F, prepared near, then resolved premultiplied color/opacity
	ref_rt DofDownsampleDepth;						// RGBA32F, near/far raw depth and output-pixel radii
	ref_rt						rt_dof_blur2;			// RGBA16F, component 0 imaginary RGBA
	ref_rt						rt_dof_blur3;			// RGBA16F, prepared far, then resolved color/validity
	ref_rt DofReal1; // RGBA16F, far component 1 real RGBA
	ref_rt DofImaginary1; // RGBA16F, far component 1 imaginary RGBA
	ref_rt DofTileMax; // R32F, maximum near radius per 8x8 reduced texels
	ref_rt DofTileDilated; // R32F, conservative near radius for separable gathering

	// env
	ref_texture				t_envmap_0		;	// env-0
	ref_texture				t_envmap_1		;	// env-1

	//GTAO
	ref_rt						rt_gtao_0;
	ref_rt						rt_ao_depth;
	ref_rt						rt_GtaoCurrent;
	ref_rt						rt_GtaoHistory[2];
	u32							GtaoFrame = 0;

	//TAA
	ref_rt					rt_Generic_0_prev; //previous frame for taa

	// smap
	// The shadow atlas of the local lights: an array, one slice a page, so that every page of the frame stays
	// readable once the next one is drawn. A page past the last slice takes the place of an earlier one
	static constexpr u32		SmapPages = 4;
	IRHISurface*				rt_smap_depth_tex = nullptr;
	IRHIDepthStencilView*		rt_smap_depth_dsv[SmapPages] = {};
	ref_texture					rt_smap_depth;
	IRHIDepthStencilView*		SmapPage(u32 Page) const { return rt_smap_depth_dsv[Page % SmapPages]; }
//	TODO: DX10: CHeck if we need old-style SMAP

	// Textures
	ref_texture					t_material;

	ref_texture					t_noise				[TEX_jitter_count];

	ref_shader					s_sslr;
	// The forward level frame draws its puddles (RenderForwardPuddles) and lays its weighed blended surfaces over its
	// scene (RenderForwardOit) with these
	ref_shader					s_puddles;
	ref_shader					s_oit_composite;
private:

	ref_shader					s_spp;
	ref_shader					s_gasmask;
	ref_shader FlatGasMaskShader;
	ref_shader					s_fxaa;
	ref_shader					s_smaa;
	ref_shader					s_scale;
	ref_shader					s_cas;
	ref_shader					s_gtao;
	ref_shader					s_taa;
	ref_shader					s_nvg;

	// For gamma correction in windowed mode
	ref_shader					s_gamma;

	// SMAA
	ref_rt						rt_smaa_edgetex;
	ref_rt						rt_smaa_blendtex;

	// SSAO
	ref_rt						rt_ssao_temp;

	ref_shader					s_accum_direct_volumetric;
	RHIObject*			g_debug_blend_state = nullptr;
	void				DrawDebugWindow(); // RenderDebugWindow.cpp

	float							f_bloom_factor;

	// Luminance
	ref_shader			s_luminance;
	bool						IsExposureFresh = true;

	//================= new bloom and lum =================
	ref_shader					s_bloom_downsample;
	ref_shader					s_bloom_upsample;
	ref_shader					s_lum_copy;
	ref_shader					s_lum_downsample;
	ref_shader					s_lum_calc;
	//=====================================================

	//============== new Depth of Field ==============
	ref_shader					s_dof_focus;
	ref_shader					s_dof_coc;
	ref_shader					s_dof_blur1;
	ref_shader					s_dof_blur2;
	ref_shader					s_dof_blur3;

	// Combine
	ref_shader				s_combine_volumetric;
public:
	ref_shader				s_combine;
	ref_shader s_r1_distort;
	ref_shader s_depth_resolve_ms;
	ref_shader s_thin_resolve_ms;
	ref_shader s_light_tiles;
	ref_texture t_rain_splash; // the rings of drops, a volume through time
	ref_texture t_rain_streaks; // the streaks that run down walls
	// The material of snow, where the level geometry takes it in its own shaders: its colour and its two bump maps
	ref_texture t_snow_base;
	ref_texture t_snow_bump;
	ref_texture t_snow_bump_x;
	bool SnowInMaterial = false;
	bool SnowIsPbr = false;
	ref_texture t_puddle_normal; // the ripples of standing water
	ref_texture t_puddle_ripples; // the rings rain leaves in it
	ref_texture t_puddle_water; // where the level's own water lies, and how high
	ref_shader s_hud_contact;
	ref_shader s_hud_contact_filter;
	ref_shader				s_postprocess;
	ref_shader ScopePostprocessShader;
	ColorMapManager ScopeColorMaps{"$user$scope_cmap0", "$user$scope_cmap1"};
	ref_shader				s_menu;
	ref_geom					g_menu;
private:
	float ScopeNoiseTime = 0.0f;
	u32 ScopeNoiseShiftWidth = 0;
	u32 ScopeNoiseShiftHeight = 0;
	float						im_noise_time;
	u32							im_noise_shift_w;
	u32							im_noise_shift_h;
	
	float						param_blur;
	float						param_gray;
	float						param_duality_h;
	float						param_duality_v;
	float						param_noise;
	float						param_noise_scale;
	float						param_noise_fps;
	u32							param_color_base;
	u32							param_color_gray;
	Fvector						param_color_add;

	//	Color mapping
	float						param_color_map_influence;
	float						param_color_map_interpolate;
	ColorMapManager				color_map_manager;

	//	Igor: used for volumetric lights
	bool						m_bHasActiveVolumetric;
public:
								CRenderTarget			();
	virtual ~CRenderTarget			() override;

	void						u_compute_texgen_screen	(GfxCmdList& Cmd, Fmatrix&	dest);
	void						u_compute_texgen_jitter	(GfxCmdList& Cmd, Fmatrix&	dest);
	void						u_setrt					(GfxCmdList& Cmd, const ref_rt& _1, const ref_rt& _2, const ref_rt& _3, const ref_rt& _4, IRHIDepthStencilView* zb);
	void						u_setrt					(GfxCmdList& Cmd, const ref_rt& _1, const ref_rt& _2, const ref_rt& _3, IRHIDepthStencilView* zb);
	void						u_setrt					(GfxCmdList& Cmd, const ref_rt& _1, const ref_rt& _2, IRHIDepthStencilView* zb);
	void						u_setrt					(GfxCmdList& Cmd, u32 W, u32 H, IRHIRenderTargetView* _1, IRHIRenderTargetView* _2, IRHIRenderTargetView* _3, IRHIDepthStencilView* zb);
	void						u_setrt					(GfxCmdList& Cmd, u32 W, u32 H, IRHIRenderTargetView* _1, IRHIRenderTargetView* _2, IRHIRenderTargetView* _3, IRHIRenderTargetView* _4, IRHIDepthStencilView* zb);

	template <class Bind>
	void						DrawPassSQ	(GfxCmdList& Cmd, const ref_shader& shader, u32 element, Bind&& bind);
	void						DrawPassSQ	(GfxCmdList& Cmd, const ref_shader& shader, u32 element);
	template <class Bind>
	void						DrawSQ			(GfxCmdList& Cmd, const ref_shader& shader, const ref_rt& target, u32 element, Bind&& bind);
	void						DrawSQ			(GfxCmdList& Cmd, const ref_shader& shader, const ref_rt& target, u32 element = 0);
	void						ResolveSurface			(const ref_rt& dst, const ref_rt& src);
	void						u_calc_tc_noise(GfxCmdList& Cmd, Fvector2& p0, Fvector2& p1, float Scale, float FPS, bool IsScope);
	void						u_calc_tc_duality_ss	(Fvector2& r0, Fvector2& r1, Fvector2& l0, Fvector2& l1);
	bool						u_need_PP				();
	bool						u_need_CM				();

	void						init_fsr				();
	bool						phase_fsr				();
	bool PhaseUpscaleNri(GfxCmdList& Cmd);
	ref_rt rt_UpscaleMotion;
	ref_rt rt_UpscaleDepth;
	ref_shader UpscaleGuides;
	void						phase_scale				(GfxCmdList& Cmd);
	void						PhaseEditorScale		(GfxCmdList& Cmd);

	void						copy_position			();

	void						phase_smaa				(GfxCmdList& Cmd);
	void						phase_taa				(GfxCmdList& Cmd);
	void						phase_mblur				(GfxCmdList& Cmd);
	void						phase_depth_upscale		(GfxCmdList& Cmd);

	void						phase_ui_postprocess	(GfxCmdList& Cmd, Fcolor*);
	void						phase_ui_postprocess_clear(GfxCmdList& Cmd);

	void						PhaseDepthPyramid		(GfxCmdList& Cmd);
	void						phase_gtao				(GfxCmdList& Cmd);
	void						phase_sslr				(GfxCmdList& Cmd);

	// phase_sslr writes rt_sslr and rt_sslr_old in turn, the other one is its history
	const ref_rt&				SslrOutput				() const { return SslrFlip ? rt_sslr_old : rt_sslr; }
	bool						SslrActive				() const;
	void						CreateSslrTargets			();
	void						pharse_velocity			(GfxCmdList& Cmd);


	void						PhaseEffectSQ			(GfxCmdList& Cmd, EffectSQ Effect);
	void						PhaseRainStreaksBegin(GfxCmdList& Cmd);
	void						PhaseRainStreaksEnd(GfxCmdList& Cmd);
	void						PhaseRainLayers(GfxCmdList& Cmd, float SkyVisibility);
	void						CreateRainStreakTargets(ERHI_FORMAT Format);

	void						phase_smap_direct		(GfxCmdList& Cmd, light* L,	u32 sub_phase);
	void						phase_smap_spot_clear	(u32 Page);
	void						phase_smap_spot			(GfxCmdList& Cmd, light* L);
	void SetupSpotShadowTarget(GfxCmdList& Cmd, IRHIDepthStencilView* Depth, u32 Width, u32 Height, RHIViewport Viewport);
	void						phase_vol_accumulator	(GfxCmdList& Cmd);

	bool						need_to_render_sunshafts();

	void						draw_volume				(GfxCmdList& Cmd, light* L);
	void						accum_direct_volumetric	(GfxCmdList& Cmd);
	void						accum_volumetric_lv		(GfxCmdList& Cmd, light* L);
	void ForwardVolumetrics(GfxCmdList& Cmd);
	void ForwardVelocity(GfxCmdList& Cmd);

	void						phase_post				(GfxCmdList& Cmd);
	void						phase_combine_volumetric(GfxCmdList& Cmd);
	void						phase_pp(GfxCmdList& Cmd, const SPPInfo* ScopeInfo = nullptr);
	//==========================================================
	// Levels: how many of the chain; the first alone is what the exposure is measured from
	void						phase_bloom_downsample(GfxCmdList& Cmd, u32 Levels = 7);
	void						ResetExposure(GfxCmdList& Cmd);
	// The passes made by their blenders, which a recompile does not reach, with the settings of now
	void						RemakeBlenderPasses();
	void						phase_bloom_upsample(GfxCmdList& Cmd);
	void						phase_new_luminance(GfxCmdList& Cmd);
	void CreateDofBlurTargets(GfxCmdList& Cmd);
	void						phase_new_dof(GfxCmdList& Cmd);
	//==========================================================


	virtual void				set_blur				(float	f) override { param_blur=f;						}
	virtual void				set_gray				(float	f) override { param_gray=f;						}
	virtual void				set_duality_h			(float	f) override { param_duality_h=std::abs(f);			}
	virtual void				set_duality_v			(float	f) override { param_duality_v=std::abs(f);			}
	virtual void				set_noise				(float	f) override { param_noise=f;					}
	virtual void				set_noise_scale			(float	f) override { param_noise_scale=f;				}
	virtual void				set_noise_fps			(float	f) override { param_noise_fps= std::abs(f)+EPS_S;	}
	virtual void				set_color_base			(u32	f) override { param_color_base=f;				}
	virtual void				set_color_gray			(u32	f) override { param_color_gray=f;				}
	virtual void				set_color_add			(const Fvector	&f) override { param_color_add=f;		}

	virtual u32					get_width				() override;
	virtual u32					get_height				() override;
	virtual u32					get_target_width		() override;
	virtual u32					get_target_height		() override;
	virtual u32					get_core_width			() override;
	virtual u32					get_core_height			() override;

	virtual void				set_cm_imfluence	(float	f) override { param_color_map_influence = f;							}
	virtual void				set_cm_interpolate	(float	f) override { param_color_map_interpolate = f;							}
	virtual void				set_cm_textures		(const shared_str &tex0, const shared_str &tex1) override {color_map_manager.SetTextures(tex0, tex1);}

	void						DoAsyncScreenshot		();
	// Resolves the LDR frame into Destination (the swapchain target when null)
	void						PhaseGammaApply			(GfxCmdList& Cmd, IRHIRenderTargetView* Destination = nullptr);

#ifdef DEBUG
	IC void						dbg_addline				(Fvector& P0, Fvector& P1, u32 c)					{
		dbg_lines.push_back		(dbg_line_t());
		dbg_lines.back().P0		= P0;
		dbg_lines.back().P1		= P1;
		dbg_lines.back().color	= c;
	}
	IC void						dbg_addplane			(Fplane& P0,  u32 c)								{
		dbg_planes.push_back(P0);
	}
#else
	IC void						dbg_addline				(Fvector& P0, Fvector& P1, u32 c)					{}
	IC void						dbg_addplane			(Fplane& P0,  u32 c)								{}
#endif
};

#include "ixr_fullscreen_pass.h"

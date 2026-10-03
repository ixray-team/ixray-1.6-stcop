#pragma once

#include "r__dsgraph_structure.h"
#include "r__occlusion.h"

#include "PSLibrary.h"

#include "r__types.h"
#include "r4_rendertarget.h"

#include "HOM.h"
#include "DetailManager.h"
#include "ModelPool.h"
#include "WallmarksEngine.h"

#include "SMAP_Allocator.h"
#include "Light_DB.h"
#include "LightTrack.h"
#include "r1_LightProjector.h"
#include "r1_LightShadows.h"
#include "r1_GlowManager.h"
#include "r_sun_cascades.h"

#include "../../xrEngine/IRenderable.h"
#include "../../xrEngine/Fmesh.h"
#include <atomic>

class dxRender_Visual;
class CLightR_Manager;

// definition
class CRender :
	public IRender_interface,
	public pureFrame
{
public:
	enum { PHASE_POINT = 3, PHASE_SPOT = 4, PHASE_LMODELS = 5 };
	bool vis_intersect = false;
	CLightR_Manager* L_Dynamic = nullptr;
	CLightProjector* L_Projector = nullptr;
	CLightShadows* L_Shadows = nullptr;
	CGlowManager* L_Glows = nullptr;

	enum
	{
		MMSM_OFF = 0,
		MMSM_ON,
		MMSM_AUTO,
		MMSM_AUTODETECT
	};

public:
	struct _options	
	{
		u32 HW_smap_FORMAT		: 32;
		u32 smapsize			: 16;
		    
		u32 nvstencil			: 1;
		u32 nvdbt				: 1;
		    
		u32 nullrt				: 1;
		    
		u32 distortion			: 1;
		u32 distortion_enabled	: 1;
		    
		u32 sunstatic			: 1;
		u32 noshadows			: 1;
		u32 disasm				: 1;
		u32 volumetricfog		: 1;
		u32 offscreen_reflecitons	: 1;
		u32 deffered_reflecitons	: 1;

		u32 dx11_use_legacy_light : 1;
		u32 dx11_enable_tessellation : 1;
		u32 dx11_disable_motion_vectors : 1;
		u32 dx11_allow_wboit_transparency : 1;
	} o;

	struct _stats
	{
		u32		l_total,	l_visible;
		u32		l_shadowed,	l_unshadowed;
		s32		s_used,		s_merged,	s_finalclip;
		u32		o_queries,	o_culled;
		u32		ic_total,	ic_culled;
	}			stats;
public:
	// Sector detection and visibility
	CSector*													pLastSector;
	CSector*													pOutdoorSector;
	Fvector														vLastCameraPos;
	u32															uLastLTRACK;
	xr_vector<IRender_Portal*>									Portals;
	xr_vector<IRender_Sector*>									Sectors;
	CDB::COLLIDER												Sectors_xrc;
	CDB::MODEL*													rmPortals;
	CHOM														HOM;
	R_occlusion													HWOCC;

	R_dsgraph_structure GraphMain;
	xr_array<R_dsgraph_structure, 6> GraphReflection;
	xr_array<R_dsgraph_structure, 3> GraphSun;

	// Global vertex-buffer container
	xr_vector<FSlideWindowItem>									SWIs;
	xr_vector<ref_shader>										Shaders;
	using VertexDeclarator = FixedVector<RHIInputElementDesc, 65>;
	xr_vector<VertexDeclarator>									nDC,xDC;
	xr_vector<IRHIBuffer*>							nVB,xVB;
	xr_vector<IRHIBuffer*>							nIB,xIB;
	xr_vector<u32>									nVBBase,xVBBase,nIBBase,xIBBase;
	xr_vector<dxRender_Visual*>									Visuals;
	CPSLibrary													PSLibrary;

	CDetailManager*												Details;
	CModelPool*													Models;
	CWallmarksEngine*											Wallmarks;

	CRenderTarget*												Target;			// Render-target

	CLight_DB													Lights;
	xr_vector<light*>											Lights_LastFrame;
	SMAP_Allocator												LP_smap_pool;
	light_Package												LP_normal;
	light_Package												LP_pending;


	shared_str													c_sbase			;
	shared_str													c_lmaterial		;
	float														o_hemi			;
	float														o_hemi_cube[CROS_impl::NUM_FACES]	;
	float														o_sun			;


	bool														m_bMakeAsyncSS;
	bool														m_bFirstFrameAfterReset;	// Determines weather the frame is the first after resetting device.
	xr_vector<sun::cascade>										m_sun_cascades;

	xr_list<light*>												v_all_lights_dque;

private:
	// Loading / Unloading
	void							LoadVisuals					(IReader	*fs);
	void							LoadLights					(IReader	*fs);
	void							LoadPortals					(IReader	*fs);
	void							LoadSectors					(IReader	*fs);
	void							LoadVertexBuffers			(IReaderBase& fs, bool _alternative);
	void							LoadIndexBuffers			(IReaderBase& fs, bool _alternative);
	void							LoadSWIs					(IReaderBase& fs);
	void							Load3DFluid					();

public:
	void							render_main					(bool deffered, bool zfill = false);
	void render_static();
	void							render_forward				();
	void							render_lights				(light_Package& LP	);
	void							render_menu					();
	void							render_rain					();

	void							render_sun_cascade			(u32 cascade_ind);
	void							init_cacades				();
	void							render_sun_cascades			();
	void begin_reflection_collect();
	void collect_reflections();
	void render_reflections();
	void wait_reflection_collect();

	void reset_sun_collect();
	void ensure_sun_collect();
	void begin_sun_collect();
	void collect_sun_cascades();
	void append_sun_dynamics();
	void wait_sun_collect();
	bool prepare_sun_cascade_xforms();
	void publish_sun_collect(bool active);

	Fvector reflection_cam_pos;
	Fvector reflection_cam_dir;
	Fvector reflection_cam_top;
	Fvector reflection_cam_right;
	float reflection_fov = 75.f;
	float reflection_near = 0.2f;
	float reflection_far = 1000.f;
	float _reflectionDistance = 0.7f;
	IRender_Sector* reflection_sector = nullptr;
	std::atomic<u32> reflection_ticket{ 0 };
	std::atomic<u32> reflection_done{ 0 };

	xr_array<Fmatrix, 3> sun_cascade_xforms{};
	Fvector sun_cull_cop{};
	u32 sun_cascade_count = 0;
	bool sun_kicked = false;
	bool sun_restore_shafts = false;
	bool sun_saved_reset_chain = false;
	std::atomic<bool> sun_collect_active{ false };
	std::atomic<u32> sun_ticket{ 0 };
	std::atomic<u32> sun_done{ 0 };
	u32 sun_seen = 0;

	bool is_render_cubemap = false;

public:
	ShaderElement*					rimp_select_sh_static		(dxRender_Visual	*pVisual, float cdist_sq);
	ShaderElement*					rimp_select_sh_dynamic		(dxRender_Visual	*pVisual, float cdist_sq, bool is_hud = false);
	RHIInputElementDesc*			getVB_Format				(int id, size_t* Count, bool	_alt=false);
	IRHIBuffer*			getVB						(int id, bool	_alt=false);
	IRHIBuffer*			getIB						(int id, bool	_alt=false);
	u32					getVB_Base					(int id, bool	_alt=false)	{ return (_alt?xVBBase:nVBBase)[id]; }
	u32					getIB_Base					(int id, bool	_alt=false)	{ return (_alt?xIBBase:nIBBase)[id]; }
	FSlideWindowItem*				getSWI						(int id);
	IRender_Portal*					getPortal					(int id);
	IRender_Sector*					getSectorActive				();
	IRenderVisual*					model_CreatePE				(const char* name);
	IRender_Sector*					detectSector				(const Fvector& P, Fvector& D);
	IRender_Sector*					detectLastSector			(const Fvector& P);
	int								translateSector				(IRender_Sector* pSector);
	
	virtual SurfaceParams getSurface(const char* nameTexture) override;

	// HW-occlusion culling
	IC u32							occq_begin					(u32&	ID		)	{ return HWOCC.occq_begin	(ID);	}
	IC void							occq_end					(u32&	ID		)	{ HWOCC.occq_end	(ID);			}
	IC bool							occq_get					(u32& ID, R_occlusion::occq_result& fragments)	{ return HWOCC.occq_get(ID, fragments); }

	Fvector avg_lit_color, avg_lit_dir;

	ICF void						apply_object				(IRenderable*	O)
	{
		if (LightingModeIsStatic())
		{
			RCache.set_c("L_dynamic_props", 0, 0, 0, 0);
			RCache.set_ca("m_plmap_clamp", 0, 0, 0, 0, 1);
			RCache.set_c("m_plmap_xform", Fidentity);
			if (!O || !O->renderable_ROS())
				return;
			CROS_impl& light_state = *static_cast<CROS_impl*>(O->renderable_ROS());
			light_state.update_smooth(O);
			const float sun = 0.5f * light_state.get_sun();
			RCache.set_c("L_dynamic_props", sun, sun, sun, 0.5f * light_state.get_hemi());
			if (L_Projector && light_state.shadow_recv_frame == Device.dwFrame && O->renderable_ShadowReceive())
				L_Projector->setup(light_state.shadow_recv_slot);
			return;
		}
		if (0==O)					return;
		if (0==O->renderable_ROS())	return;
		CROS_impl& LT				= *((CROS_impl*)O->renderable_ROS());
		LT.update_smooth			(O)								;
		o_hemi						= 0.75f*LT.get_hemi			()	;
		//o_hemi						= 0.5f*LT.get_hemi			()	;
		o_sun						= 0.75f*LT.get_sun			()	;
		CopyMemory(o_hemi_cube, LT.get_hemi_cube(), CROS_impl::NUM_FACES*sizeof(float));

		avg_lit_color = LT.get_avg_color();
		avg_lit_dir = LT.get_avg_dir();
	}
	IC void							apply_lmaterial				()
	{
		ref_constant constant = RCache.get_c(c_sbase);		
		
		RCache.hemi.set_lit_color(avg_lit_color, avg_lit_dir);
		avg_lit_color = avg_lit_dir = { 0,0,0 };

		RHIShaderConstant* C = constant ? &*constant : nullptr;		// get sampler
		if (0 == C) return;
		VERIFY(RC_dest_sampler == C->destination);
		VERIFY(RC_dx10texture == C->type);
		CTexture* T = RCache.get_ActiveTexture(u32(C->samp.index));
		VERIFY(T);

		float mtl = T->m_material;

#ifdef	DEBUG_DRAW
		if (ps_r2_ls_flags.test(R2FLAG_GLOBALMATERIAL))	
		{
			mtl = ps_r2_gmaterial;
		}
#endif

		mtl += 0.50f;
		mtl *= 0.25f;

		if (!o.dx11_use_legacy_light)
		{
			mtl = mtl - std::floor(mtl);

			//mtl = (((17.77777778f * mtl - 32.0f) * mtl + 20.22222222f) * mtl - 6.0f) * mtl + 1.0f;
			mtl = std::max(1.0f - 4.0f * mtl, (mtl - 0.25f) / 0.75f);

			mtl = mtl * mtl * (3.0f - 2.0f * mtl);
		}

		RCache.hemi.set_material (o_hemi,o_sun,0, mtl);
		RCache.hemi.set_pos_faces(o_hemi_cube[CROS_impl::CUBE_FACE_POS_X],
			o_hemi_cube[CROS_impl::CUBE_FACE_POS_Y],
			o_hemi_cube[CROS_impl::CUBE_FACE_POS_Z]);
		RCache.hemi.set_neg_faces	(o_hemi_cube[CROS_impl::CUBE_FACE_NEG_X],
			o_hemi_cube[CROS_impl::CUBE_FACE_NEG_Y],
			o_hemi_cube[CROS_impl::CUBE_FACE_NEG_Z]);
	}

public:
	// feature level
	virtual	GenerationLevel			get_generation			();

	virtual bool					is_sun_static			()	{ return o.sunstatic;}
	virtual DWORD					get_dx_level			()	{ return 0x000A0001; }

	virtual float					detail_trace_visibility(
		Fvector const& eye,
		Fvector const& target,
		float min_height,
		float opaque_distance,
		float sample_step) const override
	{
		if (!Details)
			return 1.f;
		return const_cast<CDetailManager*>(Details)->TraceVisibility(
			eye, target, min_height, opaque_distance, sample_step);
	}

	virtual void					detail_trample_mark(float x, float y, float z, float radius, float weight, bool isActor = false) override
	{
		if (Details)
			Details->TrampleMark(x, y, z, radius, weight, isActor);
	}

	virtual bool					detail_trample_enabled() const override
	{
		extern int ps_trample_enabled;
		return ps_trample_enabled != 0;
	}

	virtual float					detail_trample_draw_radius() const override
	{
		extern float ps_trample_draw_radius;
		return ps_trample_draw_radius;
	}

	// Detail Layers Editor tool (brush overlay + ImGui window)
	void renderImGuiDebugWindow_DetailLayersEditor() override;
	void DetailLayers_RenderBrush3D();
	// Releases the lazily created brush render objects while the RHI/device is still
	// alive (called from destroy()). Never leave these statics to DLL detach - the
	// device is gone by then and their destructors crash on the imported DevicePtr.
	void DetailLayers_EditorDestroy();

	// Loading / Unloading
	virtual void create();
	virtual void destroy();
	virtual	void reset_begin();
	virtual	void reset_end();

	virtual	void level_Load(IReader*);
	virtual void level_Unload();
	virtual void renderImGuiDebugWindow_SVGStorage() override;

	IRHISurface* load_texture(const char*	fname, u32& msize, bool bStaging = false) override;
	bool get_texture_metadata(const char* absolute_path, RHITextureMetadata* p_data) override;

	IRHISurface* texture_load(const char*	fname, u32& msize, bool bStaging = false);

	virtual HRESULT					shader_compile			(
		const char*							name,
		DWORD const*					pSrcData,
		UINT                            SrcDataLen,
		const char*                          pFunctionName,
		const char*                          pTarget,
		DWORD                           Flags,
		void*&							result);

	struct PuddleBase 
	{
		Fmatrix m_world = Fidentity;

		float m_height = EPS;
		float m_radius = EPS;
	};

	xr_vector<PuddleBase> m_levels_puddles;
	Frect m_puddles_level_bound;

	void							LoadPuddles();

	struct PlanarBase
	{
		Fmatrix m_world = Fidentity;

		float m_influence = EPS;
		float m_stiffness = 1.f;
		float m_radius = EPS;
	};

	xr_vector<PlanarBase> m_levels_planars;

	void							LoadPlanars();

	// Information
	virtual void					Statistics					(CGameFont* F);
	virtual const char*					getShaderPath				()									{ return "d3d11\\";	}
	virtual ref_shader				getShader					(int id);
	virtual IRender_Sector*			getSector					(int id);
	virtual IRenderVisual*			getVisual					(int id);
	virtual IRender_Sector*			detectSector				(const Fvector& P);
	virtual IRender_Target*			getTarget					();

	// Main 
	virtual void					flush						();
	virtual void					set_Object					(IRenderable* O, void* graph = nullptr);
	virtual	void					add_Occluder				(Fbox2&	bb_screenspace	);			// mask screen region as oclluded
	virtual void					add_Visual					(IRenderVisual* V, bool Ignore, void* graph = nullptr);			// add visual leaf	(no culling performed at all)

	// wallmarks
	virtual void					add_StaticWallmark			(ref_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V, bool UseCameraDirection = false);
	virtual void					add_StaticWallmark			(IWallMarkArray *pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V, bool UseCameraDirection = false) override;
	virtual void					add_StaticWallmark			(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V);
	virtual void					clear_static_wallmarks		();
	virtual StaticWallmarkHandle::WallmarkHandlePtr add_DynamicWallmark(const wm_shader& S, const Fvector& P, float w, float h, float r, CDB::TRI* T, Fvector* V) override;
	virtual void					add_SkeletonWallmark		(intrusive_ptr<CSkeletonWallmark> wm);
	virtual void					add_SkeletonWallmark		(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size);
	virtual void					add_SkeletonWallmark		(const Fmatrix* xf, IKinematics* obj, IWallMarkArray *pArray, const Fvector& start, const Fvector& dir, float size);

	//
	virtual IBlender*				blender_create				(CLASS_ID cls);

	//
	virtual IRender_ObjectSpecific*	ros_create					(IRenderable*		parent);
	virtual void					ros_destroy					(IRender_ObjectSpecific* &);

	// Lighting
	virtual IRender_Light*			light_create				();
	virtual IRender_Glow*			glow_create					();

	// Models
	virtual IRenderVisual*			model_CreateParticles		(const char* name);
	virtual IRender_DetailModel*	model_CreateDM				(IReader* F);
	virtual IRenderVisual*			model_Create				(const char* name, IReader* data=0);
	virtual IRenderVisual*			model_CreateChild			(const char* name, IReader* data);
	virtual IRenderVisual*			model_Duplicate				(IRenderVisual*	V);
	virtual void					model_Delete				(IRenderVisual* &	V, bool bDiscard);
	virtual void					model_Delete_Deffered		(IRenderVisual* &	V);
	virtual void 					model_Delete				(IRender_DetailModel* & F);
	virtual void					models_Prefetch				();
	virtual void					models_Clear				(bool b_complete);

	// Occlusion culling
	virtual bool					occ_visible					(vis_data&	V);
	virtual bool					occ_visible					(Fbox&		B);
	virtual bool					occ_visible					(sPoly&		P);

	// Main
	virtual void					Calculate					();
	virtual void					Render						();
	virtual void					RenderUI					(Fcolor* = nullptr);

	virtual void					Screenshot					(ScreenshotMode mode=SM_NORMAL, const char* name = 0);
	virtual void					Screenshot					(ScreenshotMode mode, CMemoryWriter& memory_writer);
	virtual void					ScreenshotAsyncBegin		();
	virtual void					ScreenshotAsyncEnd			(CMemoryWriter& memory_writer);
	virtual void		_BCL		OnFrame						();

	R_dsgraph_structure& TargetGraph(void* graph) { return graph ? *static_cast<R_dsgraph_structure*>(graph) : GraphMain; }

	virtual void set_Transform(Fmatrix* M, void* graph = nullptr)
	{
		VERIFY(M);
		TargetGraph(graph).val_pTransform = M;
	}
	virtual void set_LocalTransform(Fmatrix* M, void* graph = nullptr)
	{
		VERIFY(M);
		TargetGraph(graph).val_pLocalTransform = M;
	}
	virtual void set_UI(bool V, void* graph = nullptr) { TargetGraph(graph).val_bUI = V; }
	virtual void set_HUD(bool V, void* graph = nullptr) { TargetGraph(graph).val_bHUD = V; }
	virtual bool get_HUD(void* graph = nullptr) { return TargetGraph(graph).val_bHUD; }
	virtual void set_Invisible(bool V, void* graph = nullptr) { TargetGraph(graph).val_bInvisible = V; }
	virtual CDB::MODEL* GetHOMModel();
	virtual xr_vector<u32>* GetHOMInvaltids();

	// Render mode
	virtual void					rmNear						();
	virtual void					rmFar						();
	virtual void					rmNormal					();

	// Constructor/destructor/loader
	CRender														();
	virtual ~CRender											();

	xr_string						getShaderParams				();
	xr_string						getShaderParamsDebug		();

	void							addShaderOption				(const char* name, const char* value = "");
	void							clearAllShaderOptions		();

	bool							NeedMotionVectors			() const;
	bool							MotionVectorsDisabled		() const;
	void							SyncMotionVectors			();

	auto							ShaderOptionsCount			() { return m_ShaderOptions.size(); }

	virtual bool					InIndoor					() { return pLastSector!=pOutdoorSector; };
	virtual size_t					SectorsCount				() { return Sectors.size(); }

private:
	ShaderExternalMap				m_ShaderOptions;

protected:
	virtual	void					ScreenshotImpl				(ScreenshotMode mode, const char* name, CMemoryWriter* memory_writer);

private:
	FS_FileSet						m_file_set;
	void ReadVBChunk(xr_vector<IRHIBuffer*>& OutBuffer, xr_vector<VertexDeclarator>& DeclBuffer, u32 Count, IReaderBase& fs, xr_vector<u32>* OutBase = nullptr);
};

extern CRender						RImplementation;

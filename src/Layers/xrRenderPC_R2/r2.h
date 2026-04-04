#pragma once

#include "../xrRender/r__dsgraph_structure.h"
#include "../xrRender/r__occlusion.h"

#include "../xrRender/PSLibrary.h"

#include "../xrRender/r__types.h"
#include "r2_rendertarget.h"

#include "../xrRender/HOM.h"
#include "../xrRender/DetailManager.h"
#include "../xrRender/ModelPool.h"
#include "../xrRender/WallmarksEngine.h"

#include "SMAP_Allocator.h"
#include "../xrRender/Light_DB.h"
#include "../xrRender/LightTrack.h"
#include "../xrRender/r_sun_cascades.h"

#include "../../xrEngine/IRenderable.h"
#include "../../xrEngine/Fmesh.h"
#include <d3dcommon.h>


class dxRender_Visual;

// definition
class CRender													:	public R_dsgraph_structure
{
public:
	enum
	{
		PHASE_NORMAL	= 0,	// E[0]
		PHASE_SMAP		= 1,	// E[1]
	};

public:
	struct _options
	{
		u32		HW_smap_FORMAT		: 32;
		u32		smapsize			: 16;
		u32		mrtmixdepth			: 1;


		u32		nvstencil			: 1;
		u32		nvdbt				: 1;

		u32		nullrt				: 1;

		u32		distortion			: 1;
		u32		distortion_enabled	: 1;

		u32		sunstatic			: 1;
		u32		noshadows			: 1;
		u32		Tshadows			: 1;						// transluent shadows
		u32		disasm				: 1;

		u32		forcegloss			: 1;
		u32		forceskinw			: 1;
		float	forcegloss_v		;
	} o;

	Flags16 SSAO;

	struct _stats
	{
		u32		l_total,	l_visible;
		u32		l_shadowed,	l_unshadowed;
		s32		s_used,		s_merged,	s_finalclip;
		u32		o_queries,	o_culled;
		u32		ic_total,	ic_culled;
	} stats;
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

	// Global vertex-buffer container
	xr_vector<FSlideWindowItem>									SWIs;
	xr_vector<ref_shader>										ShadersLevel;
	xr_hash_map<shared_str, ref_shader> ShadersShared;
	using VertexDeclarator = FixedVector<RHIInputElementDesc, 65>;
	xr_vector<VertexDeclarator>									nDC,xDC;
	xr_vector<IRHIBuffer*>							nVB,xVB;
	xr_vector<IRHIBuffer*>							nIB,xIB;
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

public:
	void							render_main					(bool deffered, bool zfill = false);
	void							render_forward				();
	void							render_lights				(light_Package& LP	);
	void							render_menu					();
	void							render_sun_cascade			(u32 cascade_ind);
	void							init_cacades				();
	void							render_sun_cascades			();

public:
	ShaderElement*					rimp_select_sh_static		(dxRender_Visual	*pVisual, float cdist_sq);
	ShaderElement*					rimp_select_sh_dynamic		(dxRender_Visual	*pVisual, float cdist_sq, bool is_hud);
	RHIInputElementDesc*			getVB_Format(int id, size_t* Count, bool	_alt = false);
	IRHIBuffer*			getVB						(int id, bool	_alt=false);
	IRHIBuffer*			getIB						(int id, bool	_alt=false);
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
	IC u32							occq_get					(u32&	ID		)	{ return HWOCC.occq_get		(ID);	}

	ICF void						apply_object				(IRenderable*	O)
	{
		if (0==O)					return;
		if (0==O->renderable_ROS())	return;
		CROS_impl& LT				= *((CROS_impl*)O->renderable_ROS());
		LT.update_smooth			(O)								;
		o_hemi						= 0.75f*LT.get_hemi			()	;
		o_sun						= 0.75f*LT.get_sun			()	;
		CopyMemory(o_hemi_cube, LT.get_hemi_cube(), CROS_impl::NUM_FACES*sizeof(float));
	}
	IC void							apply_lmaterial				()
	{
		RHIShaderConstant*		C	= &*RCache.get_c	(c_sbase);		// get sampler
		if (0==C)			return;
		VERIFY				(RC_dest_sampler	== C->destination);
		VERIFY				(RC_sampler			== C->type);
		CTexture*		T	= RCache.get_ActiveTexture	(u32(C->samp.index));
		VERIFY				(T);
		float	mtl			= T->m_material;
#ifdef	DEBUG_DRAW
		if (ps_r2_ls_flags.test(R2FLAG_GLOBALMATERIAL))	mtl=ps_r2_gmaterial;
#endif
		RCache.hemi.set_material (o_hemi,o_sun,0,(mtl+.5f)/4.f);
		RCache.hemi.set_pos_faces(o_hemi_cube[CROS_impl::CUBE_FACE_POS_X],
								  o_hemi_cube[CROS_impl::CUBE_FACE_POS_Y],
								  o_hemi_cube[CROS_impl::CUBE_FACE_POS_Z]);
		RCache.hemi.set_neg_faces	(o_hemi_cube[CROS_impl::CUBE_FACE_NEG_X],
								 o_hemi_cube[CROS_impl::CUBE_FACE_NEG_Y],
								 o_hemi_cube[CROS_impl::CUBE_FACE_NEG_Z]);
	}

public:
	// feature level
	virtual	GenerationLevel			get_generation			()	{ return IRender_interface::GENERATION_R2; }

	virtual bool					is_sun_static			()	{ return o.sunstatic;}
	virtual DWORD					get_dx_level			()	{ return 0x00090000;}

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

	// Loading / Unloading
	virtual void					create						();
	virtual void					destroy						();
	virtual	void					reset_begin					();
	virtual	void					reset_end					();

	virtual	void					level_Load					(IReader*);
	virtual void					level_Unload				();

	IRHISurface* load_texture(const char* fname, u32& msize, bool bStaging = false) override;
	bool get_texture_metadata(const char* fname, RHITextureMetadata* p_data) override;

	virtual IDirect3DBaseTexture9*	texture_load			(const char*	fname, u32& msize);

	virtual HRESULT					shader_compile			(
		const char*							name,
		DWORD const*					pSrcData,
		UINT                            SrcDataLen,
		const char*                          pFunctionName,
		const char*                          pTarget,
		DWORD                           Flags,
		void*&							result);

	// Information
	virtual void					Statistics					(CGameFont* F);
	virtual const char*					getShaderPath				()									{ return "r2\\";	}
	virtual ref_shader				getShader					(int id);
	virtual ref_shader getShaderShared(shared_str id);
	virtual IRender_Sector*			getSector					(int id);
	virtual IRenderVisual*			getVisual					(int id);
	virtual IRender_Sector*			detectSector				(const Fvector& P);
	virtual IRender_Target*			getTarget					();

	// Main 
	virtual void					flush						();
	virtual void					set_Object					(IRenderable*		O	);
	virtual	void					add_Occluder				(Fbox2&	bb_screenspace	);			// mask screen region as oclluded
	virtual void					add_Visual					(IRenderVisual*	V, bool Ignore, bool Force = false);			// add visual leaf	(no culling performed at all)
	virtual void					add_Geometry				(IRenderVisual*	V	);			// add visual(s)	(all culling performed)

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
	virtual void					RenderUI					(bool a=false);

	virtual void					Screenshot					(ScreenshotMode mode=SM_NORMAL, const char* name = 0);
	virtual void					Screenshot					(ScreenshotMode mode, CMemoryWriter& memory_writer);
	virtual void					ScreenshotAsyncBegin		();
	virtual void					ScreenshotAsyncEnd			(CMemoryWriter& memory_writer);
	virtual void	_BCL			OnFrame						();

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


	auto							ShaderOptionsCount			() { return m_ShaderOptions.size(); }

	virtual bool					InIndoor					() { return pLastSector!=pOutdoorSector; };
	virtual size_t					SectorsCount				() { return Sectors.size(); }

private:
	xr_string_map<xr_string, xr_string>	m_ShaderOptions;

protected:
	virtual	void					ScreenshotImpl				(ScreenshotMode mode, const char* name, CMemoryWriter* memory_writer);

private:
	FS_FileSet						m_file_set;
	void ReadVBChunk(xr_vector<IRHIBuffer*>& OutBuffer, xr_vector<VertexDeclarator>& DeclBuffer, u32 Count, IReaderBase& fs);
};

extern CRender						RImplementation;

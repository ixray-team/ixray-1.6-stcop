#pragma once

#include "../xrRender/r__dsgraph_structure.h"

#include "../xrRender/PSLibrary.h"

#include "../xrRender/HOM.h"
#include "../xrRender/DetailManager.h"
#include "GlowManager.h"
#include "../xrRender/WallmarksEngine.h"
#include "FStaticRender_RenderTarget.h"
#include "../xrRender/ModelPool.h"

#include "LightShadows.h"
#include "LightProjector.h"
#include "LightPPA.h"
#include "../xrRender/Light_DB.h"
#include "../../xrEngine/FmeshRender.h"
#include <d3dcommon.h>

class dxRender_Visual;

// definition
class CRender:
	public R_dsgraph_structure
{
public:
	enum
	{
		PHASE_NORMAL,
		PHASE_POINT,
		PHASE_SPOT
	};

	struct _options
	{
		u32 vis_intersect		: 1;	// config
		u32 distortion			: 1;	// run-time modified
		u32 color_mapping		: 1;	// true if SM 1.4 and higher
		u32 disasm				: 1;	// config
		u32 forceskinw			: 1;	// config
		u32 no_detail_textures	: 1;	// config
	} o;

	struct _stats
	{
		u32		o_queries,	o_culled;
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
	
	// Global containers
	xr_vector<ref_shader> ShadersLevel;
	xr_hash_map<shared_str, ref_shader> ShadersShared;
	GeomData GlobalData;
	xr_hash_map<shared_str, GeomData> MUData;
	xr_vector<dxRender_Visual*>									Visuals;
	CPSLibrary													PSLibrary;

	CLight_DB*													L_DB;
	CLightR_Manager*											L_Dynamic;
	CLightShadows*												L_Shadows;
	CLightProjector*											L_Projector;
	CGlowManager*												L_Glows;
	CWallmarksEngine*											Wallmarks;
	CDetailManager*												Details;
	CModelPool*													Models;

	CRenderTarget*												Target;			// Render-target

	// R1-specific global constants
	Fmatrix														r1_dlight_tcgen			;
	light*														r1_dlight_light			;
	float														r1_dlight_scale			;
	cl_light_PR													r1_dlight_binder_PR		;
	cl_light_C													r1_dlight_binder_color	;
	cl_light_XFORM												r1_dlight_binder_xform	;
	shared_str													c_ldynamic_props		;
	bool														m_bMakeAsyncSS;
	bool														m_bFirstFrameAfterReset;	// Determines weather the frame is the first after resetting device.

	xr_list<light*>												v_all_lights_dque;
private:
	virtual GeomData& GetMUSlot(shared_str Name) override {return MUData[Name];}
	
	// Loading / Unloading
	void LoadVisuals(IReader *fs);
	void LoadLights(IReader *fs);
	void LoadSectors(IReader *fs);
	void LoadVertexBuffers(IReaderBase& fs);
	void LoadIndexBuffers(IReaderBase& fs);
	void LoadSWIs(IReaderBase& fs);

public:
	ShaderElement* rimp_select_sh_static(dxRender_Visual	*pVisual, float cdist_sq);
	ShaderElement* rimp_select_sh_dynamic(dxRender_Visual	*pVisual, float cdist_sq, bool is_hud);
	RHIInputElementDesc* getVB_Format(int id, size_t* Count);
	IRHIBuffer* getVB(int id);
	IRHIBuffer* getIB(int id);
	FSlideWindowItem* getSWI(int id);
	IRender_Portal* getPortal(int id);
	IRender_Sector* getSectorActive();
	IRenderVisual* model_CreatePE(str_c name);
	void ApplyBlur4(FVF::TL4uv*	dest, u32 w, u32 h, float k);
	void apply_object(IRenderable*	O);
	IC void apply_lmaterial() {};
public:
	// feature level
	virtual	GenerationLevel			get_generation			() override	{ return IRender_interface::GENERATION_R1; }
	virtual DWORD					get_dx_level			() override	{ return 0x00090000; }

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

	virtual bool					is_sun_static			() override {return true;}

	// Loading / Unloading
	virtual	void					create					() override;
	virtual	void					destroy					() override;
	virtual	void					reset_begin				() override;
	virtual	void					reset_end				() override;

	virtual	void					level_Load				(IReader*) override;
	virtual void					level_Unload			() override;
	
	IRHISurface* load_texture(str_c fname, u32& msize, bool bStaging = false) override;
	bool get_texture_metadata(str_c absolute_path, RHITextureMetadata* p_data) override;
	virtual IDirect3DBaseTexture9*	texture_load			(str_c	fname, u32& msize);

	virtual HRESULT					shader_compile			 (
		str_c							name,
		DWORD const*                    pSrcData,
		UINT                            SrcDataLen,
		str_c                          pFunctionName,
		str_c                          pTarget,
		DWORD                           Flags,
		void*&							result
	) override;

	// Information
	virtual void					Statistics				(CGameFont* F) override;
	virtual str_c					getShaderPath			() override									{ return "r1\\";	}
	virtual ref_shader				getShader				(int id);
	virtual ref_shader getShaderShared(shared_str id);
	virtual IRender_Sector*			getSector				(int id) override;
	virtual IRenderVisual*			getVisual				(int id) override;
	virtual IRender_Sector*			detectSector			(const Fvector& P) override;
	IRender_Sector*					detectLastSector		(const Fvector& P);
	IRender_Sector*					detectSector			(const Fvector& P, Fvector& D);
	int								translateSector			(IRender_Sector* pSector);
	virtual IRender_Target*			getTarget				() override;
	virtual SurfaceParams getSurface(const char* nameTexture) override;

	// Main 
	virtual void					flush					() override;
	virtual void					set_Object				(IRenderable*		O	) override;
	virtual	void					add_Occluder			(Fbox2&	bb_screenspace	) override;			// mask screen region as oclluded
	virtual void					add_Visual				(IRenderVisual*	V, bool Ignore, bool Force = false) override;			// add visual leaf (no culling performed at all)
	virtual void					add_Geometry			(IRenderVisual*	V	) override;			// add visual(s)	(all culling performed)

	// wallmarks
	virtual void					add_StaticWallmark		(ref_shader& S, const Fvector& P, float s, const CDB::TRI& T, Fvector* V, bool UseCameraDirection = false);
	virtual void					add_StaticWallmark		(IWallMarkArray* pArray, const Fvector& P, float s, const CDB::TRI& T, Fvector* V, bool UseCameraDirection = false) override;
	virtual void					add_StaticWallmark		(const wm_shader& S, const Fvector& P, float s, const CDB::TRI& T, Fvector* V) override;
	virtual void					clear_static_wallmarks	() override;
	virtual StaticWallmarkHandle::WallmarkHandlePtr add_DynamicWallmark		(const wm_shader& S, const Fvector& P, float w, float h, float r, const CDB::TRI& T, Fvector* V) override;
	virtual void					add_SkeletonWallmark	(intrusive_ptr<CSkeletonWallmark> wm);
	virtual void					add_SkeletonWallmark	(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size);
	virtual void					add_SkeletonWallmark		(const Fmatrix* xf, IKinematics* obj, IWallMarkArray *pArray, const Fvector& start, const Fvector& dir, float size) override;
	
	//
	virtual IBlender*				blender_create			(CLASS_ID cls);

	//
	virtual IRender_ObjectSpecific*	ros_create				(IRenderable* parent) override;
	virtual void					ros_destroy				(IRender_ObjectSpecific* &) override;

	// Particle library
	virtual CPSLibrary*				ps_library				(){return &PSLibrary;}

	// Lighting
	virtual IRender_Light*			light_create			() override;
	virtual IRender_Glow*			glow_create				() override;
	
	// Models
	virtual IRenderVisual*			model_CreateParticles	(str_c name) override;
	virtual IRender_DetailModel*	model_CreateDM			(IReader*F);
	virtual IRenderVisual*			model_Create			(str_c name, IReader*data=nullptr) override;
	virtual IRenderVisual*			model_CreateChild		(str_c name, IReader*data) override;
	virtual IRenderVisual*			model_GetPrototype		(str_c name) override;
	virtual CDB::MODEL*				model_GetPrototypeCollision(str_c name) override;
	virtual IRenderVisual*			model_Duplicate			(IRenderVisual*	V) override;
	virtual void					model_Delete			(IRenderVisual* &	V, bool bDiscard) override;
	virtual void					model_Delete_Deffered	(IRenderVisual* &	V) override;
	virtual void 					model_Delete			(IRender_DetailModel* & F);
	virtual void					models_Prefetch			() override;
	virtual void					models_Clear			(bool b_complete) override;
	
	// Occlusion culling
	virtual bool					occ_visible				(vis_data&	V) override;
	virtual bool					occ_visible				(Fbox&		B) override;
	virtual bool					occ_visible				(sPoly&		P) override;
	
	// Main
	virtual void					Calculate				() override;
	virtual void					Render					() override;
	virtual void					RenderUI				(bool=false) override;

	virtual void					Screenshot				(ScreenshotMode mode=SM_NORMAL, str_c name = nullptr) override;
	virtual void					Screenshot				(ScreenshotMode mode, CMemoryWriter& memory_writer) override;
	virtual void					ScreenshotAsyncBegin	() override;
	virtual void					ScreenshotAsyncEnd		(CMemoryWriter& memory_writer) override;
	virtual void	_BCL			OnFrame					() override;
	
	// Render mode
	virtual void					rmNear					() override;
	virtual void					rmFar					() override;
	virtual void					rmNormal				() override;

	virtual void ReadVBChunk(xr_vector<IRHIBuffer*>& OutBuffer, xr_vector<VertexDeclarator>& DeclBuffer, u32 Count, IReaderBase& fs) override;
	virtual void ReadIBChunk(xr_vector<IRHIBuffer*>& OutBuffer, IReaderBase& fs) override;
	virtual void ReadSWIsChunk(xr_vector<FSlideWindowItem>& SWIs, IReaderBase& fs) override;

	// Constructor/destructor/loader
	CRender													();
	virtual ~CRender										() override;

	xr_string						getShaderParams			();
	xr_string						getShaderParamsDebug	();

	void							addShaderOption			(const char* name, const char* value = "");
	void							clearAllShaderOptions	();

	auto							ShaderOptionsCount		() const { return m_ShaderOptions.size(); }

	virtual bool					InIndoor				() override { return pLastSector!=pOutdoorSector; };
	virtual size_t					SectorsCount			() override { return Sectors.size(); }

private:
	xr_string_map<xr_string, xr_string>	m_ShaderOptions;
protected:
	virtual	void					ScreenshotImpl			(ScreenshotMode mode, str_c name, CMemoryWriter* memory_writer) override;

private:
	FS_FileSet						m_file_set;
};

extern CRender						RImplementation;

//---------------------------------------------------------------------------
#pragma once

#include "..\..\..\xrCDB\frustum.h"
#include "..\..\..\XrEngine\vis_common.h"
#include "..\..\..\XrEngine\Render.h"

#include "../../../Layers/xrRender/blenders\blender.h"
#include "../../../Layers/xrRender/blenders\blender_clsid.h"
#include "../../../Layers/xrRender/xrRender_console.h"
#include "../../../Layers/xrRender/PSLibrary.h"
#include "../../../Layers/xrRender/IRenderDetailModel.H"
#include "../../../Layers/xrRender/DetailModel.H"
#include "../../../Layers/xrRender/ModelPool.h"
#include "../../../Layers/xrRender/SkeletonCustom.h"
#include "../../../xrCore/API/xrAPI.h"
#include <d3dcompiler.h>
#include "../../../Layers/xrRender/light.h"
#include "../Render/LightSpot.h"

class ISpatial;

class CBlender_accum : public IBlender {
public:
	virtual LPCSTR getComment() {
		return "INTERNAL: accumulate light";
	}

	virtual BOOL canBeDetailed() {
		return FALSE;
	}

	virtual BOOL canBeLMAPped() {
		return FALSE;
	}

	virtual void Compile(CBlender_Compile& C);

	CBlender_accum() {
		description.CLS = 0;
	};

	virtual ~CBlender_accum() {};
};

// definition (Renderer)
class CRenderTarget :public IRender_Target
{
public:
	CRenderTarget();
	virtual ~CRenderTarget();

	// 2D texgen (texture adjustment matrix)
	void	u_compute_texgen_screen(Fmatrix& m_Texgen) {
		float	_w = float(RCache.get_width());
		float	_h = float(RCache.get_height());
		float	o_w = (.5f / _w);
		float	o_h = (.5f / _h);

		Fmatrix			m_TexelAdjust =
		{
			0.5f,				0.0f,				0.0f,			0.0f,
			0.0f,				-0.5f,				0.0f,			0.0f,
			0.0f,				0.0f,				1.0f,			0.0f,
			0.5f + o_w,			0.5f + o_h,			0.0f,			1.0f
		};
		m_Texgen.mul(m_TexelAdjust, RCache.xforms.m_wvp);
	}

	void reset_light_marker(bool bResetStencil = false) {
		dwLightMarkerID = 5;

		if(bResetStencil) {
			CHK_DX(RDevice->Clear(0L, nullptr, D3DCLEAR_STENCIL, 0x0, 1.0f, 0L));
		}
	}

	void increment_light_marker() {
		dwLightMarkerID += 2;

		if(dwLightMarkerID > 255)
			reset_light_marker(true);
	}

	void accum_point_geom_create();
	void accum_point_geom_destroy();
	void accum_spot_geom_create();
	void accum_spot_geom_destroy();

	void accum_spot(light*);
	void accum_point(light*);
	void draw_volume(light*);

	u32 dwLightMarkerID = 0;

	ref_shader s_accum;
	IBlender* b_accum;

	ref_geom g_accum_point;
	ref_geom g_accum_spot;

	IDirect3DVertexBuffer9* g_accum_point_vb;
	IDirect3DIndexBuffer9* g_accum_point_ib;

	IDirect3DVertexBuffer9* g_accum_spot_vb;
	IDirect3DIndexBuffer9* g_accum_spot_ib;

	virtual	void					set_blur(float	f) {}
	virtual	void					set_gray(float	f) {}
	virtual void					set_duality_h(float	f) {}
	virtual void					set_duality_v(float	f) {}
	virtual void					set_noise(float	f) {}
	virtual void					set_noise_scale(float	f) {}
	virtual void					set_noise_fps(float	f) {}
	virtual void					set_color_base(u32	f) {}
	virtual void					set_color_gray(u32	f) {}
	//virtual void					set_color_add		(u32	f)							= 0;
	virtual void					set_color_add(const Fvector& f) {}
	virtual void					set_cm_imfluence(float	f) {}
	virtual void					set_cm_interpolate(float	f) {}
	virtual void					set_cm_textures(const shared_str& tex0, const shared_str& tex1) {}

	virtual u32			get_width			()				{ return EDevice->TargetWidth;	}
	virtual u32			get_height			()				{ return EDevice->TargetHeight;	}

	virtual u32						get_target_width()	{ return EDevice->TargetWidth;	}
	virtual u32						get_target_height() { return EDevice->TargetHeight;	}
	virtual u32						get_core_width()	{ return EDevice->TargetWidth;	}
	virtual u32						get_core_height()	{ return EDevice->TargetHeight;	}

	CTexture* t_envmap_0;	// env-0
	CTexture* t_envmap_1;	// env-1
};

class	ECORE_API CRender : public IRender_interface
{
	CRenderTarget* Target;
	Fmatrix					current_matrix;
	BOOL val_bInvisible;
	u32 dwFrameCalc = 0;
public:
	// options

	// Data
	CFrustum				ViewBase;
	CPSLibrary				PSLibrary;

	CModelPool* Models;

	xr_vector<light*> m_pointlights;
	xr_vector<light*> m_spotlights;
	CLight_Compute_XFORM_and_VIS LR;

public:
	// Occlusion culling
	virtual BOOL			occ_visible(Fbox& B);
	virtual BOOL			occ_visible(sPoly& P);
	virtual BOOL			occ_visible(vis_data& P);

	// Constructor/destructor
	CRender();
	virtual 				~CRender();


	void 					Initialize();
	void 					ShutDown();

	void					OnDeviceCreate();
	void					OnDeviceDestroy();

	virtual	void					Calculate();
	virtual void					Render();

	virtual void					set_Transform(Fmatrix* M);
	virtual void					add_Visual(IRenderVisual* visual, bool ignore_opt = false);

	virtual ref_shader		getShader(int id);
	virtual	CRenderTarget* getTarget() { return Target; }
	//.	virtual IRender_Target*	getTarget		(){return Target;}


	void					reset_begin();
	void					reset_end();
	virtual IRenderVisual* model_Create(LPCSTR name, IReader* data = 0);
	virtual IRenderVisual* model_CreateChild(LPCSTR name, IReader* data);
	virtual IRenderVisual* model_CreatePE(LPCSTR name);
	virtual IRenderVisual* model_CreateParticles(LPCSTR name);

	virtual IRender_DetailModel* model_CreateDM(IReader* R);
	virtual IRenderVisual* model_Duplicate(IRenderVisual* V);
	virtual void			model_Delete(IRenderVisual*& V, BOOL bDiscard = TRUE);
	virtual void			model_Delete(IRender_DetailModel*& F)
	{
		if (F)
		{
			CDetail* D = (CDetail*)F;
			D->Unload();
			xr_delete(D);
			F = NULL;
		}
	}
	void 					model_Render(IRenderVisual* m_pVisual, const Fmatrix& mTransform, int priority, bool strictB2F, float m_fLOD);
	void 					model_RenderSingle(IRenderVisual* m_pVisual, const Fmatrix& mTransform, float m_fLOD);
	virtual	GenerationLevel	get_generation() { return GENERATION_R2; }
	virtual bool			is_sun_static() { return true; };

	virtual void			add_SkeletonWallmark(intrusive_ptr<CSkeletonWallmark> wm) {};
	virtual void			add_SkeletonWallmark(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size) {};

	virtual void			add_SkeletonWallmark(const Fmatrix* xf, IKinematics* obj, IWallMarkArray* pArray, const Fvector& start, const Fvector& dir, float size) {}
	// Render mode
	virtual void			rmNear();
	virtual void			rmFar();
	virtual void			rmNormal();

	IC void apply_lmaterial() {
		R_constant* C = &*RCache.get_c("s_base"); // get sampler
		if(0 == C)			return;
		VERIFY(RC_dest_sampler == C->destination);
		VERIFY(RC_sampler == C->type);
		CTexture* T = RCache.get_ActiveTexture(u32(C->samp.index));
		VERIFY(T);
		float	mtl = T->m_material;
#ifdef	DEBUG_DRAW
		if(ps_r2_ls_flags.test(R2FLAG_GLOBALMATERIAL))	mtl = ps_r2_gmaterial;
#endif
		RCache.hemi.set_material(0.7, 1, 0, (mtl + .5f) / 4.f);
	}

	virtual LPCSTR			getShaderPath()
	{
		return "editor\\";
	}


	virtual IDirect3DBaseTexture9* texture_load(LPCSTR	fname, u32& mem_size);

	virtual DWORD					get_dx_level();

	// Loading / Unloading
	virtual	void					create();
	virtual	void					destroy();

		virtual	void					level_Load(IReader*);
	virtual void					level_Unload();

	//virtual IDirect3DBaseTexture9*	texture_load			(LPCSTR	fname, u32& msize)					= 0;

	// Information
	virtual	void					Statistics(CGameFont* F) {};

	//	virtual ref_shader				getShader				(int id)									= 0;
	virtual IRender_Sector* getSector(int id);
	virtual IRenderVisual* getVisual(int id);
	virtual IRender_Sector* detectSector(const Fvector& P);

	// Main 
	IC		void					set_Frustum(CFrustum* O) { VERIFY(O);	View = O; }
	virtual void					set_HUD(BOOL 		V);
	virtual BOOL					get_HUD();
	virtual void					set_Invisible(BOOL 		V);
	virtual void					flush();
	virtual void					set_Object(IRenderable* O);
	virtual	void					add_Occluder(Fbox2& bb_screenspace);	// mask screen region as oclluded (-1..1, -1..1)
	virtual void					add_Geometry(IRenderVisual* V);	// add visual(s)	(all culling performed)
//	virtual void					add_StaticWallmark		(ref_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V)=0;
	virtual void					add_StaticWallmark(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V) {}
	//	Prefer this function when possible
	virtual void					add_StaticWallmark(IWallMarkArray* pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V) {}
	virtual void					clear_static_wallmarks() {}
	//virtual void					add_SkeletonWallmark	(intrusive_ptr<CSkeletonWallmark> wm)						= 0;
	//virtual void					add_SkeletonWallmark	(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size)=0;

	//virtual IBlender*				blender_create			(CLASS_ID cls)								= 0;
	//virtual void					blender_destroy			(IBlender* &)								= 0;

	virtual IRender_ObjectSpecific* ros_create(IRenderable* parent);
	virtual void					ros_destroy(IRender_ObjectSpecific*&);

	// Lighting/glowing
	virtual IRender_Light* light_create();
	virtual void					light_destroy(IRender_Light* p_);
	virtual IRender_Glow* glow_create();
	virtual void					glow_destroy(IRender_Glow* p_);

	// Models
	virtual void					model_Logging(BOOL bEnable);
	virtual void					models_Prefetch();
	virtual void					models_Clear(BOOL b_complete);

	// Main

	virtual void					Screenshot(ScreenshotMode mode = SM_NORMAL, LPCSTR name = 0);
	virtual	void					Screenshot(ScreenshotMode mode, CMemoryWriter& memory_writer);
	virtual void					ScreenshotAsyncBegin();
	virtual void					ScreenshotAsyncEnd(CMemoryWriter& memory_writer);

	// Render mode
	virtual u32						memory_usage();

	xr_string getShaderParams() {
		xr_string params = "";
		if(!m_ShaderOptions.empty()) {
			params.append("(").append(m_ShaderOptions[0].Name);

			for(auto i = 1u; i < m_ShaderOptions.size(); ++i) {
				params.append(",").append(m_ShaderOptions[i].Name);
			}

			params.append(")");
		}
		return params;
	};

	void addShaderOption(const char* name, const char* value) {
		m_ShaderOptions.emplace_back(name, value);
	};

	void clearAllShaderOptions() {
		m_ShaderOptions.resize(0);
	}

protected:
	xr_vector<D3D_SHADER_MACRO> m_ShaderOptions;

	virtual	void					ScreenshotImpl(ScreenshotMode mode, LPCSTR name, CMemoryWriter* memory_writer) {};
	HRESULT					shader_compile(
		LPCSTR							name,
		DWORD const* pSrcData,
		UINT                            SrcDataLen,
		LPCSTR                          pFunctionName,
		LPCSTR                          pTarget,
		DWORD                           Flags,
		void*& result
	) override;
	private:
		xr_vector<ISpatial*> lstRenderables;
};
#ifdef REDITOR
#include "ui_main.h"
IC  float   CalcSSA(Fvector& C, float R)
{
    float distSQ  = UI->CurrentView().m_Camera.GetPosition().distance_to_sqr(C);
    return  R*R/distSQ;
}
#endif
extern ECORE_API CRender  	RImplementation;
//.extern ECORE_API CRender*	Render;


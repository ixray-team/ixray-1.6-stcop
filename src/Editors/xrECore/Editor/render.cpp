#include "stdafx.h"
#pragma hdrstop

#include "render.h"
#include "../Layers/xrRender/ResourceManager.h"
#include "../../../Include/xrAPI/xrAPI.h"
#include "../../xrEngine/irenderable.h"
#include "../../xrEngine/xr_object.h"
#include "../../xrEngine/CustomHUD.h"
//---------------------------------------------------------------------------
float ssaDISCARD		= 4.f;
float ssaDONTSORT		= 32.f;

ECORE_API float r_ssaDISCARD;
ECORE_API float	g_fSCREEN;

CRender   			RImplementation;

//---------------------
//---------------------------------------------------------------------------
CRender::CRender	()
{
	val_bInvisible = FALSE;
	::Render = &RImplementation;
	m_skinning					= 0;
}

CRender::~CRender	()
{
	xr_delete		(Target);
}

void					CRender::Initialize				()
{
	PSLibrary.OnCreate			();
}
void					CRender::ShutDown				()
{
	PSLibrary.OnDestroy			();
}

void					CRender::OnDeviceCreate			()
{
	Models						= xr_new<CModelPool>	();
    Models->Logging				(FALSE);
}
void					CRender::OnDeviceDestroy		()
{
	xr_delete					(Models);
}

ref_shader	CRender::getShader	(int id){ return 0; }//VERIFY(id<int(Shaders.size()));	return Shaders[id];	}

BOOL CRender::occ_visible(Fbox&	B)
{
	u32 mask		= 0xff;
	return ViewBase.testAABB(B.data(),mask);
}

BOOL CRender::occ_visible(sPoly& P)
{
	return ViewBase.testPolyInside(P);
}

BOOL CRender::occ_visible(vis_data& P)
{
	return occ_visible(P.box);
}

void CRender::Calculate()
{
	// Transfer to global space to avoid deep pointer access
	g_fSCREEN						=	float(EDevice->TargetWidth*EDevice->TargetHeight);
	r_ssaDISCARD					=	(ssaDISCARD*ssaDISCARD)/g_fSCREEN;
//	r_ssaLOD_A						=	(ssaLOD_A*ssaLOD_A)/g_fSCREEN;
//	r_ssaLOD_B						=	(ssaLOD_B*ssaLOD_B)/g_fSCREEN;
	lstRenderables.clear();
	ViewBase.CreateFromMatrix		(EDevice->mFullTransform,FRUSTUM_P_LRTB|FRUSTUM_P_FAR);
	{
		g_SpatialSpace->q_frustum
		(
			lstRenderables,
			ISpatial_DB::O_ORDERED,
			STYPE_RENDERABLE + STYPE_LIGHTSOURCE,
			ViewBase
		);

		// Exact sorting order (front-to-back)
	

		// Determine visibility for dynamic part of scene
		set_Object(0);
		if (g_hud)
		{
			g_hud->Render_First();	// R1 shadows
			g_hud->Render_Last();
		}
		u32 uID_LTRACK = 0xffffffff;
		/*if (phase == PHASE_NORMAL)*/
	/*	{
			uLastLTRACK++;
			if (lstRenderables.size())		uID_LTRACK = uLastLTRACK % lstRenderables.size();

			// update light-vis for current entity / actor
			CObject* O = g_pGameLevel->CurrentViewEntity();
			if (O) {
				CROS_impl* R = (CROS_impl*)O->ROS();
				if (R)		R->update(O);
			}
		}*/
		for (ISpatial* pSpatial : lstRenderables)
		{
			IRenderable* renderable = pSpatial->dcast_Renderable();
			if (!renderable)
				continue; 
			if (!(pSpatial->spatial.type & STYPE_RENDERABLE)) 	continue;

			set_Object(renderable);
			renderable->renderable_Render();
			set_Object(nullptr);
		}
	}
}

#include "../xrEngine/IGame_Persistent.h"
void CRender::Render()
{
	
}

IRender_DetailModel*	CRender::model_CreateDM(IReader* F)
{
	VERIFY				(F);
	CDetail*	D		= xr_new<CDetail> ();
	D->Load				(F);
	return D;
}

IRenderVisual*	CRender::model_CreatePE(LPCSTR name)
{
	PS::CPEDef*	source		= PSLibrary.FindPED	(name);
	return Models->CreatePE	(source);
}

IRenderVisual*			CRender::model_CreateParticles	(LPCSTR name)
{
	PS::CPEDef*	SE		= PSLibrary.FindPED	(name);
	if (SE) return		Models->CreatePE	(SE);
	else{
		PS::CPGDef*	SG	= PSLibrary.FindPGD	(name);
		return			SG?Models->CreatePG	(SG):0;
	}
}

void	CRender::rmNear		()
{
	CRenderTarget* T	=	getTarget	();
	D3DVIEWPORT9 VP		=	{0,0,T->get_width(),T->get_height(),0,0.02f };
	CHK_DX				(REDevice->SetViewport(&VP));
}
void	CRender::rmFar		()
{
	CRenderTarget* T	=	getTarget	();
	D3DVIEWPORT9 VP		=	{0,0,T->get_width(),T->get_height(),0.99999f,1.f };
	CHK_DX				(REDevice->SetViewport(&VP));
}
void	CRender::rmNormal	()
{
	CRenderTarget* T	=	getTarget	();
	D3DVIEWPORT9 VP		= {0,0,T->get_width(),T->get_height(),0,1.f };
	CHK_DX				(REDevice->SetViewport(&VP));
}

void 	CRender::set_Transform	(Fmatrix* M)
{
	current_matrix.set(*M);
}
#include <d3dx9.h>
void			CRender::add_Visual   		(IRenderVisual* visual)			{ if (val_bInvisible)		return; Models->RenderSingle	(dynamic_cast<dxRender_Visual*>(visual),current_matrix,1.f);}
IRenderVisual*	CRender::model_Create		(LPCSTR name, IReader* data)		{ return Models->Create(name,data);		}
IRenderVisual*	CRender::model_CreateChild	(LPCSTR name, IReader* data)		{ return Models->CreateChild(name,data);}
void 			CRender::model_Delete(IRenderVisual*& V, BOOL bDiscard) { auto v = dynamic_cast<dxRender_Visual*>(V); Models->Delete(v, bDiscard); if (v == nullptr)V = nullptr; }
IRenderVisual*	CRender::model_Duplicate	(IRenderVisual* V)					{ return Models->Instance_Duplicate(dynamic_cast<dxRender_Visual*>(V));	}
void 			CRender::model_Render		(IRenderVisual* m_pVisual, const Fmatrix& mTransform, int priority, bool strictB2F, float m_fLOD){Models->Render(dynamic_cast<dxRender_Visual*>(m_pVisual), mTransform, priority, strictB2F, m_fLOD);}
void 			CRender::model_RenderSingle	(IRenderVisual* m_pVisual, const Fmatrix& mTransform, float m_fLOD){Models->RenderSingle(dynamic_cast<dxRender_Visual*>(m_pVisual), mTransform, m_fLOD);}

//#pragma comment(lib,"d3dx_r1")
HRESULT	CRender::CompileShader			(
		LPCSTR                          pSrcData,
		UINT                            SrcDataLen,
		void*							_pDefines,
		void*							_pInclude,
		LPCSTR                          pFunctionName,
		LPCSTR                          pTarget,
		DWORD                           Flags,
		void*							_ppShader,
		void*							_ppErrorMsgs,
		void*							_ppConstantTable)
{
        CONST D3DXMACRO*                pDefines		= (CONST D3DXMACRO*)	_pDefines;
        LPD3DXINCLUDE                   pInclude		= (LPD3DXINCLUDE)		_pInclude;
        LPD3DXBUFFER*                   ppShader		= (LPD3DXBUFFER*)		_ppShader;
        LPD3DXBUFFER*                   ppErrorMsgs		= (LPD3DXBUFFER*)		_ppErrorMsgs;
        LPD3DXCONSTANTTABLE*            ppConstantTable	= (LPD3DXCONSTANTTABLE*)_ppConstantTable;
		return D3DXCompileShader		(pSrcData,SrcDataLen,pDefines,pInclude,pFunctionName,pTarget,Flags,ppShader,ppErrorMsgs,ppConstantTable);
}
HRESULT	CRender::shader_compile			(
		LPCSTR							name,
		LPCSTR                          pSrcData,
		UINT                            SrcDataLen,
		void*							_pDefines,
		void*							_pInclude,
		LPCSTR                          pFunctionName,
		LPCSTR                          pTarget,
		DWORD                           Flags,
		void*							_ppShader,
		void*							_ppErrorMsgs,
		void*							_ppConstantTable)
{
	D3DXMACRO						defines			[128];
	int								def_it			= 0;
	CONST D3DXMACRO*                pDefines		= (CONST D3DXMACRO*)	_pDefines;
	if (pDefines)	{
		// transfer existing defines
		for (;;def_it++)	{
			if (0==pDefines[def_it].Name)	break;
			defines[def_it]			= pDefines[def_it];
		}
	}
	// options
	if (m_skinning<0)		{
		defines[def_it].Name		=	"SKIN_NONE";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	if (0==m_skinning)		{
		defines[def_it].Name		=	"SKIN_0";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	if (1==m_skinning)		{
		defines[def_it].Name		=	"SKIN_1";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	if (2==m_skinning)		{
		defines[def_it].Name		=	"SKIN_2";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	if (3 == m_skinning) {
		defines[def_it].Name = "SKIN_3";
		defines[def_it].Definition = "1";
		def_it++;
	}
	if (4 == m_skinning) {
		defines[def_it].Name = "SKIN_4";
		defines[def_it].Definition = "1";
		def_it++;
	}
	// finish
	defines[def_it].Name			=	0;
	defines[def_it].Definition		=	0;
	def_it							++;

	LPD3DXINCLUDE                   pInclude		= (LPD3DXINCLUDE)		_pInclude;
	LPD3DXBUFFER*                   ppShader		= (LPD3DXBUFFER*)		_ppShader;
	LPD3DXBUFFER*                   ppErrorMsgs		= (LPD3DXBUFFER*)		_ppErrorMsgs;
	LPD3DXCONSTANTTABLE*            ppConstantTable	= (LPD3DXCONSTANTTABLE*)_ppConstantTable;
//.	return D3DXCompileShader		(pSrcData,SrcDataLen,defines,pInclude,pFunctionName,pTarget,Flags,ppShader,ppErrorMsgs,ppConstantTable);
#ifdef D3DXSHADER_USE_LEGACY_D3DX9_31_DLL //	December 2006 and later
	HRESULT		_result	= D3DXCompileShader(pSrcData,SrcDataLen,defines,pInclude,pFunctionName,pTarget,Flags|D3DXSHADER_USE_LEGACY_D3DX9_31_DLL,ppShader,ppErrorMsgs,ppConstantTable);
#else
	HRESULT		_result	= D3DXCompileShader(pSrcData,SrcDataLen,defines,pInclude,pFunctionName,pTarget,Flags,ppShader,ppErrorMsgs,ppConstantTable);
#endif
	return _result;
}

void					CRender::reset_begin			()
{
	xr_delete			(Target);
}
void					CRender::reset_end				()
{
	Target			=	xr_new<CRenderTarget>			();
}

void CRender::set_HUD(BOOL V)
{
}

BOOL CRender::get_HUD()
{
	return 0;
}

void CRender::set_Invisible(BOOL V)
{
	val_bInvisible = V;
}


DWORD CRender::get_dx_level()
{
	return 90;
}

void CRender::create()
{

}
void CRender::destroy()
{

}

void CRender::level_Load(IReader*)
{

}
void CRender::level_Unload()
{

}

// IDirect3DBaseTexture9*	texture_load			(LPCSTR	fname, u32& msize)					= 0;



//	 ref_shader				getShader				(int id)									= 0;
IRender_Sector* CRender::getSector(int id)
{
	return nullptr;
 }
IRenderVisual* CRender::getVisual(int id)
{
	return nullptr;
}
IRender_Sector* CRender::detectSector(const Fvector& P)
{
	return nullptr;
}

void CRender::flush() {}
void CRender::set_Object(IRenderable* O) {}
void CRender::add_Occluder(Fbox2& bb_screenspace) {}
void CRender::add_Geometry(IRenderVisual* V) {}
class RenderObjectSpecific :public IRender_ObjectSpecific
{
public:
	RenderObjectSpecific() {}
	virtual ~RenderObjectSpecific() {}

	virtual	void						force_mode(u32 mode)
	{}
	virtual float						get_luminocity() { return 1; }
	virtual float						get_luminocity_hemi() { return 1; }
	virtual float* get_luminocity_hemi_cube() {
		static float test[8] = {};
		return test;
	}

};
 IRender_ObjectSpecific* CRender::ros_create(IRenderable* parent) { return xr_new< RenderObjectSpecific>(); }
 void CRender::ros_destroy(IRender_ObjectSpecific*& a) { xr_delete(a); }
 class RLight : public IRender_Light
 {
 public:
 public:
	 virtual void set_type(LT type) {}
	 virtual void set_active(bool) {}
	 virtual bool get_active() { return false; }
	 virtual void set_shadow(bool) {}
	 virtual void set_volumetric(bool) {}
	 virtual void set_volumetric_quality(float) {}
	 virtual void set_volumetric_intensity(float) {}
	 virtual void set_volumetric_distance(float) {}
	 virtual void set_indirect(bool) {};
	 virtual void set_position(const Fvector& P) {}
	 virtual void set_rotation(const Fvector& D, const Fvector& R) {}
	 virtual void set_cone(float angle) {}
	 virtual void set_range(float R) {}
	 virtual void set_virtual_size(float R) {}
	 virtual void set_texture(LPCSTR name) {}
	 virtual void set_color(const Fcolor& C) {}
	 virtual void set_color(float r, float g, float b) {}
	 virtual void set_hud_mode(bool b) {}
	 virtual bool get_hud_mode() {
		 return false;
	 }
	 virtual ~RLight() {}
 };
 IRender_Light* CRender::light_create() { return xr_new< RLight>(); }
 void CRender::light_destroy(IRender_Light* p_) {  }



 class RGlow : public IRender_Glow
 {
 public:
 public:
	 RGlow() {}
	 virtual	~RGlow() {}

	 virtual void					set_active(bool) {}
	 virtual bool					get_active() { return false; }
	 virtual void					set_position(const Fvector& P) { return ; }
	 virtual void					set_direction(const Fvector& P) { return ; }
	 virtual void					set_radius(float			R) { return ; }
	 virtual void					set_texture(LPCSTR			name) { return ; }
	 virtual void					set_color(const Fcolor& C) { return ; }
	 virtual void					set_color(float r, float g, float b) { return ; }
	 virtual void					spatial_move() { return ; }
 };

 IRender_Glow* CRender::glow_create() { return xr_new< RGlow>(); }
 void CRender::glow_destroy(IRender_Glow* p_) {  }
 void CRender::model_Logging(BOOL bEnable) {}
void CRender::models_Prefetch() {}
void CRender::models_Clear(BOOL b_complete) {}
void CRender::Screenshot(ScreenshotMode mode , LPCSTR name ) {}
void CRender::Screenshot(ScreenshotMode mode, CMemoryWriter& memory_writer) {}
void CRender::ScreenshotAsyncBegin() {}
void CRender::ScreenshotAsyncEnd(CMemoryWriter& memory_writer) {}
u32 CRender::memory_usage() { return 0; }
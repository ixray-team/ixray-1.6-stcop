#include "stdafx.h"
#pragma hdrstop

#include <d3dcompiler.h>
#include "../Layers/xrRenderDX9/dx9ShaderUtils.h"
#include "render.h"


#include "../Layers/xrRender/ResourceManager.h"
#include "../../xrCore/API/xrAPI.h"
#include "../../xrEngine/irenderable.h"
#include "../../xrEngine/xr_object.h"
#include "../../xrEngine/CustomHUD.h"
//---------------------------------------------------------------------------
float ssaDISCARD = 4.f;
float ssaDONTSORT = 32.f;

ECORE_API float r_ssaDISCARD;
ECORE_API float	g_fSCREEN;

CRender   			RImplementation;

//---------------------
//---------------------------------------------------------------------------
CRender::CRender()
{
	val_bInvisible = FALSE;
	::Render = &RImplementation;
	m_skinning = 0;
}

CRender::~CRender()
{
	xr_delete(Target);
}

void					CRender::Initialize()
{
	PSLibrary.OnCreate();
}
void					CRender::ShutDown()
{
	PSLibrary.OnDestroy();
}

void					CRender::OnDeviceCreate()
{
	Models = xr_new<CModelPool>();
	Models->Logging(FALSE);
}
void					CRender::OnDeviceDestroy()
{
	xr_delete(Models);
}

ref_shader	CRender::getShader(int id) { return 0; }//VERIFY(id<int(Shaders.size()));	return Shaders[id];	}

BOOL CRender::occ_visible(Fbox& B)
{
	u32 mask = 0xff;
	return ViewBase.testAABB(B.data(), mask);
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
	g_fSCREEN = float(EDevice->TargetWidth * EDevice->TargetHeight);
	r_ssaDISCARD = (ssaDISCARD * ssaDISCARD) / g_fSCREEN;
	//	r_ssaLOD_A						=	(ssaLOD_A*ssaLOD_A)/g_fSCREEN;
	//	r_ssaLOD_B						=	(ssaLOD_B*ssaLOD_B)/g_fSCREEN;
	lstRenderables.clear();
	ViewBase.CreateFromMatrix(EDevice->mFullTransform, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);
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
#include "../../../Layers/xrRender/CHudInitializer.h"
#include "../../../Layers/xrRender/CHudInitializer.cpp"
void CRender::Render()
{

}

IRender_DetailModel* CRender::model_CreateDM(IReader* F)
{
	VERIFY(F);
	CDetail* D = xr_new<CDetail>();
	D->Load(F);
	return D;
}

IRenderVisual* CRender::model_CreatePE(LPCSTR name)
{
	PS::CPEDef* source = PSLibrary.FindPED(name);
	return Models->CreatePE(source);
}

IRenderVisual* CRender::model_CreateParticles(LPCSTR name)
{
	PS::CPEDef* SE = PSLibrary.FindPED(name);
	if (SE) return		Models->CreatePE(SE);
	else {
		PS::CPGDef* SG = PSLibrary.FindPGD(name);
		return			SG ? Models->CreatePG(SG) : 0;
	}
}

void	CRender::rmNear()
{
	CRenderTarget* T = getTarget();
	D3DVIEWPORT9 VP = { 0,0,T->get_width(),T->get_height(),0,0.02f };
	CHK_DX(REDevice->SetViewport(&VP));
}
void	CRender::rmFar()
{
	CRenderTarget* T = getTarget();
	D3DVIEWPORT9 VP = { 0,0,T->get_width(),T->get_height(),0.99999f,1.f };
	CHK_DX(REDevice->SetViewport(&VP));
}
void	CRender::rmNormal()
{
	CRenderTarget* T = getTarget();
	D3DVIEWPORT9 VP = { 0,0,T->get_width(),T->get_height(),0,1.f };
	CHK_DX(REDevice->SetViewport(&VP));
}

void 	CRender::set_Transform(Fmatrix* M)
{
	current_matrix.set(*M);
}

void CRender::add_Visual(IRenderVisual* visual, bool) {
	if(val_bInvisible) {
		return;
	}

	if(auto pKin = PKinematics(visual)) {
		pKin->CalculateBones(TRUE);
	}

	CHudInitializer initalizer(false);

	if(get_HUD()) {
		initalizer.SetHudMode();
		RCache.set_xform_view(Device.mView);
		RCache.set_xform_project(Device.mProject);
		RImplementation.rmNear();
	}

	Models->RenderSingle(dynamic_cast<dxRender_Visual*>(visual), current_matrix, 1.f);

	if(get_HUD()) {
		initalizer.SetDefaultMode();
		RCache.set_xform_view(Device.mView);
		RCache.set_xform_project(Device.mProject);
		RImplementation.rmNormal();
	}
}

IRenderVisual* CRender::model_Create(LPCSTR name, IReader* data) { return Models->Create(name, data); }
IRenderVisual* CRender::model_CreateChild(LPCSTR name, IReader* data) { return Models->CreateChild(name, data); }
void 			CRender::model_Delete(IRenderVisual*& V, BOOL bDiscard) { auto v = dynamic_cast<dxRender_Visual*>(V); Models->Delete(v, bDiscard); if (v == nullptr)V = nullptr; }
IRenderVisual* CRender::model_Duplicate(IRenderVisual* V) { return Models->Instance_Duplicate(dynamic_cast<dxRender_Visual*>(V)); }
void 			CRender::model_Render(IRenderVisual* m_pVisual, const Fmatrix& mTransform, int priority, bool strictB2F, float m_fLOD) { Models->Render(dynamic_cast<dxRender_Visual*>(m_pVisual), mTransform, priority, strictB2F, m_fLOD); }
void 			CRender::model_RenderSingle(IRenderVisual* m_pVisual, const Fmatrix& mTransform, float m_fLOD) { Models->RenderSingle(dynamic_cast<dxRender_Visual*>(m_pVisual), mTransform, m_fLOD); }

void					CRender::reset_begin()
{
	xr_delete(Target);
}
void					CRender::reset_end()
{
	Target = xr_new<CRenderTarget>();
}

bool is_Hud_mode = false;

void CRender::set_HUD(BOOL V)
{
	is_Hud_mode = !!V;
}

BOOL CRender::get_HUD()
{
	return is_Hud_mode;
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
	virtual bool get_hud_mode() { return false; }
	virtual vis_data& get_homdata() { static vis_data temp = {};  return temp; };

	virtual void set_occq_mode(bool b) {};
	virtual bool get_occq_mode() { return false; };

	virtual void set_ignore_object(CObject* O) {};
	virtual CObject* get_ignore_object() { return nullptr; };

	virtual void	set_decor_object(CObject* O, int index = 0) {};
	virtual CObject* get_decor_object(int index = 0) { return nullptr; };

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
	virtual void					set_position(const Fvector& P) { return; }
	virtual void					set_direction(const Fvector& P) { return; }
	virtual void					set_radius(float			R) { return; }
	virtual void					set_texture(LPCSTR			name) { return; }
	virtual void					set_color(const Fcolor& C) { return; }
	virtual void					set_color(float r, float g, float b) { return; }
	virtual void					spatial_move() { return; }
};

IRender_Glow* CRender::glow_create() { return xr_new< RGlow>(); }
void CRender::glow_destroy(IRender_Glow* p_) {  }
void CRender::model_Logging(BOOL bEnable) {}
void CRender::models_Prefetch() {}
void CRender::models_Clear(BOOL b_complete) {}
void CRender::Screenshot(ScreenshotMode mode, LPCSTR name) {}
void CRender::Screenshot(ScreenshotMode mode, CMemoryWriter& memory_writer) {}
void CRender::ScreenshotAsyncBegin() {}
void CRender::ScreenshotAsyncEnd(CMemoryWriter& memory_writer) {}
u32 CRender::memory_usage() { return 0; }



//--------------------------------------------------------------------------------------------------------------
class includer : public ID3DInclude {
public:
	HRESULT __stdcall Open(D3D_INCLUDE_TYPE IncludeType, LPCSTR pFileName, LPCVOID pParentData, LPCVOID* ppData, UINT* pBytes) {
		string_path pname;
		xr_strconcat(pname, ::Render->getShaderPath(), pFileName);
		IReader* R = FS.r_open("$game_shaders$", pname);
		if (0 == R) {
			// possibly in shared directory or somewhere else - open directly
			R = FS.r_open("$game_shaders$", pFileName);
			if (0 == R) {
				return E_FAIL;
			}
		}

		// duplicate and zero-terminate
		u32 size = R->length();
		u8* data = xr_alloc<u8>(size + 1);
		CopyMemory(data, R->pointer(), size);
		data[size] = 0;
		FS.r_close(R);

		*ppData = data;
		*pBytes = size;
		return	D3D_OK;
	}

	HRESULT __stdcall Close(LPCVOID pData) {
		xr_free(pData);
		return D3D_OK;
	}
};

static HRESULT create_shader(
	LPCSTR const	pTarget,
	DWORD const* buffer,
	u32	const		buffer_size,
	LPCSTR const	file_name,
	void*& result,
	bool const		disasm
)
{
	HRESULT		_result = E_FAIL;
	if (pTarget[0] == 'p') {
		SPS* sps_result = (SPS*)result;
		_result = RDevice->CreatePixelShader(buffer, &sps_result->ps);
		if (!SUCCEEDED(_result)) {
			Msg("! PS: %s", file_name);
			Msg("! CreatePixelShader hr == 0x%08x", _result);
			return		E_FAIL;
		}

		LPCVOID			data = nullptr;
		_result = D3D9FindShaderComment(buffer, MAKEFOURCC('C', 'T', 'A', 'B'), &data, nullptr);
		if (SUCCEEDED(_result) && data)
		{
			LPD3DXSHADER_CONSTANTTABLE	pConstants = LPD3DXSHADER_CONSTANTTABLE(data);
			sps_result->constants.parse(pConstants, 0x1);
		}
		else
		{
			Msg("! PS: %s", file_name);
			Msg("! D3DXFindShaderComment hr == 0x%08x", _result);
		}
	}
	else {
		SVS* svs_result = (SVS*)result;
		_result = RDevice->CreateVertexShader(buffer, &svs_result->vs);
		if (!SUCCEEDED(_result)) {
			Msg("! VS: %s", file_name);
			Msg("! CreatePixelShader hr == 0x%08x", _result);
			return		E_FAIL;
		}

		LPCVOID			data = nullptr;
		_result = D3D9FindShaderComment(buffer, MAKEFOURCC('C', 'T', 'A', 'B'), &data, nullptr);
		if (SUCCEEDED(_result) && data)
		{
			LPD3DXSHADER_CONSTANTTABLE	pConstants = LPD3DXSHADER_CONSTANTTABLE(data);
			svs_result->constants.parse(pConstants, 0x2);
		}
		else
		{
			Msg("! VS: %s", file_name);
			Msg("! D3DXFindShaderComment hr == 0x%08x", _result);
		}
	}

	if (disasm) {
		ID3DBlob* disasm_ = 0;
		D3DDisassemble(buffer, buffer_size, FALSE, 0, &disasm_);
		string_path dname;
		xr_strconcat(dname, "disasm\\", file_name, ('v' == pTarget[0]) ? ".vs.hlsl" : ".ps.hlsl");
		IWriter* W = FS.w_open("$logs$", dname);
		W->w(disasm_->GetBufferPointer(), disasm_->GetBufferSize());
		FS.w_close(W);
		_RELEASE(disasm_);
	}

	return				_result;
}


HRESULT	CRender::shader_compile(
	LPCSTR							name,
	DWORD const* pSrcData,
	UINT                            SrcDataLen,
	LPCSTR                          pFunctionName,
	LPCSTR                          pTarget,
	DWORD                           Flags,
	void*& result
)
{
	D3D_SHADER_MACRO defines[128];
	int def_it = 0;

	char	sh_name[MAX_PATH] = "";
	u32 len = 0;


	if (m_skinning < 0) {
		defines[def_it].Name = "SKIN_NONE";
		defines[def_it].Definition = "1";
		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0'; ++len;
	}

	if (0 == m_skinning) {
		defines[def_it].Name = "SKIN_0";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(0 == m_skinning); ++len;

	if (1 == m_skinning) {
		defines[def_it].Name = "SKIN_1";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(1 == m_skinning); ++len;

	if (2 == m_skinning) {
		defines[def_it].Name = "SKIN_2";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(2 == m_skinning); ++len;

	if (3 == m_skinning) {
		defines[def_it].Name = "SKIN_3";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(3 == m_skinning); ++len;

	if (4 == m_skinning) {
		defines[def_it].Name = "SKIN_4";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(4 == m_skinning); ++len;

	// finish
	defines[def_it].Name = 0;
	defines[def_it].Definition = 0;
	def_it++;
	R_ASSERT(def_it < 128);

	HRESULT		_result = E_FAIL;


	if (FAILED(_result))
	{
		includer					Includer;
		LPD3DBLOB					pShaderBuf = nullptr;
		LPD3DBLOB					pErrorBuf = nullptr;

		_result = D3DCompile(pSrcData, SrcDataLen,
			"",
			defines, &Includer, pFunctionName,
			pTarget,
			Flags, 0,
			&pShaderBuf,
			&pErrorBuf
		);

		if (SUCCEEDED(_result))
		{

			_result = create_shader(pTarget, (DWORD*)pShaderBuf->GetBufferPointer(), pShaderBuf->GetBufferSize(), name, result, false);
		}
		else
		{
			if (pErrorBuf)
				Msg("! error: %s", (LPCSTR)pErrorBuf->GetBufferPointer());
			else
				Msg("Can't compile shader hr=0x%08x", _result);
		}
	}

	return						_result;
}
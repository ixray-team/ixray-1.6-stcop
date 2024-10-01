#include "stdafx.h"
#pragma hdrstop

#include "../Layers/xrRenderDX9/dx9ShaderUtils.h"
#include "render.h"


#include "../Layers/xrRender/ResourceManager.h"
#include "../../xrCore/API/xrAPI.h"
#include "../../xrEngine/irenderable.h"
#include "../../xrEngine/xr_object.h"
#include "../../xrEngine/CustomHUD.h"

#include "..\Layers\xrRender\uber_deffer.h"
#include "..\Layers\xrRender\uber_deffer.cpp"
//#include "../../../Layers/xrRender/Light_Render_Direct_ComputeXFS.cpp"

#include "../../../Layers/xrRender/du_sphere_part.h"
#include "../../../Layers/xrRender/du_cone.h"
#include "../../../Layers/xrRender/du_sphere.h"

//---------------------------------------------------------------------------
float ssaDISCARD = 4.f;
float ssaDONTSORT = 32.f;

ECORE_API float r_ssaDISCARD;
ECORE_API float	g_fSCREEN;

CRender RImplementation;

void CRenderTarget::draw_volume(light* L) {
	switch(L->flags.type) {
		case IRender_Light::SPOT:
		RCache.set_Geometry(g_accum_spot);
		RCache.Render(D3DPT_TRIANGLELIST, 0, 0, DU_CONE_NUMVERTEX, 0, DU_CONE_NUMFACES);
		break;
		default:
		RCache.set_Geometry(g_accum_point);
		RCache.Render(D3DPT_TRIANGLELIST, 0, 0, DU_SPHERE_NUMVERTEX, 0, DU_SPHERE_NUMFACES);
		break;
	}
}

CRenderTarget::CRenderTarget() {
	t_envmap_0 = DEV->_CreateTexture("$user$env_s0");
	t_envmap_1 = DEV->_CreateTexture("$user$env_s1");

	b_accum = new CBlender_accum();
	s_accum.create(b_accum, "r2\\accum_spot_s", "lights\\lights_spot01");

	// POINT
	{
		accum_point_geom_create();
		g_accum_point.create(D3DFVF_XYZ, g_accum_point_vb, g_accum_point_ib);
	}

	// SPOT
	{
		accum_spot_geom_create();
		g_accum_spot.create(D3DFVF_XYZ, g_accum_spot_vb, g_accum_spot_ib);
	}
};

CRenderTarget::~CRenderTarget() {
	accum_spot_geom_destroy();
	accum_point_geom_destroy();

	xr_delete(b_accum);
};

void	light::xform_calc() {
	if(Device.dwFrame == m_xform_frame)	return;
	m_xform_frame = Device.dwFrame;

	// build final rotation / translation
	Fvector					L_dir, L_up, L_right;

	// dir
	L_dir.set(direction);
	float l_dir_m = L_dir.magnitude();
	if(_valid(l_dir_m) && l_dir_m > EPS_S)	L_dir.div(l_dir_m);
	else									L_dir.set(0, 0, 1);

	// R&N
	if(right.square_magnitude() > EPS) {
		// use specified 'up' and 'right', just enshure ortho-normalization
		L_right.set(right);				L_right.normalize();
		L_up.crossproduct(L_dir, L_right);		L_up.normalize();
		L_right.crossproduct(L_up, L_dir);			L_right.normalize();
	}
	else {
		// auto find 'up' and 'right' vectors
		L_up.set(0, 1, 0);				if(_abs(L_up.dotproduct(L_dir)) > .99f)	L_up.set(0, 0, 1);
		L_right.crossproduct(L_up, L_dir);			L_right.normalize();
		L_up.crossproduct(L_dir, L_right);		L_up.normalize();
	}

	// matrix
	Fmatrix					mR;
	mR.i = L_right;	mR._14 = 0;
	mR.j = L_up;		mR._24 = 0;
	mR.k = L_dir;	mR._34 = 0;
	mR.c = position;	mR._44 = 1;

	// switch
	switch(flags.type) {
		case IRender_Light::REFLECTED:
		case IRender_Light::POINT:
		{
			// scale of identity sphere
			float		L_R = range;
			Fmatrix		mScale;		mScale.scale(L_R, L_R, L_R);
			m_xform.mul_43(mR, mScale);
		}
		break;
		case IRender_Light::SPOT:
		{
			// scale to account range and angle
			float		s = 2.f * range * tanf(cone / 2.f);
			Fmatrix		mScale;		mScale.scale(s, s, range);	// make range and radius
			m_xform.mul_43(mR, mScale);
		}
		break;
		case IRender_Light::OMNIPART:
		{
			float		L_R = 2 * range;		// volume is half-radius
			Fmatrix		mScale;		mScale.scale(L_R, L_R, L_R);
			m_xform.mul_43(mR, mScale);
		}
		break;
		default:
		m_xform.identity();
		break;
	}
}

void CRenderTarget::accum_spot(light* L) 
{
	if(L == nullptr) {
		return;
	}

	if(L->flags.type == IRender_Light::SPOT) 
	{
		RImplementation.LR.compute_xf_spot(L);
	}

	// *** assume accumulator setted up ***
	// *****************************	Mask by stencil		*************************************

	ref_shader shader = s_accum;
	{
		// setup xform
		L->xform_calc();

		RCache.set_xform_world(L->m_xform);
		RCache.set_xform_view(Device.mView);
		RCache.set_xform_project(Device.mProject);

		// *** similar to "Carmack's reverse", but assumes convex, non intersecting objects,
		// *** thus can cope without stencil clear with 127 lights
		// *** in practice, 'cause we "clear" it back to 0x1 it usually allows us to > 200 lights :)
		RCache.set_ColorWriteEnable(FALSE);
		RCache.set_Element(s_accum->E[0]);		// masker

		RCache.set_CullMode(CULL_CW);
		RCache.set_Stencil(TRUE, D3DCMP_LESSEQUAL, dwLightMarkerID, 0x01, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE);
		draw_volume(L);

		RCache.set_CullMode(CULL_CCW);
		RCache.set_Stencil(TRUE, D3DCMP_LESSEQUAL, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE);
		draw_volume(L);
	}

	// *****************************	Minimize overdraw	*************************************
	// Select shader (front or back-faces), *** back, if intersect near plane
	RCache.set_ColorWriteEnable();
	RCache.set_CullMode(CULL_CW); // back

	// 2D texgens 
	Fmatrix m_Texgen; u_compute_texgen_screen(m_Texgen);

	// Shadow xform (+texture adjustment matrix)
	Fmatrix m_Lmap = Fidentity;

	if(L->flags.type == IRender_Light::SPOT)
	{

		// lmap
		auto view_dim = 1.f;
		auto view_sx = 0.f;
		auto view_sy = 0.f;

		Fmatrix m_TexelAdjust = {
			view_dim / 2.f, 0.0f, 0.0f, 0.0f,
			0.0f, -view_dim / 2.f, 0.0f, 0.0f,
			0.0f, 0.0f, 1.0f, 0.0f,
			view_dim / 2.f + view_sx,	view_dim / 2.f + view_sy, 0.0, 1.0f
		};

		Fmatrix xf_world; xf_world.invert(Device.mView);
		Fmatrix xf_view = L->X.S.view;

		// compute xforms
		Fmatrix xf_project; xf_project.mul(m_TexelAdjust, L->X.S.project);
		m_Lmap.mul(xf_view, xf_world);
		m_Lmap.mulA_44(xf_project);
	}

	// Common constants
	Fvector	 L_dir, L_clr, L_pos; float L_spec;

	L_clr.set(L->color.r, L->color.g, L->color.b);
	L_spec = L_clr.dotproduct(Fvector().set(0.333f, 0.333f, 0.333f));

	Device.mView.transform_tiny(L_pos, L->position);
	Device.mView.transform_dir(L_dir, L->direction);
	L_dir.normalize();

	// Draw volume with projective texgen
	{
		RCache.set_Element(shader->E[L->flags.type == IRender_Light::SPOT ? 1 : 2]);

		// Constants
		float	att_R = L->range * .95f;
		float	att_factor = 1.f / (att_R * att_R);

		RCache.set_c("Ldynamic_pos", L_pos.x, L_pos.y, L_pos.z, att_factor);
		RCache.set_c("Ldynamic_color", L_clr.x, L_clr.y, L_clr.z, L_spec);

		RCache.set_c("m_texgen", m_Texgen);
		RCache.set_c("m_shadow", m_Lmap);

		RCache.set_Stencil(TRUE, D3DCMP_LESSEQUAL, dwLightMarkerID, 0xff, 0x00);
		draw_volume(L);
	}

	increment_light_marker();
}

#include "../../../Layers/xrRenderPC_R2/r2_rendertarget_accum_spot_geom.cpp"
#include "../../../Layers/xrRenderPC_R2/r2_rendertarget_accum_point_geom.cpp"
#include "../../../Layers/xrRender/light.cpp"

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
	Models = new CModelPool();
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
	if (dwFrameCalc == Device.dwFrame)
		return;

	// Transfer to global space to avoid deep pointer access
	g_fSCREEN = float(EDevice->TargetWidth * EDevice->TargetHeight);
	r_ssaDISCARD = (ssaDISCARD * ssaDISCARD) / g_fSCREEN;
	//	r_ssaLOD_A						=	(ssaLOD_A*ssaLOD_A)/g_fSCREEN;
	//	r_ssaLOD_B						=	(ssaLOD_B*ssaLOD_B)/g_fSCREEN;
	lstRenderables.clear();
	ViewBase.CreateFromMatrix(EDevice->mFullTransform, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);
	Target->reset_light_marker();

	{
		g_SpatialSpace->q_frustum
		(
			lstRenderables,
			ISpatial_DB::O_ORDERED,
			STYPE_RENDERABLE + STYPE_LIGHTSOURCE + STYPE_PARTICLE,
			ViewBase
		);

		// Exact sorting order (front-to-back)

		m_pointlights.resize(0);
		m_spotlights.resize(0);

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
			if(pSpatial->spatial.type & STYPE_LIGHTSOURCE) {
				if(light* L = (light*)pSpatial->dcast_Light()) {
					if(Device.dwFrame == L->frame_render) continue;
					L->frame_render = Device.dwFrame;
					L->flags.bShadow = FALSE;
					L->flags.bOccq = FALSE;
					if(L->flags.type == IRender_Light::SPOT) {
						m_spotlights.push_back(L);
					}
					else {
						m_pointlights.push_back(L);
					}
				}
			}

			IRenderable* renderable = pSpatial->dcast_Renderable();
			if (!renderable)
				continue;

			if(!!(pSpatial->spatial.type & STYPE_RENDERABLE) || !!(pSpatial->spatial.type & STYPE_PARTICLE)) {
				set_Object(renderable);
				renderable->renderable_Render();
				set_Object(nullptr);
			}
		}
	}

	dwFrameCalc = Device.dwFrame;
}

#include "../xrEngine/IGame_Persistent.h"
#include "../../../Layers/xrRender/CHudInitializer.h"
#include "../../../Layers/xrRender/CHudInitializer.cpp"
#include "../../../Layers/xrRender/dxEnvironmentRender.h"
#include "../../../xrEngine/IGame_Level.h"

void CRender::Render()
{
	if(Target) 
	{
		if(g_pGamePersistent && g_pGameLevel) 
		{
			auto& envdescren = *(dxEnvDescriptorMixerRender*)(&*g_pGamePersistent->Environment().CurrentEnv->m_pDescriptorMixer);
			envdescren.sky_r_textures_env[0].second->Load();
			envdescren.sky_r_textures_env[1].second->Load();

			Target->t_envmap_0->surface_set(envdescren.sky_r_textures_env[0].second->pSurface);
			Target->t_envmap_1->surface_set(envdescren.sky_r_textures_env[1].second->pSurface);
		}

		for(auto& light : m_spotlights) 
		{
			Target->accum_spot(light);
		}

		for(auto& light : m_pointlights)
		{
			Target->accum_spot(light);
		}

		Target->reset_light_marker(true);

		RCache.set_CullMode(CULL_CCW); // back
		RCache.set_Stencil(FALSE);
	}

	m_pointlights.resize(0);
	m_spotlights.resize(0);
}

IRender_DetailModel* CRender::model_CreateDM(IReader* F)
{
	VERIFY(F);
	CDetail* D = new CDetail();
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

	if(!visual) {
		return;
	}

	if(auto pKin = PKinematics(visual)) {
		pKin->CalculateBones(TRUE);
	}

	Models->RenderSingle(dynamic_cast<dxRender_Visual*>(visual), current_matrix, 1.f);
}

IRenderVisual* CRender::model_Create(LPCSTR name, IReader* data) { return Models->Create(name, data); }
IRenderVisual* CRender::model_CreateChild(LPCSTR name, IReader* data) { return Models->CreateChild(name, data); }
void 			CRender::model_Delete(IRenderVisual*& V, BOOL bDiscard) { auto v = dynamic_cast<dxRender_Visual*>(V); Models->Delete(v, bDiscard); if (v == nullptr)V = nullptr; }
IRenderVisual* CRender::model_Duplicate(IRenderVisual* V) { return Models->Instance_Duplicate(dynamic_cast<dxRender_Visual*>(V)); }
void 			CRender::model_Render(IRenderVisual* m_pVisual, const Fmatrix& mTransform, int priority, bool strictB2F, float m_fLOD) { Models->Render(dynamic_cast<dxRender_Visual*>(m_pVisual), mTransform, priority, strictB2F, m_fLOD); }
void 			CRender::model_RenderSingle(IRenderVisual* m_pVisual, const Fmatrix& mTransform, float m_fLOD) { Models->RenderSingle(dynamic_cast<dxRender_Visual*>(m_pVisual), mTransform, m_fLOD); }

void CRender::reset_begin() {
	xr_delete(Target);
}

void CRender::reset_end() {
	Target = new CRenderTarget();
}

void CRender::set_HUD(BOOL V)
{
	static CHudInitializer initalizer(false);

	if(!!V) {
		initalizer = CHudInitializer(false);
		initalizer.SetHudMode();

		RCache.set_xform_view(Device.mView);
		RCache.set_xform_project(Device.mProject);

		RImplementation.rmNear();
	}
	else {
		initalizer.SetDefaultMode();

		RCache.set_xform_view(Device.mView);
		RCache.set_xform_project(Device.mProject);

		RImplementation.rmNormal();
	}
}

BOOL CRender::get_HUD() {
	return false;
}

void CRender::set_Invisible(BOOL V)
{
	val_bInvisible = V;
}

DWORD CRender::get_dx_level()
{
	return 90;
}

static class cl_lighting_enable : public R_constant_setup {
	virtual void setup(R_constant* C) {
		float is_lighting_enable = 0.0f;
		if(g_pGamePersistent && g_pGameLevel) {
			if(g_pGamePersistent->Environment().Current[0] && g_pGamePersistent->Environment().Current[1]) {
				is_lighting_enable = 1.0f;
			}
		}
		RCache.set_c(C, is_lighting_enable, is_lighting_enable, is_lighting_enable, is_lighting_enable);
	}
} binder_lighting_enable;

void CRender::create()
{
	DEV->RegisterConstantSetup("is_lighting_enable", &binder_lighting_enable);
}

void CRender::destroy()
{
	xr_delete(Target);
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
IRender_ObjectSpecific* CRender::ros_create(IRenderable* parent) { return new RenderObjectSpecific(); }
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

IRender_Light* CRender::light_create() {
	light* L = new light();
	L->flags.bStatic = false;
	L->flags.bActive = false;
	L->flags.bShadow = true;
	return L;
//	return xr_new< RLight>();
}

void CRender::light_destroy(IRender_Light* p_) {
}

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

IRender_Glow* CRender::glow_create() { return new RGlow(); }
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

	for(u32 i = 0; i < m_ShaderOptions.size(); ++i) {
		defines[def_it++] = m_ShaderOptions[i];
	}

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

void CBlender_accum::Compile(CBlender_Compile& C) {
	IBlender::Compile(C);

	if(C.iElement == 0) {
		C.r_Pass("accum_mask", "dumb", false, TRUE, FALSE);
		C.r_End();

		return;
	}

	if(C.iElement > 2) {
		return;
	}

	if(C.iElement == 1) {
		RImplementation.addShaderOption("USE_LMAP", "1");
	}

	C.r_Pass("accum_volume", "accum_base", false, FALSE, FALSE, TRUE, D3DBLEND_ONE, D3DBLEND_ONE);

	C.r_Sampler_rtf("s_base", "$user$diffuse");
	C.r_Sampler_rtf("s_position", "$user$position");
	C.r_Sampler_rtf("s_normal", "$user$normal");

	C.r_Sampler_clw("s_material", "shaders\\r2_material");

	if(C.iElement == 1) {
		C.r_Sampler_clf("s_lmap", *C.L_textures[0]);
	}

	C.r_End();
}

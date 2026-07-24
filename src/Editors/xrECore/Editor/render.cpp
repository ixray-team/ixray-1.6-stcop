#include "stdafx.h"


#include "render.h"


#include "Layers/xrRender/ResourceManager.h"
#include "xrCore/API/xrAPI.h"
#include "xrEngine/irenderable.h"
#include "xrEngine/xr_object.h"
#include "xrEngine/CustomHUD.h"

#include "Layers\xrRender\uber_deffer.cpp"
#include "Layers/xrRender/du_cone.h"
#include "Layers/xrRender/du_sphere.h"
#include "Layers/xrRender/FTreeVisual_Prototype.h"

//---------------------------------------------------------------------------
float ssaDISCARD = 4.f;
float ssaDONTSORT = 32.f;

ECORE_API float r_ssaDISCARD;
ECORE_API float	g_fSCREEN;

CRender RImplementation;

void CRenderTarget::draw_volume(light* L)
{
	switch(L->flags.type)
	{
		case IRender_Light::SPOT:
		RCache.set_Geometry(g_accum_spot);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, DU_CONE_NUMVERTEX, 0, DU_CONE_NUMFACES);
		break;
		default:
		RCache.set_Geometry(g_accum_point);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, DU_SPHERE_NUMVERTEX, 0, DU_SPHERE_NUMFACES);
		break;
	}
}

CRenderTarget::CRenderTarget()
{
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
		L_up.set(0, 1, 0);				if(std::abs(L_up.dotproduct(L_dir)) > .99f)	L_up.set(0, 0, 1);
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

		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::FRONT);
		RCache.set_Stencil(TRUE, D3DCMP_LESSEQUAL, dwLightMarkerID, 0x01, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE);
		draw_volume(L);

		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
		RCache.set_Stencil(TRUE, D3DCMP_LESSEQUAL, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE);
		draw_volume(L);
	}

	// *****************************	Minimize overdraw	*************************************
	// Select shader (front or back-faces), *** back, if intersect near plane
	RCache.set_ColorWriteEnable();
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::FRONT);

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

#include "../../../Layers/xrRender/light.cpp"

//---------------------
//---------------------------------------------------------------------------
CRender::CRender()
{
	val_bUI = FALSE;
	val_bInvisible = FALSE;
	::Render = &RImplementation;
	Engine.External.SetSkinningMode();
}

CRender::~CRender()
{
	xr_delete(Target);
}

void CRender::Initialize()
{
	PSLibrary.OnCreate();
}
void CRender::ShutDown()
{
	PSLibrary.OnDestroy();
}

void CRender::OnDeviceCreate()
{
	Models = new CModelPool();
}

void CRender::OnDeviceDestroy()
{
	xr_delete(Models);
}

ref_shader	CRender::getShader(int id) { return nullptr; }
ref_shader CRender::getShaderShared(shared_str id) {return nullptr;}

bool CRender::occ_visible(Fbox& B)
{
	u32 mask = 0xff;
	return ViewBase.testAABB(B.data(), mask);
}

bool CRender::occ_visible(sPoly& P)
{
	return ViewBase.testPolyInside(P);
}

bool CRender::occ_visible(vis_data& P)
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
		//Lights Delete queue
		for (light*L:v_all_lights_dque)
			xr_delete(L);
		v_all_lights_dque.clear();
	}
	{
		g_SpatialSpace->q_frustum
		(
			lstRenderables,
			ISpatial_DB::O_ORDERED,
			ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::LIGHTSOURCE | ESPATIAL_TYPE::PARTICLE,
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
		for (ISpatialShared& pSpatial : lstRenderables)
		{
			if ((pSpatial->type & ESPATIAL_TYPE::LIGHTSOURCE) != ESPATIAL_TYPE::NONE)
			{
				if(light* L = (light*)pSpatial->dcast_Light())
				{
					if(Device.dwFrame == L->frame_render) continue;
					L->frame_render = Device.dwFrame;
					L->flags.bShadow = FALSE;
					L->flags.bOccq = FALSE;
					if(L->flags.type == IRender_Light::SPOT)
					{
						m_spotlights.push_back(L);
					}
					else 
					{
						m_pointlights.push_back(L);
					}
				}
			}

			IRenderable* renderable = pSpatial->dcast_Renderable();
			if (!renderable)
				continue;

			if((pSpatial->type & ESPATIAL_TYPE::RENDERABLE) != ESPATIAL_TYPE::NONE || (pSpatial->type & ESPATIAL_TYPE::PARTICLE) != ESPATIAL_TYPE::NONE)
			{
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
#include "../../../xrCore/git_version.h"

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

		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK); // back
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
	RHIViewport VP = { 0,0,(float)T->get_width(),(float)T->get_height(),0,0.02f };
	GRHI->SetViewport(VP);
}
void	CRender::rmFar()
{
	CRenderTarget* T = getTarget();
	RHIViewport VP = { 0,0,(float)T->get_width(),(float)T->get_height(),0.99999f,1.f };
	GRHI->SetViewport(VP);
}
void	CRender::rmNormal()
{
	CRenderTarget* T = getTarget();
	RHIViewport VP = { 0,0,(float)T->get_width(),(float)T->get_height(),0,1.f };
	GRHI->SetViewport(VP);
}

void CRender::RenderUI(bool) 
{
}

void 	CRender::set_Transform(Fmatrix& M)
{
	current_matrix.set(M);
}

void CRender::add_Visual(IRenderVisual* visual, bool IgnoreOptimize, bool Force)
{
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

IRenderVisual* CRender::model_GetPrototype(str_c name)
{
	return Models->GetPrototype(name);
}

CDB::MODEL* CRender::model_GetPrototypeCollision(str_c name)
{
	return ((FTreeVisual_Prototype*)model_GetPrototype(name))->GetCollisionModel();
}

IRenderVisual* CRender::model_CreateChild(LPCSTR name, IReader* data) { return Models->CreateChild(name, data); }
void 			CRender::model_Delete(IRenderVisual*& V, bool bDiscard) { auto v = dynamic_cast<dxRender_Visual*>(V); Models->Delete(v, bDiscard); if (v == nullptr)V = nullptr; }
IRenderVisual* CRender::model_Duplicate(IRenderVisual* V) { return Models->Instance_Duplicate(dynamic_cast<dxRender_Visual*>(V)); }
void 			CRender::model_Render(IRenderVisual* m_pVisual, const Fmatrix& mTransform, int priority, bool strictB2F, float m_fLOD) { Models->Render(dynamic_cast<dxRender_Visual*>(m_pVisual), mTransform, priority, strictB2F, m_fLOD); }
void 			CRender::model_RenderSingle(IRenderVisual* m_pVisual, const Fmatrix& mTransform, float m_fLOD) { Models->RenderSingle(dynamic_cast<dxRender_Visual*>(m_pVisual), mTransform, m_fLOD); }

void CRender::reset_begin() {
	xr_delete(Target);
}

void CRender::reset_end() {
	Target = new CRenderTarget();
}

void CRender::set_HUD(bool V)
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

bool CRender::get_HUD()
{
	return false;
}

void CRender::set_UI(bool V)
{
	val_bUI = V;
}

void CRender::set_Invisible(bool V)
{
	val_bInvisible = V;
}

DWORD CRender::get_dx_level()
{
	return 90;
}

static class cl_lighting_enable : 
	public RHIShaderConstant::Setup
{
	virtual void setup(RHIShaderConstant* C)
	{
		float is_lighting_enable = 0.0f;
		if(g_pGamePersistent && psDeviceFlags.test(rsEnvironment))
		{
			is_lighting_enable = (int)g_pGamePersistent->Environment().Current[0] && g_pGamePersistent->Environment().Current[1];
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
void CRender::models_Prefetch() {}
void CRender::models_Clear(bool b_complete) {}
void CRender::Screenshot(ScreenshotMode mode, LPCSTR name) {}
void CRender::Screenshot(ScreenshotMode mode, CMemoryWriter& memory_writer) {}
void CRender::ScreenshotAsyncBegin() {}
void CRender::ScreenshotAsyncEnd(CMemoryWriter& memory_writer) {}
u32 CRender::memory_usage() { return 0; }



//--------------------------------------------------------------------------------------------------------------
#include "../../Layers/xrRender/ShaderResourceTraits.h"
template <typename T>
static HRESULT create_shader(
	LPCSTR const pTarget,
	DWORD const* buffer,
	u32	const buffer_size,
	LPCSTR const file_name,
	T*& result,
	bool const disasm
) {
	result->sh = ShaderTypeTraits<T>::CreateHWShader(buffer, buffer_size);

	ID3DShaderReflection* pReflection = 0;

	HRESULT const _hr = D3DReflect(buffer, buffer_size, IID_ID3DShaderReflection, (void**)&pReflection);
	if (SUCCEEDED(_hr) && pReflection) {
		// Parse constant table data
		result->constants.parse(pReflection, ShaderTypeTraits<T>::GetShaderDest());

		_RELEASE(pReflection);
	}
	else {
		Msg("! D3DReflectShader %s hr == 0x%08x", file_name, _hr);
	}

	return _hr;
}

static HRESULT create_shader(
	LPCSTR const pTarget,
	DWORD const* buffer,
	u32	const buffer_size,
	LPCSTR const file_name,
	void*& result,
	bool const disasm
) {
	HRESULT		_result = E_FAIL;
	if (pTarget[0] == 'p') {
		SPS* sps_result = (SPS*)result;
		_result = RDevice->CreatePixelShader(buffer, buffer_size, 0, &sps_result->ps);
		if (!SUCCEEDED(_result)) {
			Msg("! PS: %s", file_name);
			Msg("! CreatePixelShader hr == 0x%08x", _result);
			return		E_FAIL;
		}

		ID3DShaderReflection* pReflection = 0;

		_result = D3DReflect(buffer, buffer_size, IID_ID3DShaderReflection, (void**)&pReflection);

		//	Parse constant, texture, sampler binding
		//	Store input signature blob
		if (SUCCEEDED(_result) && pReflection) {
			//	Let constant table parse it's data
			sps_result->constants.parse(pReflection, RC_dest_pixel);

			_RELEASE(pReflection);
		}
		else {
			Msg("! PS: %s", file_name);
			Msg("! D3DReflectShader hr == 0x%08x", _result);
		}
	}
	else if (pTarget[0] == 'v') {
		SVS* svs_result = (SVS*)result;
		_result = RDevice->CreateVertexShader(buffer, buffer_size, 0, &svs_result->vs);

		if (!SUCCEEDED(_result)) {
			Msg("! VS: %s", file_name);
			Msg("! CreatePixelShader hr == 0x%08x", _result);
			return		E_FAIL;
		}

		ID3DShaderReflection* pReflection = 0;
		_result = D3DReflect(buffer, buffer_size, IID_ID3DShaderReflection, (void**)&pReflection);

		//	Parse constant, texture, sampler binding
		//	Store input signature blob
		if (SUCCEEDED(_result) && pReflection) {
			//	TODO: DX10: share the same input signatures

			//	Store input signature (need only for VS)
			//CHK_DX( D3DxxGetInputSignatureBlob(pShaderBuf->GetBufferPointer(), pShaderBuf->GetBufferSize(), &_vs->signature) );
			ID3DBlob* pSignatureBlob;
			CHK_DX(D3DGetInputSignatureBlob(buffer, buffer_size, &pSignatureBlob));
			VERIFY(pSignatureBlob);

			svs_result->signature = DEV->_CreateInputSignature(pSignatureBlob);

			_RELEASE(pSignatureBlob);

			//	Keep full VS bytecode so tools can reflect input parameters
			ID3DBlob* pCodeBlob = nullptr;
			if (SUCCEEDED(D3DCreateBlob(buffer_size, &pCodeBlob)))
			{
				CopyMemory(pCodeBlob->GetBufferPointer(), buffer, buffer_size);
				svs_result->vs_code = pCodeBlob;
			}

			//	Let constant table parse it's data
			svs_result->constants.parse(pReflection, RC_dest_vertex);

			_RELEASE(pReflection);
		}
		else {
			Msg("! VS: %s", file_name);
			Msg("! D3DXFindShaderComment hr == 0x%08x", _result);
		}
	}
	else if (pTarget[0] == 'g') {
		SGS* sgs_result = (SGS*)result;
		_result = RDevice->CreateGeometryShader(buffer, buffer_size, 0, &sgs_result->gs);
		if (!SUCCEEDED(_result)) {
			Msg("! GS: %s", file_name);
			Msg("! CreateGeometryShaderhr == 0x%08x", _result);
			return		E_FAIL;
		}

		ID3DShaderReflection* pReflection = 0;

		_result = D3DReflect(buffer, buffer_size, IID_ID3DShaderReflection, (void**)&pReflection);

		//	Parse constant, texture, sampler binding
		//	Store input signature blob
		if (SUCCEEDED(_result) && pReflection) {
			//	Let constant table parse it's data
			sgs_result->constants.parse(pReflection, RC_dest_geometry);

			_RELEASE(pReflection);
		}
		else {
			Msg("! PS: %s", file_name);
			Msg("! D3DReflectShader hr == 0x%08x", _result);
		}
	}
	else if (pTarget[0] == 'c') {
		_result = create_shader(pTarget, buffer, buffer_size, file_name, (SCS*&)result, disasm);
	}
	else if (pTarget[0] == 'h') {
		_result = create_shader(pTarget, buffer, buffer_size, file_name, (SHS*&)result, disasm);
	}
	else if (pTarget[0] == 'd') {
		_result = create_shader(pTarget, buffer, buffer_size, file_name, (SDS*&)result, disasm);
	}
	else {
		NODEFAULT;
	}

	return _result;
}

class includer : public ID3DInclude {
public:
	HRESULT  __stdcall Open(D3D_INCLUDE_TYPE IncludeType, LPCSTR pFileName, LPCVOID pParentData, LPCVOID* ppData, UINT* pBytes) {
		string_path pname;
		xr_strconcat(pname, ::Render->getShaderPath(), pFileName);
		IReader* R = FS.r_open(_game_shaders_, pname);
		if (0 == R) {
			// possibly in shared directory or somewhere else - open directly
			R = FS.r_open(_game_shaders_, pFileName);
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

HRESULT	CRender::shader_compile(
	LPCSTR name,
	DWORD const* pSrcData,
	UINT SrcDataLen,
	LPCSTR pFunctionName,
	LPCSTR pTarget,
	DWORD Flags,
	void*& result)
{
	D3D_SHADER_MACRO defines[128];
	int def_it = 0;

	char c_smapsize[32];
	char c_sun_shafts[32];
	char c_sun_quality[32];

	char sh_name[MAX_PATH] = "";

	// options
	u32 len = xr_strlen(sh_name);

	for (u32 i = 0; i < m_ShaderOptions.size(); ++i) {
		defines[def_it++] = m_ShaderOptions[i];
	}

	// options
	const int m_skinning = Engine.External.GetSkinningMode();

	if (ps_r2_ls_flags_ext.test(RFLAG_CLOUD_SHADOWS)) {
		defines[def_it].Name = "USE_SUNMASK";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(ps_r2_ls_flags_ext.test(RFLAG_CLOUD_SHADOWS)); ++len;

	// skinning
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

	//	Igor: need restart options
	if (ps_r2_ls_flags.test(R2FLAG_SOFT_WATER)) {
		defines[def_it].Name = "USE_SOFT_WATER";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if (ps_r2_ls_flags.test(R2FLAG_SOFT_PARTICLES)) {
		defines[def_it].Name = "USE_SOFT_PARTICLES";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if (ps_r2_ls_flags.test(R2FLAG_DOF)) {
		defines[def_it].Name = "USE_DOF";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if (ps_r2_ls_flags_ext.test(R4FLAG_SCREEN_SPACE_HUD_SHADOWS)) {
		defines[def_it].Name = "USE_HUD_SHADOWS";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if (ps_r2_ls_flags_ext.test(R4FLAG_HASHED_ALPHA_TEST)) {
		defines[def_it].Name = "USE_HASHED_AREF";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if (ps_r2_ls_flags_ext.test(R4FLAG_SSLR_ON_WATER)) {
		defines[def_it].Name = "USE_SSLR_ON_WATER";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if (ps_r_sun_shafts > 0) {
		xr_sprintf(c_sun_shafts, "%d", ps_r_sun_shafts);
		defines[def_it].Name = "SUN_SHAFTS_QUALITY";
		defines[def_it].Definition = c_sun_shafts;

		def_it++;
		sh_name[len] = '0' + static_cast<char>(ps_r_sun_shafts); ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if (ps_r_sun_quality > 0) {
		xr_sprintf(c_sun_quality, "%d", ps_r_sun_quality);
		defines[def_it].Name = "SUN_QUALITY";
		defines[def_it].Definition = c_sun_quality;

		def_it++;
		sh_name[len] = '0' + static_cast<char>(ps_r_sun_quality); ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if (ps_r2_ls_flags.test(R2FLAG_STEEP_PARALLAX)) {
		defines[def_it].Name = "ALLOW_STEEPPARALLAX";
		defines[def_it].Definition = "1";
		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if (RFeatureLevel == D3D_FEATURE_LEVEL_10_1) {
		defines[def_it].Name = "SM_4_1";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(RFeatureLevel == D3D_FEATURE_LEVEL_10_1); ++len;

	if (RFeatureLevel >= D3D_FEATURE_LEVEL_11_0) {
		defines[def_it].Name = "SM_5";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(RFeatureLevel >= D3D_FEATURE_LEVEL_11_0); ++len;

	// finish
	defines[def_it].Name = 0;
	defines[def_it].Definition = 0;

	def_it++;
	sh_name[len] = 0;

	if (0 == xr_strcmp(pFunctionName, "main")) {
		if ('v' == pTarget[0]) {
			if (RFeatureLevel == D3D_FEATURE_LEVEL_10_0) {
				pTarget = "vs_4_0";
			}
			else if (RFeatureLevel == D3D_FEATURE_LEVEL_10_1) {
				pTarget = "vs_4_1";
			}
			else if (RFeatureLevel == D3D_FEATURE_LEVEL_11_0) {
				pTarget = "vs_5_0";
			}
			else if (RFeatureLevel == D3D_FEATURE_LEVEL_11_1) {
				pTarget = "vs_5_0";
			}
		}
		else if ('p' == pTarget[0]) {
			if (RFeatureLevel == D3D_FEATURE_LEVEL_10_0) {
				pTarget = "ps_4_0";
			}
			else if (RFeatureLevel == D3D_FEATURE_LEVEL_10_1) {
				pTarget = "ps_4_1";
			}
			else if (RFeatureLevel == D3D_FEATURE_LEVEL_11_0) {
				pTarget = "ps_5_0";
			}
			else if (RFeatureLevel == D3D_FEATURE_LEVEL_11_1) {
				pTarget = "ps_5_0";
			}
		}
		else if ('g' == pTarget[0]) {
			if (RFeatureLevel == D3D_FEATURE_LEVEL_10_0) {
				pTarget = "gs_4_0";
			}
			else if (RFeatureLevel == D3D_FEATURE_LEVEL_10_1) {
				pTarget = "gs_4_1";
			}
			else if (RFeatureLevel == D3D_FEATURE_LEVEL_11_0) {
				pTarget = "gs_5_0";
			}
			else if (RFeatureLevel == D3D_FEATURE_LEVEL_11_1) {
				pTarget = "gs_5_0";
			}
		}
		else if ('c' == pTarget[0]) {
			if (RFeatureLevel == D3D_FEATURE_LEVEL_11_0) {
				pTarget = "cs_5_0";
			}
			else if (RFeatureLevel == D3D_FEATURE_LEVEL_11_1) {
				pTarget = "cs_5_0";
			}
		}
	}

	HRESULT _result = E_FAIL;

	char extension[3];
	strncpy_s(extension, pTarget, 2);

	string_path file_name;
	{
		string_path file;
		xr_strcpy(file, "shaders_cache\\");
		xr_strcat(file, _VER);
		xr_strcat(file, "\\d3d11\\");
		xr_strcat(file, name);
		xr_strcat(file, ".");
		xr_strcat(file, extension);
		xr_strcat(file, "\\");
		xr_strcat(file, sh_name);
		FS.update_path(file_name, "$app_data_root$", file);
	}

	u32 const RealCodeCRC = crc32(pSrcData, SrcDataLen);

	if (FS.exist(file_name) && false) 
	{
#ifdef DEBUG
		Msg("compilied shader library found %s", file_name);
#endif // DEBUG
		IReader* file = FS.r_open(file_name);

		if (file->length() > 4) {
			u32 ShaderCRC = file->r_u32();
			u32 CodeSRC = file->r_u32();

			if (RealCodeCRC == CodeSRC) {
				u32 const real_crc = crc32(file->pointer(), file->elapsed());
				if (real_crc == ShaderCRC) {
					_result = create_shader(pTarget, (DWORD*)file->pointer(), file->elapsed(), file_name, result, false);
				}
			}
		}
		file->close();
	}

	if (FAILED(_result)) {
		LPD3DBLOB pShaderBuf = nullptr;
		LPD3DBLOB pErrorBuf = nullptr;
		includer Includer;

		_result = D3DCompile(
			pSrcData,
			SrcDataLen,
			"",//nullptr, //LPCSTR pFileName,	//	NVPerfHUD bug workaround.
			defines, &Includer, pFunctionName,
			pTarget,
			Flags, 0,
			&pShaderBuf,
			&pErrorBuf
		);

		if (SUCCEEDED(_result)) {
			if (/*ps_r__common_flags.test(RFLAG_USE_CACHE)*/1) {
				IWriter* file = FS.w_open(file_name);
				u32 const crc = crc32(pShaderBuf->GetBufferPointer(), pShaderBuf->GetBufferSize());
				file->w_u32(crc);
				file->w_u32(RealCodeCRC);
				file->w(pShaderBuf->GetBufferPointer(), (u32)pShaderBuf->GetBufferSize());
				FS.w_close(file);
			}
			_result = create_shader(pTarget, (DWORD*)pShaderBuf->GetBufferPointer(), (u32)pShaderBuf->GetBufferSize(), file_name, result, false);
		}
		else {
			Msg("! %s", file_name);

			if (pErrorBuf) {
				Msg("! error: %s", (LPCSTR)pErrorBuf->GetBufferPointer());
			}
			else {
				Msg("Can't compile shader hr=0x%08x", _result);
			}
		}
	}

	return _result;
}


void CBlender_accum::Compile(CBlender_Compile& C) 
{
	IBlender::Compile(C);

	if (C.iElement == 0) {
		C.r_Pass("accum_mask", "dumb", false, TRUE, FALSE);
		C.r_End();

		return;
	}

	if (C.iElement > 2) {
		return;
	}

	if (C.iElement == 1) {
		RImplementation.addShaderOption("USE_LMAP", "1");
	}

	C.r_Pass("accum_volume", "accum_base", false, FALSE, FALSE, TRUE, D3DBLEND_ONE, D3DBLEND_ONE);

	C.r_dx10Texture("s_base", "$user$diffuse");
	C.r_dx10Texture("s_position", "$user$position");
	C.r_dx10Texture("s_normal", "$user$normal");

	C.r_dx10Texture("s_material", "shaders\\r2_material");

	if (C.iElement == 1) {
		C.r_dx10Texture("s_lmap", *C.L_textures[0]);
	}

	C.r_dx10Sampler("smp_rtlinear");
	C.r_dx10Sampler("smp_material");
	C.r_dx10Sampler("smp_nofilter");
	C.r_End();
}
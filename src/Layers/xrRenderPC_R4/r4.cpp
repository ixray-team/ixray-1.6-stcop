#include "stdafx.h"
#include "r4.h"
#include "../xrRender/fbasicvisual.h"
#include "../../xrEngine/xr_object.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/igame_persistent.h"
#include "../../xrEngine/environment.h"
#include "../xrRender/SkeletonCustom.h"
#include "../xrRender/LightTrack.h"
#include "../xrRender/dxRenderDeviceRender.h"
#include "../xrRender/dxWallMarkArray.h"
#include "../xrRender/dxUIShader.h"
#include "../../xrCore/git_version.h"

#include "../xrRenderDX10/3DFluid/dx103DFluidManager.h"
#include "../xrRender/ShaderResourceTraits.h"

CRender										RImplementation;

//////////////////////////////////////////////////////////////////////////
class CGlow				: public IRender_Glow
{
public:
	bool				bActive;
public:
	CGlow() : bActive(false)		{ }
	virtual void					set_active			(bool b)					{ bActive=b;		}
	virtual bool					get_active			()							{ return bActive;	}
	virtual void					set_position		(const Fvector& P)			{ }
	virtual void					set_direction		(const Fvector& D)			{ }
	virtual void					set_radius			(float R)					{ }
	virtual void					set_texture			(LPCSTR name)				{ }
	virtual void					set_color			(const Fcolor& C)			{ }
	virtual void					set_color			(float r, float g, float b)	{ }
};

float		r_dtex_range		= 50.f;
//////////////////////////////////////////////////////////////////////////
ShaderElement*			CRender::rimp_select_sh_dynamic	(dxRender_Visual	*pVisual, float cdist_sq)
{
	int		id	= SE_R2_SHADOW;
	if	(CRender::PHASE_NORMAL == RImplementation.phase)
	{
		id = ((_sqrt(cdist_sq)-pVisual->vis.sphere.R)<r_dtex_range)?SE_R2_NORMAL_HQ:SE_R2_NORMAL_LQ;
	}
	return pVisual->shader->E[id]._get();
}
//////////////////////////////////////////////////////////////////////////
ShaderElement*			CRender::rimp_select_sh_static	(dxRender_Visual	*pVisual, float cdist_sq)
{
	int		id	= SE_R2_SHADOW;
	if	(CRender::PHASE_NORMAL == RImplementation.phase)
	{
		id = ((_sqrt(cdist_sq)-pVisual->vis.sphere.R)<r_dtex_range)?SE_R2_NORMAL_HQ:SE_R2_NORMAL_LQ;
	}
	return pVisual->shader->E[id]._get();
}
static class cl_parallax		: public R_constant_setup		{	virtual void setup	(R_constant* C)
{
	float			h			=	ps_r2_df_parallax_h;
	RCache.set_c	(C,h,-h/2.f,1.f/r_dtex_range,1.f/r_dtex_range);
}}	binder_parallax;

static class cl_LOD		: public R_constant_setup
{
	virtual void setup	(R_constant* C)
	{
		RCache.LOD.set_LOD(C);
	}
} binder_LOD;

static class cl_pos_decompress_params : public R_constant_setup {
	virtual void setup(R_constant* C) {

		float VertTan = 1.0f / Device.mProject._22;
		float HorzTan = VertTan / Device.fASPECT;

		RCache.set_c(C, HorzTan, -VertTan, (2.0f * HorzTan) / RCache.get_width(), -(2.0f * VertTan) / RCache.get_height());
	}
}	binder_pos_decompress_params;

extern ENGINE_API float psHUD_FOV;
static class cl_pos_decompress_params_hud : public R_constant_setup {
	virtual void setup(R_constant* C) {
		float VertTan = -1.0f * tanf(deg2rad(psHUD_FOV / 2.0f));
		float HorzTan = -VertTan / Device.fASPECT;

		RCache.set_c(C, HorzTan, VertTan, (2.0f * HorzTan) / RCache.get_width(), (2.0f * VertTan) / RCache.get_height());

	}
}	binder_pos_decompress_params_hud;

static class cl_pos_decompress_params2		: public R_constant_setup		{	virtual void setup	(R_constant* C)
{
	RCache.set_c	(C, RCache.get_width(), RCache.get_height(), 1.0f / RCache.get_width(), 1.0f / RCache.get_height());

}}	binder_pos_decompress_params2;

static class cl_depth_unpack : public R_constant_setup {
	virtual void setup(R_constant* C) {
		RCache.set_c(C, Device.mProject_saved._43, Device.mProject_saved._33,
			Device.mProject_hud._43, Device.mProject_hud._33);
	}
} binder_depth_unpack;

static class cl_water_intensity : public R_constant_setup		
{	
	virtual void setup	(R_constant* C)
	{
		CEnvDescriptor&	E = *g_pGamePersistent->Environment().CurrentEnv;
		float fValue = E.m_fWaterIntensity;
		RCache.set_c	(C, fValue, fValue, fValue, 0);
	}
}	binder_water_intensity;

static class cl_sun_shafts_intensity : public R_constant_setup		
{	
	virtual void setup	(R_constant* C)
	{
		CEnvDescriptor&	E = *g_pGamePersistent->Environment().CurrentEnv;
		float fValue = E.m_fSunShaftsIntensity;
		RCache.set_c	(C, fValue, fValue, fValue, 0);
	}
}	binder_sun_shafts_intensity;

static class cl_alpha_ref	: public R_constant_setup 
{	
	virtual void setup (R_constant* C) 
	{ 
		StateManager.BindAlphaRef(C);
	}
} binder_alpha_ref;

//////////////////////////////////////////////////////////////////////////
// Just two static storage
void CRender::create()
{
	Device.seqFrame.Add	(this,REG_PRIORITY_HIGH+0x12345678);

	m_skinning			= -1;

	// hardware
	o.smapsize			= ps_r2_smapsize;

	o.nullrt = false;

	// SMAP / DST
	o.HW_smap_FETCH4	= FALSE;
	o.HW_smap			= true;
	o.HW_smap_PCF		= o.HW_smap		;
	if (o.HW_smap)		
	{
		//	For ATI it's much faster on DX10 to use D32F format
			o.HW_smap_FORMAT	= DXGI_FORMAT_R24G8_TYPELESS;
		Msg				("* HWDST/PCF supported and used");
	}

	// search for ATI formats
	if (!o.HW_smap)	
	{
		o.HW_smap		= true;
		if (o.HW_smap)	{
			o.HW_smap_FORMAT= MAKEFOURCC	('D','F','2','4');
			o.HW_smap_PCF	= FALSE			;
			o.HW_smap_FETCH4= TRUE			;
		}
		Msg ("* DF24/F4 supported and used [%X]", o.HW_smap_FORMAT);
	}

	// nvstencil on NV40 and up
	o.nvstencil = FALSE;
	if (Core.ParamsData.test(ECoreParams::nonvs))
		o.nvstencil	= FALSE;

	// nv-dbt
	o.nvdbt = false;

	o.no_ram_textures = ps_r__common_flags.test(RFLAG_NO_RAM_TEXTURES);
	if(o.no_ram_textures) {
		Msg("* Managed textures disabled");
	}

	// options
	o.sunstatic = !ps_r2_ls_flags.test(R2FLAG_SUN) ? TRUE : FALSE;
	o.volumetricfog = ps_r2_ls_flags.test(R3FLAG_VOLUMETRIC_SMOKE);
	o.noshadows = Core.ParamsData.test(ECoreParams::noshadows);
	o.Tshadows = Core.ParamsData.test(ECoreParams::tsh);
	o.distortion_enabled = !Core.ParamsData.test(ECoreParams::nodistort);
	o.distortion = o.distortion_enabled;
	o.disasm = Core.ParamsData.test(ECoreParams::disasm);

	if(ps_r_ssao) {
		SSAO = ps_r2_ls_flags_ssao;

		if(SSAO.test(ESSAO_DATA::SSAO_HDAO) || SSAO.test(ESSAO_DATA::SSAO_GTAO)) {
			SSAO.set(ESSAO_DATA::SSAO_OPT_DATA, false);
		}
	}

	o.dx11_enable_tessellation = RFeatureLevel >= D3D_FEATURE_LEVEL_11_0 && ps_r2_ls_flags_ext.test(R2FLAGEXT_ENABLE_TESSELLATION);

	// constants
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("parallax", &binder_parallax);
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("water_intensity", &binder_water_intensity);
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("sun_shafts_intensity", &binder_sun_shafts_intensity);
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("m_AlphaRef", &binder_alpha_ref);
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("pos_decompression_params", &binder_pos_decompress_params);
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("pos_decompression_params2", &binder_pos_decompress_params2);
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("pos_decompression_params_hud", &binder_pos_decompress_params_hud);
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("depth_unpack", &binder_depth_unpack);
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("triLOD", &binder_LOD);

	c_lmaterial = "L_material";
	c_sbase = "s_base";

	m_bMakeAsyncSS = false;

	Target = new CRenderTarget();	// Main target

	Models = new CModelPool();
	PSLibrary.OnCreate();
	HWOCC.occq_create(occq_size);

	rmNormal();
	marker = 0;

	D3D_QUERY_DESC qdesc{};
	qdesc.MiscFlags = 0;
	qdesc.Query = D3D_QUERY_EVENT;

	ZeroMemory(q_sync_point, sizeof(q_sync_point));

	for(u32 i = 0; i < 2; ++i) {
		R_CHK(RDevice->CreateQuery(&qdesc, &q_sync_point[i]));
	}

	RContext->End(q_sync_point[0]);

	xrRender_apply_tf();
	::PortalTraverser.initialize();

	FluidManager.Initialize(70, 70, 70);
	FluidManager.SetScreenSize((u32)RCache.get_width(), (u32)RCache.get_height());
}

void CRender::destroy() {
	m_bMakeAsyncSS = false;
	FluidManager.Destroy();
	::PortalTraverser.destroy();

	for(u32 i = 0; i < 2; ++i) {
		_RELEASE(q_sync_point[i]);
	}

	HWOCC.occq_destroy();
	xr_delete(Models);
	xr_delete(Target);
	PSLibrary.OnDestroy();
	Device.seqFrame.Remove(this);
	r_dsgraph_destroy();
}

void CRender::reset_begin() {
	if (b_loaded)
	{
		Details->Unload();
		xr_delete(Details);
	}
	xr_delete(Target);
	HWOCC.occq_destroy();

	for(u32 i = 0; i < 2; ++i) {
		_RELEASE(q_sync_point[i]);
	}
}

void CRender::reset_end() {
	D3D_QUERY_DESC			qdesc{};
	qdesc.MiscFlags = 0;
	qdesc.Query = D3D_QUERY_EVENT;

	for(u32 i = 0; i < 2; ++i) {
		R_CHK(RDevice->CreateQuery(&qdesc, &q_sync_point[i]));
	}
	//	Prevent error on first get data
	RContext->End(q_sync_point[0]);

	HWOCC.occq_create(occq_size);

	if (b_loaded)
	{
		Details = new CDetailManager();
		Details->Load();
	}
	Target = new CRenderTarget();

	xrRender_apply_tf();
	FluidManager.SetScreenSize((u32)RCache.get_width(), (u32)RCache.get_height());

	// Set this flag true to skip the first render frame,
	// that some data is not ready in the first frame (for example device camera position)
	m_bFirstFrameAfterReset = true;
}

void CRender::OnFrame() 
{
	Models->DeleteQueue();
}

// Implementation
IRender_ObjectSpecific* CRender::ros_create(IRenderable* parent) {
	return new CROS_impl();
}

void CRender::ros_destroy(IRender_ObjectSpecific*& p) {
	xr_delete(p);
}

IRenderVisual* CRender::model_Create(LPCSTR name, IReader* data) {
	return Models->Create(name, data);
}

IRenderVisual* CRender::model_CreateChild(LPCSTR name, IReader* data) {
	return Models->CreateChild(name, data);
}

IRenderVisual* CRender::model_Duplicate(IRenderVisual* V) {
	return Models->Instance_Duplicate((dxRender_Visual*)V);
}

void CRender::model_Delete(IRenderVisual*& V, BOOL bDiscard) {
	dxRender_Visual* pVisual = (dxRender_Visual*)V;
	Models->Delete(pVisual, bDiscard);
	V = 0;
}

IRender_DetailModel* CRender::model_CreateDM(IReader* F) {
	CDetail* D = new CDetail();
	D->Load(F);
	return D;
}

void CRender::model_Delete(IRender_DetailModel*& F) {
	if(F) {
		CDetail* D = (CDetail*)F;
		D->Unload();
		xr_delete(D);
		F = nullptr;
	}
}

IRenderVisual* CRender::model_CreatePE(LPCSTR name) {
	PS::CPEDef* SE = PSLibrary.FindPED(name);
	R_ASSERT3(SE, "Particle effect doesn't exist", name);
	return Models->CreatePE(SE);
}

IRenderVisual* CRender::model_CreateParticles(LPCSTR name) {
	PS::CPEDef* SE = PSLibrary.FindPED(name);
	if(SE) {
		return Models->CreatePE(SE);
	}
	else {
		PS::CPGDef* SG = PSLibrary.FindPGD(name);
		R_ASSERT3(SG, "Particle effect or group doesn't exist", name);
		return Models->CreatePG(SG);
	}
}

void CRender::models_Prefetch() {
	Models->Prefetch();
}

void CRender::models_Clear(BOOL b_complete) {
	Models->ClearPool(b_complete);
}

ref_shader CRender::getShader(int id) {
	VERIFY(id<int(Shaders.size()));	return Shaders[id];
}

IRender_Portal* CRender::getPortal(int id) {
	VERIFY(id<int(Portals.size()));	return Portals[id];
}

IRender_Sector* CRender::getSector(int id) {
	VERIFY(id<int(Sectors.size()));	return Sectors[id];
}

IRender_Sector* CRender::getSectorActive() {
	return pLastSector;
}

IRenderVisual* CRender::getVisual(int id) {
	VERIFY(id<int(Visuals.size()));	return Visuals[id];
}

D3DVERTEXELEMENT9* CRender::getVB_Format(int id, BOOL	_alt) {
	if(_alt) {
		VERIFY(id<int(xDC.size()));	return xDC[id].begin();
	}
	else {
		VERIFY(id<int(nDC.size()));	return nDC[id].begin();
	}
}

ID3DVertexBuffer* CRender::getVB(int id, BOOL	_alt) {
	if(_alt) {
		VERIFY(id<int(xVB.size()));	return xVB[id];
	}
	else {
		VERIFY(id<int(nVB.size()));	return nVB[id];
	}
}

ID3DIndexBuffer* CRender::getIB(int id, BOOL	_alt) {
	if(_alt) {
		VERIFY(id<int(xIB.size()));	return xIB[id];
	}
	else {
		VERIFY(id<int(nIB.size()));	return nIB[id];
	}
}

FSlideWindowItem* CRender::getSWI(int id) {
	VERIFY(id<int(SWIs.size()));
	return &SWIs[id];
}
IRender_Target* CRender::getTarget() {
	return Target;
}

IRender_Light* CRender::light_create() {
	return Lights.Create();
}
IRender_Glow* CRender::glow_create() {
	return new CGlow();
}

void CRender::flush() {
	r_dsgraph_render_graph(0);
}

BOOL CRender::occ_visible(vis_data& P) {
	return HOM.visible(P);
}

BOOL CRender::occ_visible(sPoly& P) {
	return HOM.visible(P);
}

BOOL CRender::occ_visible(Fbox& P) {
	return HOM.visible(P);
}

void CRender::add_Visual(IRenderVisual* V, bool ignore_opt) {
	add_leafs_Dynamic((dxRender_Visual*)V, ignore_opt);
}
void CRender::add_Geometry(IRenderVisual* V) {
	add_Static((dxRender_Visual*)V, View->getMask());
}

void CRender::add_StaticWallmark(ref_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* verts) {
	if(T->suppress_wm) {
		return;
	}
	VERIFY2(_valid(P) && _valid(s) && T && verts && (s > EPS_L), "Invalid static wallmark params");
	Wallmarks->AddStaticWallmark(T, verts, P, &*S, s);
}

void CRender::add_StaticWallmark(IWallMarkArray* pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V) {
	dxWallMarkArray* pWMA = (dxWallMarkArray*)pArray;
	ref_shader* pShader = pWMA->dxGenerateWallmark();
	if(pShader) {
		add_StaticWallmark(*pShader, P, s, T, V);
	}
}

void CRender::add_StaticWallmark(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V) {
	dxUIShader* pShader = (dxUIShader*)&*S;
	add_StaticWallmark (pShader->hShader, P, s, T, V);
}

void CRender::clear_static_wallmarks() {
	Wallmarks->clear();
}

void CRender::add_SkeletonWallmark(intrusive_ptr<CSkeletonWallmark> wm) {
	Wallmarks->AddSkeletonWallmark(wm);
}

void CRender::add_SkeletonWallmark(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size) {
	Wallmarks->AddSkeletonWallmark(xf, obj, sh, start, dir, size);
}

void CRender::add_SkeletonWallmark(const Fmatrix* xf, IKinematics* obj, IWallMarkArray* pArray, const Fvector& start, const Fvector& dir, float size) {
	dxWallMarkArray* pWMA = (dxWallMarkArray*)pArray;
	ref_shader* pShader = pWMA->dxGenerateWallmark();
	if(pShader) {
		add_SkeletonWallmark(xf, (CKinematics*)obj, *pShader, start, dir, size);
	}
}

void CRender::add_Occluder(Fbox2& bb_screenspace) {
	HOM.occlude(bb_screenspace);
}

void CRender::set_Object(IRenderable* O) {
	val_pObject = O;
}

void CRender::rmNear() {
	IRender_Target* T = getTarget();
	D3D_VIEWPORT VP = {0,0,(float)T->get_width(),(float)T->get_height(),0,0.02f};

	RContext->RSSetViewports(1, &VP);
}

void CRender::rmFar() {
	IRender_Target* T = getTarget();
	D3D_VIEWPORT VP = {0,0,(float)T->get_width(),(float)T->get_height(),0.99999f,1.f};

	RContext->RSSetViewports(1, &VP);
}

void CRender::rmNormal() {
	IRender_Target* T = getTarget();
	D3D_VIEWPORT VP = {0,0,(float)T->get_width(),(float)T->get_height(),0,1.f};

	RContext->RSSetViewports(1, &VP);
}

CRender::SurfaceParams CRender::getSurface(const char* nameTexture)
{
	auto texture = DEV->_CreateTexture(nameTexture);
	SurfaceParams surface = {};
	surface.Surface = texture->get_SRView();
	surface.w = texture->get_Width();
	surface.h = texture->get_Height();

	return surface;
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CRender::CRender() : m_bFirstFrameAfterReset(false)
{
	init_cacades();
}

CRender::~CRender()
{
	for(auto& it : SWIs) {
		xr_free(it.sw);
		it.sw = nullptr;
		it.count = 0;
	}
	SWIs.clear();
}

#include "../../xrEngine/GameFont.h"
void CRender::Statistics(CGameFont* _F) {
	CGameFont& F = *_F;
	F.OutSet(250, 35);
	F.OutNext(" **** LT:%2d,LV:%2d **** ", stats.l_total, stats.l_visible);	stats.l_visible = 0;
	F.OutNext("    S(%2d)   | (%2d)NS   ", stats.l_shadowed, stats.l_unshadowed);
	F.OutNext("smap use[%2d], merge[%2d], finalclip[%2d]", stats.s_used, stats.s_merged - stats.s_used, stats.s_finalclip);
	stats.s_used = 0; stats.s_merged = 0; stats.s_finalclip = 0;
	F.OutSkip();
	F.OutNext(" **** Occ-Q(%03.1f) **** ", 100.f * f32(stats.o_culled) / f32(stats.o_queries ? stats.o_queries : 1));
	F.OutNext(" total  : %2d", stats.o_queries);	stats.o_queries = 0;
	F.OutNext(" culled : %2d", stats.o_culled);	stats.o_culled = 0;
	F.OutSkip();
	u32	ict = stats.ic_total + stats.ic_culled;
	F.OutNext(" **** iCULL(%03.1f) **** ", 100.f * f32(stats.ic_culled) / f32(ict ? ict : 1));
	F.OutNext(" visible: %2d", stats.ic_total);	stats.ic_total = 0;
	F.OutNext(" culled : %2d", stats.ic_culled);	stats.ic_culled = 0;
#ifdef DEBUG
	HOM.stats();
#endif
}

xr_string CRender::getShaderParams() {
	xr_string params = "";
	if(!m_ShaderOptions.empty()) {
		params.append("(").append(m_ShaderOptions[0].Name);

		for(size_t i = 1u; i < m_ShaderOptions.size(); ++i) {
			params.append(",").append(m_ShaderOptions[i].Name);
		}

		params.append(")");
	}
	return params;
}

void CRender::addShaderOption(const char* name, const char* value) {
	D3D_SHADER_MACRO macro = {name, value};
	m_ShaderOptions.push_back(macro);
}

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
	if(SUCCEEDED(_hr) && pReflection) {
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
	if(pTarget[0] == 'p') {
		SPS* sps_result = (SPS*)result;
		_result = RDevice->CreatePixelShader(buffer, buffer_size, 0, &sps_result->ps);
		if(!SUCCEEDED(_result)) {
			Msg("! PS: %s", file_name);
			Msg("! CreatePixelShader hr == 0x%08x", _result);
			return		E_FAIL;
		}

		ID3DShaderReflection* pReflection = 0;

		_result = D3DReflect(buffer, buffer_size, IID_ID3DShaderReflection, (void**)&pReflection);

		//	Parse constant, texture, sampler binding
		//	Store input signature blob
		if(SUCCEEDED(_result) && pReflection) {
			//	Let constant table parse it's data
			sps_result->constants.parse(pReflection, RC_dest_pixel);

			_RELEASE(pReflection);
		}
		else {
			Msg("! PS: %s", file_name);
			Msg("! D3DReflectShader hr == 0x%08x", _result);
		}
	}
	else if(pTarget[0] == 'v') {
		SVS* svs_result = (SVS*)result;
		_result = RDevice->CreateVertexShader(buffer, buffer_size, 0, &svs_result->vs);

		if(!SUCCEEDED(_result)) {
			Msg("! VS: %s", file_name);
			Msg("! CreatePixelShader hr == 0x%08x", _result);
			return		E_FAIL;
		}

		ID3DShaderReflection* pReflection = 0;
		_result = D3DReflect(buffer, buffer_size, IID_ID3DShaderReflection, (void**)&pReflection);

		//	Parse constant, texture, sampler binding
		//	Store input signature blob
		if(SUCCEEDED(_result) && pReflection) {
			//	TODO: DX10: share the same input signatures

			//	Store input signature (need only for VS)
			//CHK_DX( D3DxxGetInputSignatureBlob(pShaderBuf->GetBufferPointer(), pShaderBuf->GetBufferSize(), &_vs->signature) );
			ID3DBlob* pSignatureBlob;
			CHK_DX(D3DGetInputSignatureBlob(buffer, buffer_size, &pSignatureBlob));
			VERIFY(pSignatureBlob);

			svs_result->signature = dxRenderDeviceRender::Instance().Resources->_CreateInputSignature(pSignatureBlob);

			_RELEASE(pSignatureBlob);

			//	Let constant table parse it's data
			svs_result->constants.parse(pReflection, RC_dest_vertex);

			_RELEASE(pReflection);
		}
		else {
			Msg("! VS: %s", file_name);
			Msg("! D3DXFindShaderComment hr == 0x%08x", _result);
		}
	}
	else if(pTarget[0] == 'g') {
		SGS* sgs_result = (SGS*)result;
		_result = RDevice->CreateGeometryShader(buffer, buffer_size, 0, &sgs_result->gs);
		if(!SUCCEEDED(_result)) {
			Msg("! GS: %s", file_name);
			Msg("! CreateGeometryShaderhr == 0x%08x", _result);
			return		E_FAIL;
		}

		ID3DShaderReflection* pReflection = 0;

		_result = D3DReflect(buffer, buffer_size, IID_ID3DShaderReflection, (void**)&pReflection);

		//	Parse constant, texture, sampler binding
		//	Store input signature blob
		if(SUCCEEDED(_result) && pReflection) {
			//	Let constant table parse it's data
			sgs_result->constants.parse(pReflection, RC_dest_geometry);

			_RELEASE(pReflection);
		}
		else {
			Msg("! PS: %s", file_name);
			Msg("! D3DReflectShader hr == 0x%08x", _result);
		}
	}
	else if(pTarget[0] == 'c') {
		_result = create_shader(pTarget, buffer, buffer_size, file_name, (SCS*&)result, disasm);
	}
	else if(pTarget[0] == 'h') {
		_result = create_shader(pTarget, buffer, buffer_size, file_name, (SHS*&)result, disasm);
	}
	else if(pTarget[0] == 'd') {
		_result = create_shader(pTarget, buffer, buffer_size, file_name, (SDS*&)result, disasm);
	}
	else {
		NODEFAULT;
	}

	if(disasm) {
		ID3DBlob* disasm_ = 0;
		D3DDisassemble(buffer, buffer_size, FALSE, 0, &disasm_);
		string_path		dname;
		xr_strconcat(dname, "disasm\\", file_name, ('v' == pTarget[0]) ? ".vs.hlsl" : ('p' == pTarget[0]) ? ".ps.hlsl" : ".gs.hlsl");
		IWriter* W = FS.w_open("$logs$", dname);
		W->w(disasm_->GetBufferPointer(), (u32)disasm_->GetBufferSize());
		FS.w_close(W);
		_RELEASE(disasm_);
	}

	return _result;
}

class includer : public ID3DInclude {
public:
	HRESULT  __stdcall Open(D3D_INCLUDE_TYPE IncludeType, LPCSTR pFileName, LPCVOID pParentData, LPCVOID* ppData, UINT* pBytes) {
		string_path pname;
		xr_strconcat(pname, ::Render->getShaderPath(), pFileName);
		IReader* R = FS.r_open("$game_shaders$", pname);
		if(0 == R) {
			// possibly in shared directory or somewhere else - open directly
			R = FS.r_open("$game_shaders$", pFileName);
			if(0 == R) {
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
	char c_ssao[32];
	char c_sun_quality[32];

	char sh_name[MAX_PATH] = "";

	for(u32 i = 0; i < m_ShaderOptions.size(); ++i) {
		defines[def_it++] = m_ShaderOptions[i];
	}

	u32 len = xr_strlen(sh_name);

	// options
	{
		xr_sprintf(c_smapsize, "%04d", u32(o.smapsize));

		defines[def_it].Name = "SMAP_size";
		defines[def_it].Definition = c_smapsize;

		def_it++;

		VERIFY(xr_strlen(c_smapsize) == 4);
		xr_strcat(sh_name, c_smapsize); len += 4;
	}

	if(ps_r2_ls_flags_ext.test(RFLAG_CLOUD_SHADOWS)) {
		defines[def_it].Name = "USE_SUNMASK";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(ps_r2_ls_flags_ext.test(RFLAG_CLOUD_SHADOWS)); ++len;

	if(o.sunstatic) {
		defines[def_it].Name = "USE_R2_STATIC_SUN";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(o.sunstatic); ++len;

	bool HasBlur = SSAO.test(ESSAO_DATA::SSAO_BLUR);
	bool HasHDAO = SSAO.test(ESSAO_DATA::SSAO_HDAO);

	if(HasBlur) {
		defines[def_it].Name = "USE_SSAO_BLUR";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(HasBlur); ++len;

	if(HasHDAO) {
		defines[def_it].Name = "HDAO";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(HasHDAO); ++len;

	// skinning
	if(m_skinning < 0) {
		defines[def_it].Name = "SKIN_NONE";
		defines[def_it].Definition = "1";
		def_it++;

		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0'; ++len;
	}

	if(0 == m_skinning) {
		defines[def_it].Name = "SKIN_0";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(0 == m_skinning); ++len;

	if(1 == m_skinning) {
		defines[def_it].Name = "SKIN_1";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(1 == m_skinning); ++len;

	if(2 == m_skinning) {
		defines[def_it].Name = "SKIN_2";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(2 == m_skinning); ++len;

	if(3 == m_skinning) {
		defines[def_it].Name = "SKIN_3";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(3 == m_skinning); ++len;

	if(4 == m_skinning) {
		defines[def_it].Name = "SKIN_4";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(4 == m_skinning); ++len;

	//	Igor: need restart options
	if(ps_r2_ls_flags.test(R2FLAG_SOFT_WATER)) {
		defines[def_it].Name = "USE_SOFT_WATER";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if(ps_r2_ls_flags.test(R2FLAG_SOFT_PARTICLES)) {
		defines[def_it].Name = "USE_SOFT_PARTICLES";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if(ps_r2_ls_flags.test(R2FLAG_DOF)) {
		defines[def_it].Name = "USE_DOF";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if(ps_r2_ls_flags_ext.test(R4FLAG_SCREEN_SPACE_HUD_SHADOWS)) {
		defines[def_it].Name = "USE_HUD_SHADOWS";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if(ps_r2_ls_flags_ext.test(R4FLAG_HASHED_ALPHA_TEST)) {
		defines[def_it].Name = "USE_HASHED_AREF";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if(ps_r2_ls_flags_ext.test(R4FLAG_SSLR_ON_WATER)) {
		defines[def_it].Name = "USE_SSLR_ON_WATER";
		defines[def_it].Definition = "1";

		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if(ps_r_sun_shafts > 0) {
		xr_sprintf(c_sun_shafts, "%d", ps_r_sun_shafts);
		defines[def_it].Name = "SUN_SHAFTS_QUALITY";
		defines[def_it].Definition = c_sun_shafts;

		def_it++;
		sh_name[len] = '0' + static_cast<char>(ps_r_sun_shafts); ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if(ps_r_ssao > 0) {
		xr_sprintf(c_ssao, "%d", ps_r_ssao);
		defines[def_it].Name = "SSAO_QUALITY";
		defines[def_it].Definition = c_ssao;

		def_it++;
		sh_name[len] = '0' + static_cast<char>(ps_r_ssao); ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if(ps_r_sun_quality > 0) {
		xr_sprintf(c_sun_quality, "%d", ps_r_sun_quality);
		defines[def_it].Name = "SUN_QUALITY";
		defines[def_it].Definition = c_sun_quality;

		def_it++;
		sh_name[len] = '0' + static_cast<char>(ps_r_sun_quality); ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if(ps_r2_ls_flags.test(R2FLAG_STEEP_PARALLAX)) {
		defines[def_it].Name = "ALLOW_STEEPPARALLAX";
		defines[def_it].Definition = "1";
		def_it++;
		sh_name[len] = '1'; ++len;
	}
	else {
		sh_name[len] = '0';	++len;
	}

	if(RFeatureLevel == D3D_FEATURE_LEVEL_10_1) {
		defines[def_it].Name = "SM_4_1";
		defines[def_it].Definition = "1";
		def_it++;
	}
	sh_name[len] = '0' + char(RFeatureLevel == D3D_FEATURE_LEVEL_10_1); ++len;

	if(RFeatureLevel >= D3D_FEATURE_LEVEL_11_0) {
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

	if(0 == xr_strcmp(pFunctionName, "main")) {
		if('v' == pTarget[0]) {
			if(RFeatureLevel == D3D_FEATURE_LEVEL_10_0) {
				pTarget = "vs_4_0";
			}
			else if(RFeatureLevel == D3D_FEATURE_LEVEL_10_1) {
				pTarget = "vs_4_1";
			}
			else if(RFeatureLevel == D3D_FEATURE_LEVEL_11_0) {
				pTarget = "vs_5_0";
			}
			else if(RFeatureLevel == D3D_FEATURE_LEVEL_11_1) {
				pTarget = "vs_5_0";
			}
		}
		else if('p' == pTarget[0]) {
			if(RFeatureLevel == D3D_FEATURE_LEVEL_10_0) {
				pTarget = "ps_4_0";
			}
			else if(RFeatureLevel == D3D_FEATURE_LEVEL_10_1) {
				pTarget = "ps_4_1";
			}
			else if(RFeatureLevel == D3D_FEATURE_LEVEL_11_0) {
				pTarget = "ps_5_0";
			}
			else if(RFeatureLevel == D3D_FEATURE_LEVEL_11_1) {
				pTarget = "ps_5_0";
			}
		}
		else if('g' == pTarget[0]) {
			if(RFeatureLevel == D3D_FEATURE_LEVEL_10_0) {
				pTarget = "gs_4_0";
			}
			else if(RFeatureLevel == D3D_FEATURE_LEVEL_10_1) {
				pTarget = "gs_4_1";
			}
			else if(RFeatureLevel == D3D_FEATURE_LEVEL_11_0) {
				pTarget = "gs_5_0";
			}
			else if(RFeatureLevel == D3D_FEATURE_LEVEL_11_1) {
				pTarget = "gs_5_0";
			}
		}
		else if('c' == pTarget[0]) {
			if(RFeatureLevel == D3D_FEATURE_LEVEL_11_0) {
				pTarget = "cs_5_0";
			}
			else if(RFeatureLevel == D3D_FEATURE_LEVEL_11_1) {
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

	if(FS.exist(file_name) && ps_r__common_flags.test(RFLAG_USE_CACHE)) {
#ifdef DEBUG
		Msg("compilied shader library found %s", file_name);
#endif // DEBUG
		IReader* file = FS.r_open(file_name);

		if(file->length() > 4) {
			u32 ShaderCRC = file->r_u32();
			u32 CodeSRC = file->r_u32();

			if(RealCodeCRC == CodeSRC) {
				u32 const real_crc = crc32(file->pointer(), file->elapsed());
				if(real_crc == ShaderCRC) {
					_result = create_shader(pTarget, (DWORD*)file->pointer(), file->elapsed(), file_name, result, o.disasm);
				}
			}
		}
		file->close();
	}

	if(FAILED(_result)) {
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

		if(SUCCEEDED(_result)) {
			if(ps_r__common_flags.test(RFLAG_USE_CACHE)) {
				IWriter* file = FS.w_open(file_name);
				u32 const crc = crc32(pShaderBuf->GetBufferPointer(), pShaderBuf->GetBufferSize());
				file->w_u32(crc);
				file->w_u32(RealCodeCRC);
				file->w(pShaderBuf->GetBufferPointer(), (u32)pShaderBuf->GetBufferSize());
				FS.w_close(file);
			}
			_result = create_shader(pTarget, (DWORD*)pShaderBuf->GetBufferPointer(), (u32)pShaderBuf->GetBufferSize(), file_name, result, o.disasm);
		}
		else {
			Msg("! %s", file_name);

			if(pErrorBuf) {
				Msg("! error: %s", (LPCSTR)pErrorBuf->GetBufferPointer());
			}
			else {
				Msg("Can't compile shader hr=0x%08x", _result);
			}
		}
	}

	return _result;
}

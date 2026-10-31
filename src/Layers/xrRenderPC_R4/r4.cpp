#include "stdafx.h"
#include "r4.h"
#include "r1_GlowManager.h"
#include "FBasicVisual.h"
#include "../../xrEngine/xr_object.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#include "SkeletonCustom.h"
#include "LightTrack.h"
#include "dxRenderDeviceRender.h"
#include "dxWallMarkArray.h"
#include "dxUIShader.h"
#include "../../xrCore/git_version.h"

#include "3DFluid/dx103DFluidManager.h"
#include "ShaderResourceTraits.h"

#include "RenderInterfaceShared.h"
#include "OverlayAPI/DLSSWrapper.h"
#include "../../xrEngine/EngineAPI.h"
#include "r1_LightPPA.h"
#include "SVGStorage.h"

CRender RImplementation;

//////////////////////////////////////////////////////////////////////////
class CDynamicGlow : public IRender_Glow
{
public:
	bool				bActive;
public:
	CDynamicGlow() : bActive(false)		{ }
	virtual void					set_active			(bool b)					{ bActive=b;		}
	virtual bool					get_active			()							{ return bActive;	}
	virtual void					set_position		(const Fvector& P)			{ }
	virtual void					set_direction		(const Fvector& D)			{ }
	virtual void					set_radius			(float R)					{ }
	virtual void					set_texture			(const char* name)				{ }
	virtual void					set_color			(const Fcolor& C)			{ }
	virtual void					set_color			(float r, float g, float b)	{ }
};

float r_dtex_range = 50.f;

ShaderElement* CRender::rimp_select_sh_dynamic(dxRender_Visual* pVisual, float cdist_sq, bool is_hud)
{
	const u32 phase = g_r_cull_tls.active ? g_r_cull_tls.phase : RImplementation.phase;
	if (LightingModeIsStaticRender())
	{
		if (GraphMain.val_bUI && pVisual->shader->E[ShaderVariant::UI])
			return pVisual->shader->E[ShaderVariant::UI]._get();
		if (phase == PHASE_NORMAL)
			return pVisual->shader->E[L_Projector && L_Projector->shadowing() ? ShaderVariant::NormalHQ : ShaderVariant::NormalLQ]._get();
		return rimp_select_sh_static(pVisual, cdist_sq);
	}
	if (!!GraphMain.val_bUI)
	{
		const auto& E = pVisual->shader->E;
		return (E[ShaderVariant::UI] ? E[ShaderVariant::UI] : E[ShaderVariant::Special] ? E[ShaderVariant::Special] : E[ShaderVariant::NormalLQ])._get();
	}

	int id = ShaderVariant::Shadow;

	if (CRender::PHASE_NORMAL == phase)
	{
		if (is_hud)
		{
			if (auto pSh = pVisual->shader->E[ShaderVariant::HUD]._get())
			{
				return pSh;
			}
		}

		id = ((_sqrt(cdist_sq) - pVisual->vis.sphere.R) < r_dtex_range || psDeviceFlags.test(rsClearBB)) ? ShaderVariant::NormalHQ : ShaderVariant::NormalLQ;
	}
	else if (CRender::PHASE_REFLECT == phase)
	{
		id = ShaderVariant::Reflection;
	}

	return pVisual->shader->E[id]._get();
}

ShaderElement* CRender::rimp_select_sh_static(dxRender_Visual* pVisual, float cdist_sq)
{
	const u32 phase = g_r_cull_tls.active ? g_r_cull_tls.phase : RImplementation.phase;
	if (LightingModeIsStaticRender())
	{
		int id = ShaderVariant::NormalLQ;
		if (phase == PHASE_POINT) id = ShaderVariant::PointLight;
		else if (phase == PHASE_SPOT) id = ShaderVariant::SpotLight;
		else if (phase == PHASE_LMODELS) id = ShaderVariant::ModelLighting;
		else if ((_sqrt(cdist_sq) - pVisual->vis.sphere.R) < r_dtex_range) id = ShaderVariant::NormalHQ;
		return pVisual->shader->E[id]._get();
	}
	int id = ShaderVariant::Shadow;

	if (CRender::PHASE_NORMAL == phase)
	{
		id = ((_sqrt(cdist_sq) - pVisual->vis.sphere.R) < r_dtex_range || psDeviceFlags.test(rsClearBB)) ? ShaderVariant::NormalHQ : ShaderVariant::NormalLQ;
	}
	else if (CRender::PHASE_REFLECT == phase)
	{
		id = ShaderVariant::Reflection;
	}

	return pVisual->shader->E[id]._get();
}

//////////////////////////////////////////////////////////////////////////
// Just two static storage
void CRender::create()
{
	Engine.External.SetSkinningMode();

	Device.seqFrame.Add	(this,REG_PRIORITY_HIGH+0x12345678);
	Device.BeginReflectionCollect = xr_make_delegate(this, &CRender::begin_reflection_collect);
	Device.CollectReflections = xr_make_delegate(this, &CRender::collect_reflections);
	Device.ResetSunCollect = xr_make_delegate(this, &CRender::reset_sun_collect);
	Device.CollectSunCascades = xr_make_delegate(this, &CRender::collect_sun_cascades);
	Device.EnsureSunCollect = xr_make_delegate(this, &CRender::ensure_sun_collect);

	// hardware
	o.smapsize			= ps_r2_smapsize;

	o.nullrt = false;

	// SMAP / DST
	o.HW_smap_FORMAT = static_cast<u32>(ERHI_FORMAT::R24G8_TYPELESS);

	// nvstencil on NV40 and up
	o.nvstencil = false;
	if (Core.ParamsData.test(ECoreParams::nonvs))
		o.nvstencil	= false;

	// nv-dbt
	o.nvdbt = false;

	// options
	o.sunstatic = LightingModeIsStaticRender() || !ps_r2_ls_flags.test(R2FLAG_SUN);
	o.volumetricfog = LightingModeIsDynamicRender() && ps_r2_ls_flags.test(R3FLAG_VOLUMETRIC_SMOKE);
	o.noshadows = Core.ParamsData.test(ECoreParams::noshadows);
	o.distortion_enabled = !Core.ParamsData.test(ECoreParams::nodistort);
	o.distortion = o.distortion_enabled;
	o.disasm = Core.ParamsData.test(ECoreParams::disasm);

	if(LightingModeIsDynamicRender() && !EngineExternal().ShadersOptions.contains(xr_string("USE_LEGACY_LIGHT")))
	{
		o.deffered_reflecitons = true; //!!ps_r2_ls_flags_ext.test(R4FLAG_SSLR_ON_WORLD);
		o.offscreen_reflecitons = true; //!!ps_r2_ls_flags_ext.test(R4FLAG_OFFSCREEN_REFLECTIONS);
		o.dx11_use_legacy_light = false;
	}
	else
	{
		o.deffered_reflecitons = o.offscreen_reflecitons = false;
		o.dx11_use_legacy_light = true;
	}

	if (LightingModeIsStaticRender())
		o.deffered_reflecitons = o.offscreen_reflecitons = false;

	o.RainPuddles = LightingModeIsDynamicRender() && o.dx11_use_legacy_light && ps_r2_ls_flags_ext.test(R4FLAG_RAIN_PUDDLES);

	o.dx11_disable_motion_vectors = MotionVectorsDisabled();

	o.dx11_allow_wboit_transparency = LightingModeIsDynamicRender() && !!EngineExternal().ShadersOptions.contains(xr_string("ALLOW_WBOIT_TRANSPARENCY"));

	o.dx11_enable_tessellation = LightingModeIsDynamicRender() && ps_r2_ls_flags_ext.test(R2FLAGEXT_ENABLE_TESSELLATION);


	m_bMakeAsyncSS = false;

	if (LightingModeIsDynamicRender())
		g_DLSSWrapper.Create();
	else
		L_Dynamic = new CLightR_Manager();

	Target = new CRenderTarget();	// Main target

	// The editor device owns the model pool and the particle library (see r4_editor.cpp);
	// without a level the editors also need their own sun.
	if (Device.IsEditorMode())
	{
		Lights.LoadEditor();
		EditorCreate();
	}
	else
	{
		Models = new CModelPool();
		PSLibrary.OnCreate();
	}
	HWOCC.occq_create(occq_size);

	rmNormal();

	GraphMain.marker = 0;

	for (R_dsgraph_structure& Graph : GraphReflection)
	{
		Graph.marker = 0;
		Graph.PortalTraverser.initialize();
	}

	for (R_dsgraph_structure& Graph : GraphSun)
	{
		Graph.marker = 0;
		Graph.PortalTraverser.initialize();
	}

	xrRender_apply_tf();
	GraphMain.PortalTraverser.initialize();

	if (LightingModeIsDynamicRender())
	{
		FluidManager.Initialize(70, 70, 70);
		FluidManager.SetScreenSize((u32)GfxCmdList::GetWidth(), (u32)GfxCmdList::GetHeight());
	}

	Device.ModelDefferClear = xr_make_delegate(Models, &CModelPool::DeleteQueuedDeffer);
}

void CRender::destroy() 
{
	// Release the editor's lazily created brush resources while the device is still
	// alive - their destructors dereference DEV, which is null by DLL detach time.
	DetailLayers_EditorDestroy();

	m_bMakeAsyncSS = false;
	if (LightingModeIsDynamicRender())
		FluidManager.Destroy();
	xr_delete(L_Dynamic);
	GraphMain.PortalTraverser.destroy();

	HWOCC.occq_destroy();
	xr_delete(Target);
	if (Device.IsEditorMode())
	{
		EditorDestroy();
		Lights.Unload();
	}
	else
	{
		xr_delete(Models);
		PSLibrary.OnDestroy();
	}
	Device.seqFrame.Remove(this);

	GraphMain.r_dsgraph_destroy();

	for (R_dsgraph_structure& Graph : GraphReflection)
	{
		Graph.r_dsgraph_destroy();
		Graph.PortalTraverser.destroy();
	}

	for (R_dsgraph_structure& Graph : GraphSun)
	{
		Graph.r_dsgraph_destroy();
		Graph.PortalTraverser.destroy();
	}

	Device.ModelDefferClear = nullptr;
	Device.BeginReflectionCollect = nullptr;
	Device.CollectReflections = nullptr;
	Device.ResetSunCollect = nullptr;
	Device.CollectSunCascades = nullptr;
	Device.EnsureSunCollect = nullptr;

	if (LightingModeIsDynamicRender())
		g_DLSSWrapper.Destroy();
}

void CRender::reset_begin() {
	// Its compute pass reads the scene depth target, which is recreated
	if (Device.IsEditorMode())
	{
		EditorHiZ.Destroy();
	}

	xr_delete(L_Projector);
	xr_delete(L_Shadows);
	// Update incremental shadowmap-visibility solver
	// BUG-ID: 10646
	{
		u32 it = 0;
		for(it = 0; it < Lights_LastFrame.size(); it++) {
			if(0 == Lights_LastFrame[it])	continue;
			try {
				Lights_LastFrame[it]->svis.resetoccq();
			}
			catch(...) {
				Msg("! Failed to flush-OCCq on light [%d] %X", it, *(u32*)(&Lights_LastFrame[it]));
			}
		}
		Lights_LastFrame.clear();
	}

	if (GraphMain.b_loaded)
	{
		Details->Unload();
		xr_delete(Details);
	}
	xr_delete(Target);
	HWOCC.occq_destroy();

	if(LightingModeIsDynamicRender() && !EngineExternal().ShadersOptions.contains(xr_string("USE_LEGACY_LIGHT"))) 
	{
		o.deffered_reflecitons = !!ps_r2_ls_flags_ext.test(R4FLAG_SSLR_ON_WORLD);
		o.offscreen_reflecitons = !!ps_r2_ls_flags_ext.test(R4FLAG_OFFSCREEN_REFLECTIONS);
	}
	else
	{
		o.deffered_reflecitons = o.offscreen_reflecitons = false;
	}
	o.RainPuddles = LightingModeIsDynamicRender() && o.dx11_use_legacy_light && ps_r2_ls_flags_ext.test(R4FLAG_RAIN_PUDDLES);
}

void CRender::reset_end()
{
	//	Prevent error on first get data
	HWOCC.occq_create(occq_size);

	if (GraphMain.b_loaded)
	{
		Details = new CDetailManager();
		Details->Load();
	}
	Target = new CRenderTarget();

	if (GraphMain.b_loaded && LightingModeIsStaticRender())
	{
		L_Projector = new CLightProjector();
		L_Shadows = new CLightShadows();
	}

	if (L_Glows)
	{
		L_Glows->reset_geometry();
	}

	xrRender_apply_tf();
	if (LightingModeIsDynamicRender())
	{
		FluidManager.SetScreenSize((u32)GfxCmdList::GetWidth(), (u32)GfxCmdList::GetHeight());
	}

	// Set this flag true to skip the first render frame,
	// that some data is not ready in the first frame (for example device camera position)
	m_bFirstFrameAfterReset = true;
}

void CRender::OnFrame()
{
	Models->DeleteQueue();

	//Lights Delete queue
	for (light* L : v_all_lights_dque)
		xr_delete(L);
	v_all_lights_dque.clear();
}

// Implementation
IRender_ObjectSpecific* CreateEditorObjectSpecific();

IRender_ObjectSpecific* CRender::ros_create(IRenderable* parent) {
	if (Device.IsEditorMode())
	{
		return CreateEditorObjectSpecific();
	}

	return new CROS_impl();
}

void CRender::ros_destroy(IRender_ObjectSpecific*& p) {
	xr_delete(p);
}

IRenderVisual* CRender::model_Create(const char* name, IReader* data) {
	return Models->Create(name, data);
}

IRenderVisual* CRender::model_CreateChild(const char* name, IReader* data) {
	return Models->CreateChild(name, data);
}

IRenderVisual* CRender::model_Duplicate(IRenderVisual* V) {
	return Models->Instance_Duplicate((dxRender_Visual*)V);
}

void CRender::model_Delete(IRenderVisual*& V, bool bDiscard) {
	dxRender_Visual* pVisual = (dxRender_Visual*)V;
	if (Models != nullptr)
	{
		Models->Delete(pVisual, bDiscard);
	}
	V = 0;
}

void CRender::model_Delete_Deffered(IRenderVisual*& V) {
	dxRender_Visual* pVisual = (dxRender_Visual*)V;
	Models->DeleteDeffered(pVisual);
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

IRenderVisual* CRender::model_CreatePE(const char* name) {
	PS::CPEDef* SE = PSLibrary.FindPED(name);
	R_ASSERT3(SE, "Particle effect doesn't exist", name);
	return Models->CreatePE(SE);
}

IRenderVisual* CRender::model_CreateParticles(const char* name) {
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

RHIInputElementDesc* CRender::getVB_Format(int id, size_t* Count, bool	_alt)
{
	if(_alt)
	{
		*Count = xDC[id].size();
		return xDC[id].begin();
	}
	else
	{
		*Count = nDC[id].size();
		return nDC[id].begin();
	}
}

IRHIBuffer* CRender::getVB(int id, bool	_alt) {
	if(_alt) {
		VERIFY(id<int(xVB.size()));	return xVB[id];
	}
	else {
		VERIFY(id<int(nVB.size()));	return nVB[id];
	}
}

IRHIBuffer* CRender::getIB(int id, bool	_alt) {
	if(_alt) {
		VERIFY(id<int(xIB.size()));	return xIB[id];
	}
	else {
		VERIFY(id<int(nIB.size()));	return nIB[id];
	}
}

FSlideWindowItem* CRender::getSWI(int id)
{
	VERIFY(id<int(SWIs.size()));
	return &SWIs[id];
}

IRender_Light* CRender::light_create()
{
	return Lights.Create();
}

IRender_Glow* CRender::glow_create()
{
	return LightingModeIsStaticRender() ? static_cast<IRender_Glow*>(new CGlow()) : new CDynamicGlow();
}

void CRender::set_Object(IRenderable* O, void* graph)
{
	R_dsgraph_structure& Graph = TargetGraph(graph);
	Graph.val_pObject = O;

	// Isolated graphs (sun / reflections) run on PreRenderThread while GraphMain
	// is built on the render thread. Projector and R1 shadows belong to GraphMain.
	if (graph && graph != &GraphMain)
		return;

	if (L_Projector) L_Projector->set_object(phase == PHASE_NORMAL ? O : nullptr);
	if (L_Shadows) L_Shadows->set_object(phase == PHASE_NORMAL ? O : nullptr);
}

CRender::SurfaceParams CRender::getSurface(const char* nameTexture)
{
	auto texture = DEV->_CreateTexture(nameTexture);
	texture->Load();
	SurfaceParams surface = {};
	surface.Surface = texture->GetView()->GetRawSRV();
	surface.w = (float)texture->get_Width();
	surface.h = (float)texture->get_Height();

	return surface;
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CRender::CRender() : m_bFirstFrameAfterReset(false)
{
	GraphMain.Backend = &Backend;
	for (R_dsgraph_structure& Graph : GraphReflection)
	{
		Graph.Backend = &Backend;
	}
	for (R_dsgraph_structure& Graph : GraphSun)
	{
		Graph.Backend = &Backend;
	}
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

namespace
{
	u32 DefinesCRC(const ShaderExternalMap& Defines)
	{
		xr_vector<xr_string> Parts;
		Parts.reserve(Defines.size());
		for (const auto& [Name, Value] : Defines)
		{
			Parts.push_back(Name + Value);
		}
		std::sort(Parts.begin(), Parts.end());
		u32 CRC = 0xffffffff;
		for (const xr_string& Part : Parts)
		{
			CRC = crc32(Part.data(), Part.size(), CRC);
		}
		return CRC;
	}
}

ShaderExternalMap CRender::GetShaderDefines(const ShaderExternalMap& Defines) const
{
	const bool Dynamic = LightingModeIsDynamicRender();
	const bool Static = LightingModeIsStaticRender();
	const int Skinning = Engine.External.GetSkinningMode();
	string16 SmapSize, SunShafts, SunQuality;
	xr_sprintf(SmapSize, "%04u", u32(o.smapsize));
	xr_sprintf(SunShafts, "%d", ps_r_sun_shafts);
	xr_sprintf(SunQuality, "%d", ps_r_sun_quality);

	const struct
	{
		bool Enabled;
		const char* Name;
		const char* Value;
	} Flags[] =
	{
		{ true, "SMAP_size", SmapSize },
		{ Dynamic && ps_r2_ls_flags_ext.test(RFLAG_CLOUD_SHADOWS), "USE_SUNMASK", "1" },
		{ Dynamic && o.sunstatic, "USE_R2_STATIC_SUN", "1" },
		{ Skinning < 0, "SKIN_NONE", "1" },
		{ Skinning == 0, "SKIN_0", "1" },
		{ Skinning == 1, "SKIN_1", "1" },
		{ Skinning == 2, "SKIN_2", "1" },
		{ Skinning == 3, "SKIN_3", "1" },
		{ Skinning == 4, "SKIN_4", "1" },
		{ Dynamic && ps_r2_ls_flags.test(R2FLAG_SOFT_WATER), "USE_SOFT_WATER", "1" },
		{ !o.dx11_use_legacy_light && o.offscreen_reflecitons, "USE_OFFSCREEN_REFLECTIONS", "1" },
		{ !o.dx11_use_legacy_light && o.deffered_reflecitons, "USE_SSLR_REFLECTIONS", "1" },
		{ !!o.RainPuddles, "USE_RAIN_PUDDLES", "1" },
		{ Dynamic && ps_r2_ls_flags.test(R2FLAG_SOFT_PARTICLES), "USE_SOFT_PARTICLES", "1" },
		{ Dynamic && ps_r2_ls_flags.test(R2FLAG_DOF), "USE_DOF", "1" },
		{ Dynamic && ps_r2_ls_flags_ext.test(R4FLAG_SCREEN_SPACE_HUD_SHADOWS), "USE_HUD_SHADOWS", "1" },
		{ Dynamic && ps_r2_ls_flags_ext.test(R4FLAG_HASHED_ALPHA_TEST), "USE_HASHED_AREF", "1" },
		{ Dynamic && ps_r2_ls_flags_ext.test(R4FLAG_SSLR_ON_WATER), "USE_SSLR_ON_WATER", "1" },
		{ Dynamic && ps_r_sun_shafts > 0, "SUN_SHAFTS_QUALITY", SunShafts },
		{ Dynamic && ps_r_sun_quality > 0, "SUN_QUALITY", SunQuality },
		{ Dynamic && ps_r2_ls_flags.test(R2FLAG_STEEP_PARALLAX), "ALLOW_STEEPPARALLAX", "1" },
		{ !!o.dx11_disable_motion_vectors, "DISABLE_MOTION_VECTORS", "1" },
		{ Device.IsEditorMode(), "EDITOR_MODE", "1" },
		{ EditorInstancing, "USE_EDITOR_INSTANCING", "1" },
		{ Static, "USE_R1_STATIC_LIGHTING", "2" },
		{ Static, "USE_LEGACY_LIGHT", "1" },
	};

	ShaderExternalMap Result = EngineExternal().ShadersOptions;
	for (const auto& [Name, Value] : Defines)
	{
		Result[Name] = Value;
	}
	for (const auto& Flag : Flags)
	{
		if (Flag.Enabled)
		{
			Result[Flag.Name] = Flag.Value;
		}
	}
	return Result;
}

xr_string CRender::getShaderParams(const ShaderExternalMap& Defines) const
{
	ShaderExternalMap Key = GetShaderDefines(Defines);
	std::erase_if(Key, [](const auto& Define) { return Define.first.starts_with("SKIN_"); });
	string32 Text;
	xr_sprintf(Text, "_%08x", DefinesCRC(Key));
	return Text;
}

xr_string CRender::getShaderParamsDebug(const ShaderExternalMap& Defines) const
{
	xr_string Params;
	for (const auto& [Name, Value] : GetShaderDefines(Defines))
	{
		Params += Name + (Value.empty() ? "" : "_" + Value) + ",";
	}
	return Params;
}

bool CRender::NeedMotionVectors() const
{
	if (LightingModeIsStaticRender())
		return false;

	return (ps_r_scale_mode >= 2) || 
	       (ps_r2_aa_type == 3) || 
	       ps_r4_mblur_quality > 0 || 
	       o.deffered_reflecitons;
}

IRender_interface::GenerationLevel CRender::get_generation()
{
	return LightingModeIsStaticRender() ? IRender_interface::GENERATION_R1 : IRender_interface::GENERATION_R2;
}

bool CRender::MotionVectorsDisabled() const
{
	// The editors draw no velocity-dependent effects
	if (Device.IsEditorMode())
	{
		return true;
	}

	return !NeedMotionVectors() || EngineExternal().ShadersOptions.contains(xr_string("DISABLE_MOTION_VECTORS"));
}

void CRender::SyncMotionVectors()
{
	if (o.dx11_disable_motion_vectors == MotionVectorsDisabled())
		return;

	o.dx11_disable_motion_vectors = !o.dx11_disable_motion_vectors;
	dxRenderDeviceRender::Instance().Resources->RecompileShaders();
}

void CRender::SetShaderCacheBypass(bool bypass)
{
	m_shaderCacheBypass = bypass;
}

template <typename T>
static HRESULT create_shader(
		const char* const pTarget,
		DWORD const* buffer,
		u32	const buffer_size,
		const char* const file_name,
		T*& result,
		bool const disasm
) {
	if (RImplementation.ShaderCacheBypassed() && result->sh)
	{
		return SUCCEEDED(GRHI->ReplaceShader(result->sh, buffer, buffer_size)) ? S_OK : E_FAIL;
	}
	result->sh = ShaderTypeTraits<T>::CreateHWShader(buffer, buffer_size);
	(void)pTarget;
	(void)file_name;
	(void)disasm;
	return result->sh ? S_OK : E_FAIL;
}

static HRESULT create_shader(
		const char* const pTarget,
		DWORD const* buffer,
		u32	const buffer_size,
		const char* const file_name,
		void*& result,
		bool const disasm
) {
	HRESULT		_result = E_FAIL;
	if(pTarget[0] == 'p') {
		SPS* sps_result = (SPS*)result;
		_result = RImplementation.ShaderCacheBypassed() && sps_result->ps ?
			GRHI->ReplaceShader(sps_result->ps, buffer, buffer_size) :
			GRHI->CreateShader(buffer, buffer_size, ERHI_SHADER_TYPE::PS, &sps_result->ps);
		if(!SUCCEEDED(_result)) {
			Msg("! PS: %s", file_name);
			Msg("! CreatePixelShader hr == 0x%08x", _result);
			return		E_FAIL;
		}

		_result = S_OK;
	}
	else if(pTarget[0] == 'v') {
		SVS* svs_result = (SVS*)result;
		_result = RImplementation.ShaderCacheBypassed() && svs_result->vs ?
			GRHI->ReplaceShader(svs_result->vs, buffer, buffer_size) :
			GRHI->CreateShader(buffer, buffer_size, ERHI_SHADER_TYPE::VS, &svs_result->vs);

		if(!SUCCEEDED(_result)) {
			Msg("! VS: %s", file_name);
			Msg("! CreatePixelShader hr == 0x%08x", _result);
			return		E_FAIL;
		}

		RHIBlob* pSignatureBlob = nullptr;
		if (SUCCEEDED(GRHI->GetInputSignature(buffer, buffer_size, &pSignatureBlob)) && pSignatureBlob)
		{
			svs_result->signature = dxRenderDeviceRender::Instance().Resources->_CreateInputSignature(pSignatureBlob);
			_RELEASE(pSignatureBlob);
		}

		// The editors bind structured instance buffers and reflect VS inputs, the game does not
		if (Device.IsEditorMode())
		{
			RHIShaderReflection reflection;
			if (SUCCEEDED(GRHI->ReflectShader(buffer, buffer_size, reflection)))
			{
				for (const RHIShaderResourceDesc& Resource : reflection.Resources)
				{
					if (Resource.Type != ERHI_SHADER_RESOURCE_TYPE::STRUCTURED)
					{
						continue;
					}

					if (!xr_strcmp(Resource.Name, "detail_buffer"))
					{
						svs_result->instance_buffer_slot = Resource.BindPoint;
					}
					else if (!xr_strcmp(Resource.Name, "editor_instances"))
					{
						svs_result->editor_instance_slot = Resource.BindPoint;
					}
				}
			}

			RHIBlob* pCodeBlob = nullptr;
			if (SUCCEEDED(GRHI->CreateBlob(buffer_size, &pCodeBlob)))
			{
				CopyMemory(pCodeBlob->GetBufferPointer(), buffer, buffer_size);
				svs_result->vs_code = pCodeBlob;
			}
		}
		_result = S_OK;
	}
	else if(pTarget[0] == 'g') {
		SGS* sgs_result = (SGS*)result;
		_result = RImplementation.ShaderCacheBypassed() && sgs_result->gs ?
			GRHI->ReplaceShader(sgs_result->gs, buffer, buffer_size) :
			GRHI->CreateShader(buffer, buffer_size, ERHI_SHADER_TYPE::GS, &sgs_result->gs);
		if(!SUCCEEDED(_result)) {
			Msg("! GS: %s", file_name);
			Msg("! CreateGeometryShaderhr == 0x%08x", _result);
			return		E_FAIL;
		}

		_result = S_OK;
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

	if (disasm) 
	{
		RHIBlob* disasm_ = 0;
		GRHI->DisassembleShader(buffer, buffer_size, &disasm_);
		string_path		dname;
		xr_strconcat(dname, "disasm\\", file_name, ".", pTarget);
		IWriter* W = FS.w_open("$logs$", dname);
		W->w(disasm_->GetBufferPointer(), (u32)disasm_->GetBufferSize());
		FS.w_close(W);
		_RELEASE(disasm_);
	}

	return _result;
}


void GetShaderCRC(IReader* F, u32& final_crc, SStringVec& files)
{
	string_path pFileName = {};
	string_path pFilePath = {};

	xr_string string;

	while (!F->eof())
	{
		F->r_string(string);
		_Trim(string);

		if (string[0] == '#' && strstr(string.data(), "#include"))
		{
			if (_GetItem(string.c_str(), 1, pFileName, '"'))
			{
				if(std::find(files.begin(), files.end(), pFileName) == files.end())
				{
					xr_strconcat(pFilePath, ::Render->getShaderPath(), pFileName);

					IReader* R = FS.r_open(_game_shaders_, pFilePath);
					if (!R)
						R = FS.r_open(_game_shaders_, pFileName);
					VERIFY3(R, "include file not found", pFilePath);

					files.push_back(pFileName);

					if (!R)
					{
						continue;
					}

					GetShaderCRC(R, final_crc, files);
					FS.r_close(R);
				}
			}
		}

		final_crc = crc32(string.data(), string.size(), final_crc);
	}
}

class includer : public IRHIShaderInclude {
public:
	HRESULT Open(u32 IncludeType, const char* pFileName, LPCVOID pParentData, LPCVOID* ppData, UINT* pBytes) {
		string_path pname;
		xr_strconcat(pname, ::Render->getShaderPath(), pFileName);
		IReader* R = FS.r_open(_game_shaders_, pname);
		if(0 == R) {
			// possibly in shared directory or somewhere else - open directly
			R = FS.r_open(_game_shaders_, pFileName);
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
		return	S_OK;
	}

	HRESULT Close(LPCVOID pData) {
		xr_free(pData);
		return S_OK;
	}
};

HRESULT	CRender::shader_compile(
	const char* name,
	DWORD const* pSrcData,
	UINT SrcDataLen,
	const char* pFunctionName,
	const char* pTarget,
	DWORD Flags,
	void*& result,
	const ShaderExternalMap& Defines)
{
	const ShaderExternalMap AllDefines = GetShaderDefines(Defines);
	RHIShaderMacro defines[128];
	u32 def_it = 0;
	for (const auto& [Name, Value] : AllDefines)
	{
		VERIFY(def_it < std::size(defines) - 1);
		defines[def_it++] = { Name.c_str(), Value.c_str() };
	}
	defines[def_it] = { nullptr, nullptr };

	char ShaderTarget[] = "?s_5_0";
	ShaderTarget[0] = pTarget[0];
	pTarget = ShaderTarget;

	HRESULT _result = E_FAIL;

	char extension[3];
	strncpy_s(extension, pTarget, 2);

	string_path file_name;
	{
		string_path file;
		xr_strcpy(file, "shaders_cache\\");
		xr_strcat(file, _VER);
		xr_strcat(file, GRHI->UsesDXIL(pTarget[0]) ? "\\dxil" : "\\d3d11");
		xr_strcat(file, Core.ParamsData.test(ECoreParams::debug_shaders) ? "_debug\\" : "\\");
		xr_strcat(file, name);
		xr_strcat(file, ".");
		xr_strcat(file, extension);
		FS.update_path(file_name, "$app_data_root$", file);
	}

	u32 RealCodeCRC = u32(-1);
	SStringVec crc_file_list;

	IReader R = IReader((char*)pSrcData, SrcDataLen);
	GetShaderCRC(&R, RealCodeCRC, crc_file_list);

	const u32 options_crc = DefinesCRC(AllDefines);

	Flags |= RHI_SHADER_OPTIMIZATION_LEVEL3;
	if (Core.ParamsData.test(ECoreParams::debug_shaders))
		Flags |= RHI_SHADER_DEBUG | RHI_SHADER_DEBUG_NAME_FOR_SOURCE;

	xr_resource_uniq* inShader = (xr_resource_uniq*)result;

	if(!m_shaderCacheBypass && FS.exist(file_name) && ps_r__common_flags.test(RFLAG_USE_CACHE)) 
	{
#ifdef DEBUG
		Msg("compilied shader library found %s", file_name);
#endif // DEBUG
		IReader* file = FS.r_open(file_name);

		if(file->length() > 12) 
		{
			u32 ShaderCRC = file->r_u32();
			u32 CodeSRC = file->r_u32();
			u32 cached_options = file->r_u32();

			if(RealCodeCRC == CodeSRC && cached_options == options_crc)
			{
				u32 const real_crc = crc32(file->pointer(), file->elapsed());

				if(real_crc == ShaderCRC) 
				{
					inShader->m_crc1 = ShaderCRC;
					inShader->m_crc2 = CodeSRC;

					_result = create_shader(pTarget, (DWORD*)file->pointer(), file->elapsed(), file_name, result, o.disasm);
				}
			}
		}

		file->close();
	}

	if(FAILED(_result)) 
	{
		RHIBlob* pShaderBuf = nullptr;
		RHIBlob* pErrorBuf = nullptr;

		includer Includer;
		_result = GRHI->CompileShader(
			pSrcData,
			SrcDataLen,
			name,
			defines, &Includer, pFunctionName,
			pTarget,
			Flags,
			&pShaderBuf,
			&pErrorBuf
		);

		if(SUCCEEDED(_result))
		{
			IWriter* file = FS.w_open(file_name);
			u32 const crc = crc32(pShaderBuf->GetBufferPointer(), pShaderBuf->GetBufferSize());

			file->w_u32(crc);
			file->w_u32(RealCodeCRC);
			file->w_u32(options_crc);
			file->w(pShaderBuf->GetBufferPointer(), (u32)pShaderBuf->GetBufferSize());

			FS.w_close(file);

			inShader->m_crc1 = crc;
			inShader->m_crc2 = RealCodeCRC;

			_result = create_shader(pTarget, (DWORD*)pShaderBuf->GetBufferPointer(), (u32)pShaderBuf->GetBufferSize(), file_name, result, o.disasm);
		}

		if (pErrorBuf)
		{
			Msg("! error: %s\n%s", file_name, (LPCSTR)pErrorBuf->GetBufferPointer());
		}

		if (FAILED(_result) && !pErrorBuf)
		{
			Msg("Can't compile shader [%s] hr=0x%08x", file_name, _result);
		}
		_RELEASE(pShaderBuf);
		_RELEASE(pErrorBuf);
	}

	return _result;
}



void CRender::renderImGuiDebugWindow_SVGStorage()
{
	if (ImGui::Begin("Render Debug - SVG Storage"), &Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_RenderDebug_SVGStorageViewer)])
	{
		if (DEV)
		{
			CSVGStorage* pStorage = DEV->GetSVGStorage();

			if (pStorage)
			{
				if (ImGui::CollapsingHeader("Runtime"))
				{
					CTextureAtlas* pDefault = pStorage->get_atlas(_kSVGStorage_DefaultAtlasID);

					auto p_atlas_draw = [](const CTextureAtlas* pAtlas) -> void
					{
						static bool _ViewerState_EnableDeleting = false;
						static xr_stack_string<256> _ViewerState_QueryResult;
						static float _ViewerState_QueryWidth = 0.0f;
						static float _ViewerState_QueryHeight = 0.0f;

						char name[32];
						xr_sprintf(name, sizeof(name), "[%d] %s", pAtlas->getID(), _kSVGStorage_DefaultAtlasName);

						if (ImGui::CollapsingHeader(name))
						{
							const auto& elements = pAtlas->getElements();

							ImGui::SeparatorText("Atlas Info");

							ImGui::Text("width: %.2f", float(pAtlas->getWidth()));
							ImGui::Text("height: %.2f", float(pAtlas->getHeight()));

							ImGui::Checkbox("Deleting", &_ViewerState_EnableDeleting);

							ImGui::DragFloat("w", &_ViewerState_QueryWidth);
							ImGui::DragFloat("h", &_ViewerState_QueryHeight);

							if (ImGui::Button("find nearest"))
							{
								const auto* pElement = pAtlas->findNearest(_ViewerState_QueryWidth, _ViewerState_QueryHeight);

								if (pElement)
								{
									xr_sprintf(_ViewerState_QueryResult.data(), _ViewerState_QueryResult.max_size(), "w: %.2f h: %.2f\nx: %.2f y: %.2f\nu0: %.2f v0: %.2f u1: %.2f v1: %.2f", pElement->w(), pElement->h(), pElement->x(), pElement->y(), pElement->u0(pAtlas->getWidth()), pElement->v0(pAtlas->getHeight()), pElement->u1(pAtlas->getWidth()), pElement->v1(pAtlas->getHeight()));
								}
								else
								{
									_ViewerState_QueryResult.clear();
									xr_sprintf(_ViewerState_QueryResult.data(), _ViewerState_QueryResult.max_size(), "failed to obtain element!");
								}
							}

							if (_ViewerState_QueryResult.empty() == false)
							{
								ImGui::SameLine();
								if (ImGui::Button("Reset"))
								{
									_ViewerState_QueryResult.clear();
								}

								ImGui::Text("Nearest Query:");
								ImGui::Text("%s", _ViewerState_QueryResult.c_str());
							}


							ImGui::SeparatorText("Elements");
							ImGui::Text("amount: %zu", elements.size());

							ImGui::SeparatorText("Atlas");

							float atlasPixelW = pAtlas->getWidth();
							float atlasPixelH = pAtlas->getHeight();

							ImVec2 atlasDisplaySize = ImVec2((float)atlasPixelW, (float)atlasPixelH);

							ImGui::Image(pAtlas->getResource(), atlasDisplaySize, ImVec2(0, 0), ImVec2(1, 1), ImVec4(1, 1, 1, 1), ImVec4(1, 1, 1, 1));

							ImVec2 atlasMin = ImGui::GetItemRectMin();
							ImVec2 atlasMax = ImGui::GetItemRectMax();
							ImVec2 atlasOnScreenSize = ImVec2(atlasMax.x - atlasMin.x, atlasMax.y - atlasMin.y);

							float scaleX = atlasOnScreenSize.x / (float)atlasPixelW;
							float scaleY = atlasOnScreenSize.y / (float)atlasPixelH;

							ImVec2 parentCursorBackup = ImGui::GetCursorPos();

							int hoveredIndex = -1;
							ImVec2 hoveredSubMin, hoveredSubSize;

							ImVec2 mousePos = ImGui::GetMousePos();

							bool break_called = false;
							u32 hovered_icon_w;
							u32 hovered_icon_h;
							float hovered_icon_x;
							float hovered_icon_y;

							float hovered_icon_u0;
							float hovered_icon_v0;
							float hovered_icon_u1;
							float hovered_icon_v1;

							int i = 0;
							for (const auto& element : elements)
							{
								ImVec2 subMin = ImVec2(
									atlasMin.x + 1 + element.x() * scaleX,
									atlasMin.y + 1 + element.y() * scaleY
								);

								ImVec2 subSize = ImVec2(
									element.w() * scaleX,
									element.h() * scaleY
								);
								ImVec2 subMax = ImVec2(subMin.x + subSize.x, subMin.y + subSize.y);


								if (mousePos.x >= subMin.x && mousePos.x <= subMax.x &&
									mousePos.y >= subMin.y && mousePos.y <= subMax.y)
								{
									hoveredIndex = i;

									hovered_icon_w = element.w();
									hovered_icon_h = element.h();
									hovered_icon_x = element.x();
									hovered_icon_y = element.y();

									hovered_icon_u0 = element.u0(pAtlas->getWidth());
									hovered_icon_v0 = element.v0(pAtlas->getHeight());

									hovered_icon_u1 = element.u1(pAtlas->getWidth());
									hovered_icon_v1 = element.v1(pAtlas->getHeight());

									hoveredSubMin = subMin;
									hoveredSubSize = subSize;
									break_called = true;
									break; // stop after first hit (assuming subregions donâ€™t overlap)
								}

								++i;
							}

							i = 0;

							bool hovered_icon_clicked = false;
							for (const auto& element : elements)
							{
								ImVec2 subMin = ImVec2(
									atlasMin.x + 1 + element.x() * scaleX,
									atlasMin.y + 1 + element.y() * scaleY
								);
								ImVec2 subSize = ImVec2(
									element.w() * scaleX,
									element.h() * scaleY
								);


								ImU32 borderColor = (i == hoveredIndex)
														? IM_COL32(255, 255, 0, 255) // yellow
														: IM_COL32(255, 0, 0, 255);	 // red


								ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
								ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0, 0, 0, 0));

								ImGuiWindowFlags childFlags =
									ImGuiWindowFlags_NoTitleBar |
									ImGuiWindowFlags_NoResize |
									ImGuiWindowFlags_NoMove |
									ImGuiWindowFlags_NoScrollbar |
									ImGuiWindowFlags_NoScrollWithMouse |
									ImGuiWindowFlags_NoSavedSettings;

								ImGui::SetCursorScreenPos(subMin);
								char subRegionChildId[48];
								xr_sprintf(subRegionChildId, sizeof(subRegionChildId), "SubRegion##%d", i);
								ImGui::BeginChild(
									subRegionChildId,
									subSize,
									/*border=*/false,
									childFlags
								);


								ImDrawList* dl = ImGui::GetWindowDrawList();
								ImVec2 rectMin = ImVec2(subMin.x - 0.5f, subMin.y);
								ImVec2 rectMax = ImVec2(subMin.x + subSize.x + 0.5f, subMin.y + subSize.y);

								dl->AddRect(rectMin, rectMax, borderColor, 0.0f, 0, 2.0f);


								ImGui::Dummy(subSize);

								if (i == hoveredIndex)
								{
									hovered_icon_clicked = ImGui::IsItemClicked();
								}

								ImGui::EndChild();
								ImGui::PopStyleColor();
								ImGui::PopStyleVar();


								ImGui::SetCursorPos(parentCursorBackup);

								++i;
							}


							if (hoveredIndex >= 0 && hovered_icon_w && hovered_icon_h)
							{
								bool clicked = ImGui::IsItemClicked();

								if (_ViewerState_EnableDeleting)
								{
									if (hovered_icon_clicked)
									{
										// yeah slow (prob dumb), but it is for debug purposes, so there's no need to point out on that thing, seriously :/
										// upd: we don't need to make removeElement as const since it is obvious write operation and must be accessible only when we have non const pointer (like we don't read, but this viewer is for reading mainly)
										const_cast<CTextureAtlas*>(pAtlas)->removeElement(hovered_icon_w, hovered_icon_h);
									}
								}

								ImGui::BeginTooltip();
								ImGui::Text("Lookup id: %d", hoveredIndex);
								ImGui::SeparatorText("Dimensions");
								ImGui::Text("w=%.2f h=%.2f", float(hovered_icon_w), float(hovered_icon_h), hovered_icon_x, hovered_icon_y);
								ImGui::SeparatorText("Offset");
								ImGui::Text("x=%.2f y=%.2f", hovered_icon_x, hovered_icon_y);
								ImGui::SeparatorText("UV");
								ImGui::Text("u0=%.2f v0=%.2f\nu1=%.2f v1=%.2f", hovered_icon_u0, hovered_icon_v0, hovered_icon_u1, hovered_icon_v1);


								ImGui::EndTooltip();
							}
						}
					};

					p_atlas_draw(pDefault);
					const auto& atlases = pStorage->get_atlases();

					for (const auto& atlas : atlases)
					{
						p_atlas_draw(&atlas);
					}
				}

				if (ImGui::CollapsingHeader("Cache"))
				{
#ifdef DEBUG
					xr_vector<CSVGStorage::SvgDebugCacheTableRow> rows;
					pStorage->DebugCollectSvgCacheRows(rows);
					if (ImGui::Button("Invalidate document LRU"))
					{
						pStorage->InvalidateAllSvgDocuments();
					}
					ImGui::Text(
						"doc LRU hits: %llu misses: %llu",
						static_cast<unsigned long long>(pStorage->DebugGetSvgDocCacheHits()),
						static_cast<unsigned long long>(pStorage->DebugGetSvgDocCacheMisses())
					);
					ImGui::Text("new atlas allocations: %llu", static_cast<unsigned long long>(pStorage->DebugGetSvgNewAtlasAllocCount()));
					if (pStorage->DebugGetSvgRenderToBitmapSamples() > 0)
					{
						const double avgNs = static_cast<double>(pStorage->DebugGetSvgRenderToBitmapNsAccum()) /
											 static_cast<double>(pStorage->DebugGetSvgRenderToBitmapSamples());
						ImGui::Text("renderToBitmap avg ns: %.0f", avgNs);
					}
					if (ImGui::Button("Reset SVG metrics"))
					{
						pStorage->DebugResetSvgMetrics();
					}
					if (ImGui::BeginTable("svg_cache", 4, ImGuiTableFlags_Borders))
					{
						ImGui::TableSetupColumn("key");
						ImGui::TableSetupColumn("variants");
						ImGui::TableSetupColumn("pixels");
						ImGui::TableSetupColumn("last access");
						ImGui::TableHeadersRow();
						for (const auto& r : rows)
						{
							ImGui::TableNextRow();
							ImGui::TableNextColumn();
							ImGui::TextUnformatted(r.tableKey.c_str());
							ImGui::TableNextColumn();
							ImGui::Text("%u", r.variantCount);
							ImGui::TableNextColumn();
							ImGui::Text("%u", r.totalRasterPixels);
							ImGui::TableNextColumn();
							ImGui::Text("%llu", static_cast<unsigned long long>(r.lastAccessSeq));
						}
						ImGui::EndTable();
					}
#endif
				}
			}
		}
	}

	ImGui::End();
}

CDB::MODEL* CRender::GetHOMModel()
{
	return RImplementation.HOM.GetHOMModel();
}

xr_vector<u32>* CRender::GetHOMInvaltids()
{
	return RImplementation.HOM.get_invaltids();
}

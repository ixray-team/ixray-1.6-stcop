#include "stdafx.h"
#include "RenderThread.h"
#include "ixr.h"
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

#include "ShaderResourceTraits.h"

#include "RenderInterfaceShared.h"
#include "UpscaleNri.h"
#include "../../xrEngine/EngineAPI.h"
#include "SVGStorage.h"

CRender RImplementation;

//////////////////////////////////////////////////////////////////////////
class CDynamicGlow : public IRender_Glow
{
public:
	bool				bActive;
public:
	CDynamicGlow() : bActive(false)		{ }
	virtual void					set_active			(bool b) override { bActive=b;		}
	virtual bool					get_active			() override { return bActive;	}
	virtual void					set_position		(const Fvector& P) override { }
	virtual void					set_direction		(const Fvector& D) override { }
	virtual void					set_radius			(float R) override { }
	virtual void					set_texture			(const char* name) override { }
	virtual void					set_color			(const Fcolor& C) override { }
	virtual void					set_color			(float r, float g, float b) override { }
};

float r_dtex_range = 50.f;

ICF float ViewDetailTextureRange(const R_dsgraph_structure& Graph)
{
	return Graph.OwnsView ? Graph.ViewParams.DetailTextureRange : r_dtex_range;
}

ShaderElement* CRender::rimp_select_sh_dynamic(const R_dsgraph_structure& Graph, dxRender_Visual* pVisual, float cdist_sq, bool is_hud)
{
	const u32 phase = Graph.ViewPhase();
	if (phase == PHASE_NORMAL)
	{
		// The HUD keeps a blended surface sorted where the world weighs it order-independently
		ShaderElement* World = pVisual->shader->E[ShaderVariant::NormalHQ]._get();
		ShaderElement* Hud = pVisual->shader->E[ShaderVariant::HUD]._get();
		return is_hud && World && World->flags.OrderIndependent && Hud && !Hud->passes.empty() ? Hud : World;
	}
	if (phase == PHASE_SMAP)
		return pVisual->shader->E[ShaderVariant::Shadow]._get();
	return rimp_select_sh_static(Graph, pVisual, cdist_sq);
}

ShaderElement* CRender::rimp_select_sh_static(const R_dsgraph_structure& Graph, dxRender_Visual* pVisual, float cdist_sq)
{
	const u32 phase = Graph.ViewPhase();
	if (phase == PHASE_SMAP)
		return pVisual->shader->E[ShaderVariant::Shadow]._get();
	if (phase == PHASE_REFLECT)
		return pVisual->shader->E[ShaderVariant::Reflection]._get();
	int id = ShaderVariant::NormalLQ;
	if ((_sqrt(cdist_sq) - pVisual->vis.sphere.R) < ViewDetailTextureRange(Graph)) id = ShaderVariant::NormalHQ;
	return pVisual->shader->E[id]._get();
}

// What the renderer does and does not do under its features
void CRender::ApplyPresetOptions()
{
	ApplyVideoSettings();

	// options
	o.sunstatic = false;
	o.noshadows = Core.ParamsData.test(ECoreParams::noshadows);
	o.distortion_enabled = !Core.ParamsData.test(ECoreParams::nodistort);
	o.distortion = o.distortion_enabled;
	o.disasm = Core.ParamsData.test(ECoreParams::disasm);

	// The frame keeps the cube of what is around the eye for the rays its screen misses
	o.offscreen_reflecitons = !!ps_r2_ls_flags_ext.test(R4FLAG_OFFSCREEN_REFLECTIONS);

	// Puddles in the terrain shader, with the rest of the wetness feature off
	o.RainPuddles = !!ps_r2_ls_flags_ext.test(R4FLAG_RAIN_PUDDLES);
	UpdateRainPuddleMode();
	o.GtaoHq = ps_r2_ls_flags_ext.test(R4FLAG_GTAO_HQ);
	o.SslrHq = ps_r2_ls_flags_ext.test(R4FLAG_SSLR_HQ);

	o.dx11_disable_motion_vectors = MotionVectorsDisabled();
	o.DofCompiled = g_RenderFeatures.DepthOfField && (ps_r2_ls_flags.test(R2FLAG_DOF) || (g_pGamePersistent && (g_pGamePersistent->IsDofForced() || g_pGamePersistent->IsDofCameraForced())));
}

//////////////////////////////////////////////////////////////////////////
// Just two static storage
void CRender::create()
{
	Device.seqFrame.Add	(this,REG_PRIORITY_HIGH+0x12345678);
	Device.seqAppStart.Add(&dxRenderDeviceRender::Instance().Resources->Manifest, REG_PRIORITY_HIGH);
	Device.BeginGraphCollect = xr_make_delegate(this, &CRender::BeginGraphCollect);
	Device.CollectGraphs = xr_make_delegate(this, &CRender::CollectGraphs);
	Device.PaceFrame = xr_make_delegate(&Pacer, &FramePacer::Wait);
	Dynamic.Create();

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

	RegisterVideoSettingsSupport(true);
	ApplyPresetOptions();
	HasUpscaler = g_RenderFeatures.Upscaling;


	m_bMakeAsyncSS = false;


	if (o.ProceduralRainPuddles)
	{
		RainPuddleWater.Build();
	}
	Target = new CRenderTarget();	// Main target

	// The editor device owns the model pool and the particle library (see ixr_editor.cpp);
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
	rmNormal();


	ForEachViewGraph([](R_dsgraph_structure& Graph)
	{
		Graph.PortalTraverser.initialize();
	});

	xrRender_apply_tf();
	GraphMain.PortalTraverser.initialize();

	Device.ModelDefferClear = xr_make_delegate(this, &CRender::ClearDeferredModels);
}

void CRender::destroy()
{
	// The recorded lists hold the shaders and geometries they name, and the queues what the game let go: held past
	// this point they are let go when the library unloads, after the device
	RenderThread::WaitIdle();
	FrameDraws.Clear();
	RecordedScenes.clear();

	if (Models)
	{
		FreeQueued();
	}

	Device.seqAppStart.Remove(&dxRenderDeviceRender::Instance().Resources->Manifest);
	dxRenderDeviceRender::Instance().Resources->Manifest.Save();
	GRHI->DevicePtr->SaveLearnedPipelines();
	dxRenderDeviceRender::Instance().Resources->ReleaseKeptStages();
	NriPasses.Destroy();
	Dynamic.Destroy();
	Device.PaceFrame = nullptr;
	Pacer.Destroy();
	ResetSunShadowCache();
	// Release the editor's lazily created brush resources while the device is still
	// alive - their destructors dereference DEV, which is null by DLL detach time.
	DetailLayers_EditorDestroy();
	RainPuddleWater.Destroy();
	HeightfieldShadows.Destroy();
	// Held past this point it is let go when the library unloads, after the device
	SurfaceProjectorDefault = nullptr;
	PuddleGeom = nullptr;

	m_bMakeAsyncSS = false;
	_RELEASE(SurfaceLightView);
	_RELEASE(SurfaceLightBuffer);
	_RELEASE(ForwardLightView);
	_RELEASE(ForwardLightBuffer);
	_RELEASE(ForwardDecalView);
	_RELEASE(ForwardDecalBuffer);

	for (u32 Layer = 0; Layer < DecalLayers; ++Layer)
	{
		_RELEASE(DecalLayerTarget[Layer]);
		DecalLayerTexture[Layer] = nullptr;
	}

	_RELEASE(DecalArrayView);
	_RELEASE(DecalArray);
	GraphMain.PortalTraverser.destroy();

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
	GraphMainStatic.r_dsgraph_destroy();

	ForEachViewGraph([](R_dsgraph_structure& Graph)
	{
		Graph.r_dsgraph_destroy();
		Graph.PortalTraverser.destroy();
	});

	Device.ModelDefferClear = nullptr;
	Device.BeginGraphCollect = nullptr;
	Device.CollectGraphs = nullptr;

	if (HasUpscaler)
		UpscaleNri::Destroy();

	RegisterVideoSettingsSupport(false);
}

void CRender::BeginGraphCollect()
{
	if (RenderThread::IsPipelined() && !IsGraphCollectHere)
	{
		return;
	}

	// The collected graphs hold shader pointers: recompile only before collecting.
	if (!Device.IsEditorMode())
	{
		SyncMotionVectors();
		SyncDof();
		SyncShadingQuality();
	}
	GraphJobs.clear();
	GraphJobsDone.store(0, std::memory_order_relaxed);

	begin_reflection_collect();
	ReflectionJobsEnd = (u32)GraphJobs.size();
}

void CRender::CollectGraphs()
{
	if (RenderThread::IsPipelined() && !(IsGraphCollectHere && g_AppInfo.IsPrimaryThread()))
	{
		return;
	}

	for (u32 Index = 0; Index < (u32)GraphJobs.size(); ++Index)
	{
		GraphCollectJob& Job = GraphJobs[Index];

		if (Job.IsPortalsOnly)
		{
			Job.Graph->Begin(Job.View);
			Job.Graph->Traverse();
		}
		else
		{
			Job.Graph->Build(Job.View);
		}

		GraphJobsDone.store(Index + 1, std::memory_order_release);
		GraphJobsDone.notify_all();
	}
}

void CRender::WaitGraphJobs(u32 Count)
{
	for (u32 Done = GraphJobsDone.load(std::memory_order_acquire); Done < Count; Done = GraphJobsDone.load(std::memory_order_acquire))
	{
		GraphJobsDone.wait(Done, std::memory_order_acquire);
	}
}

// The editors have no sectors: their scene is submitted into the pass's graph
R_dsgraph_structure& CRender::BuildPassGraph(GfxCmdList& Cmd, const GraphView& View)
{
	GraphPass.Begin(View);
	EditorRenderSubspace(Cmd, GraphPass);
	return GraphPass;
}

void CRender::reset_begin() {
	RenderThread::WaitIdle();
	// The pre-render stage runs in parallel with FrameMove, where a reset can start
	Device.PreRenderThread.Wait();
	// Its pipelines and wrappers refer to targets that are about to be recreated
	NriPasses.Destroy();
	UpscaleNri::ReleaseFrames();
	ResetSunShadowCache();
	// Its compute pass reads the scene depth target, which is recreated
	if (Device.IsEditorMode())
	{
		EditorHiZ.Destroy();
	}

	// The grass outlives a reset: the scene's tables name its models' geometry and shaders, and nothing of it is
	// made of the targets
	xr_delete(Target);

	// A restart takes the settings it applies at once: the menu sets them and restarts in the same frame
	ApplyVideoSettings();
	o.offscreen_reflecitons = !!ps_r2_ls_flags_ext.test(R4FLAG_OFFSCREEN_REFLECTIONS);
	o.RainPuddles = !!ps_r2_ls_flags_ext.test(R4FLAG_RAIN_PUDDLES);
	UpdateRainPuddleMode();
	o.GtaoHq = ps_r2_ls_flags_ext.test(R4FLAG_GTAO_HQ);
	o.SslrHq = ps_r2_ls_flags_ext.test(R4FLAG_SSLR_HQ);
}

void CRender::reset_end()
{
	//	Prevent error on first get data
	if (o.ProceduralRainPuddles)
	{
		RainPuddleWater.Build(GraphMain.b_loaded && g_pGameLevel ? &g_pGameLevel->ObjectSpace : nullptr);
	}
	else
	{
		RainPuddleWater.Destroy();
	}

	Target = new CRenderTarget();

	if (L_Glows)
	{
		L_Glows->reset_geometry();
	}

	xrRender_apply_tf();

	// Set this flag true to skip the first render frame,
	// that some data is not ready in the first frame (for example device camera position)
	m_bFirstFrameAfterReset = true;
}

// Models, lights and light trackers the game let go: a frame that is drawn may still hold them
void CRender::FreeQueued()
{
	Models->DeleteQueue();

	for (light* L : v_all_lights_dque)
		xr_delete(L);
	v_all_lights_dque.clear();

	for (IRender_ObjectSpecific*& Track : TrackersToFree)
		xr_delete(Track);
	TrackersToFree.clear();
	TexturesToFree.clear();
}

void CRender::OnFrame()
{
	// With the frames one behind the game the last one is still drawn now: this waits for the frames to meet
	if (!RenderThread::IsPipelined())
	{
		FrameSync();
	}
}

void CRender::FrameSync()
{
	FreeQueued();

	if (RenderThread::IsPipelined())
	{
		Models->DeleteQueuedDeffer();
	}

	// What the last frames learned is written now and then: a list is only written when it has grown
	if ((Device.dwFrame & 511) == 511)
	{
		dxRenderDeviceRender::Instance().Resources->Manifest.Save();
		GRHI->DevicePtr->SaveLearnedPipelines();
	}
}

void CRender::ClearDeferredModels()
{
	if (!RenderThread::IsPipelined())
	{
		Models->DeleteQueuedDeffer();
	}
}

// The cube's faces are chosen and their graphs built when the frames meet: the frame in flight reads the last ones
void CRender::CollectGraphsNow()
{
	IsGraphCollectHere = true;
	BeginGraphCollect();
	CollectGraphs();
	IsGraphCollectHere = false;
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
	if (p && RenderThread::IsPipelined())
	{
		TrackersToFree.push_back(p);
		p = nullptr;
		return;
	}

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
	return g_RenderFeatures.SpriteGlows ? static_cast<IRender_Glow*>(new CGlow()) : new CDynamicGlow();
}

void CRender::set_Object(IRenderable* O, R_dsgraph_structure* Graph)
{
	(Graph ? *Graph : GraphMain).val_pObject = O;

	// Isolated graphs (sun / reflections) run on PreRenderThread while GraphMain
	// is built on the render thread. Projector and R1 shadows belong to GraphMain.
	if (Graph && Graph != &GraphMain)
		return;

}

CRender::SurfaceParams CRender::getSurface(const char* nameTexture)
{
	CTexture* texture = DEV->_GetPinnedTexture(nameTexture);
	texture->Load();
	SurfaceParams surface = {};
	surface.Surface = RHIUtils::ImGui::Image(texture->pSurface);
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
	ForEachViewGraph([this](R_dsgraph_structure& Graph)
	{
		Graph.Backend = &Backend;
	});
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
#ifdef DEBUG
	HOM.stats();
#endif
}

static thread_local bool g_EditorInstancing = false;

void CRender::SetEditorInstancing(bool Value)
{
	g_EditorInstancing = Value;
}

bool CRender::IsEditorInstancing() const
{
	return g_EditorInstancing;
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

xr_string CRender::getShaderParams(const ShaderExternalMap& Defines) const
{
	string32 Text;
	xr_sprintf(Text, "_%08x", DefinesCRC(GetShaderDefines(Defines)));
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

// The cube of what is around the eye is drawn and read while the frame traces reflections
bool CRender::HasReflectionCube() const
{
	// The cube serves the glass and, without the screen trace, every mirror: not the trace, only the frame of high shading
	// with its occlusion, whose depth pass splits the surfaces it is drawn with
	return o.offscreen_reflecitons && ForwardOcclusionNormals();
}

bool CRender::NeedMotionVectors() const
{
	if (!g_RenderFeatures.MotionVectors)
		return false;

	return (g_RenderFeatures.Upscaling && ps_r_scale_mode >= 2) || 
	       (ps_r2_aa_type == 3) || 
	       ps_r4_mblur_quality > 0;
}

IRender_interface::GenerationLevel CRender::get_generation()
{
	// The game gives the frame the lamps, the actor's shadow and the shadows of a shot of a dynamic renderer
	return IRender_interface::GENERATION_R2;
}

bool CRender::MotionVectorsDisabled() const
{
	// The frame has no G-buffer to write velocity to
	return true;
}

void CRender::SyncMotionVectors()
{
	if (o.dx11_disable_motion_vectors == MotionVectorsDisabled())
		return;

	o.dx11_disable_motion_vectors = !o.dx11_disable_motion_vectors;
	dxRenderDeviceRender::Instance().Resources->RecompileShaders();
}

void CRender::SyncDof()
{
	o.DofCompiled = g_RenderFeatures.DepthOfField && (ps_r2_ls_flags.test(R2FLAG_DOF) || (g_pGamePersistent && (g_pGamePersistent->IsDofForced() || g_pGamePersistent->IsDofCameraForced())));
}

void CRender::SetShaderCacheBypass(bool bypass)
{
	m_shaderCacheBypass = bypass;
}

template <typename T>
static HRESULT create_shader(
		const char* const pTarget,
		u32 const* buffer,
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

struct ShaderSlots
{
	u64 Reads = 0;
	u64 Flat = 0;
	u64 Buffers = 0;
	u32 Writes = 0;
	u32 Storage = 0;
	ESlotKind Kinds[64] = {};
};

static ShaderSlots ReflectSlots(u32 const* buffer, u32 buffer_size)
{
	using EType = ERHI_SHADER_RESOURCE_TYPE;
	using enum ESlotKind;
	// By ERHI_SRV_DIMENSION
	static constexpr ESlotKind TextureKinds[] = { Other, Other, Other, Other, Flat, Array, Other, Other, Volume, Cube, CubeArray, Other };
	ShaderSlots Slots;
	RHIShaderReflection reflection;

	if (FAILED(GRHI->ReflectShader(buffer, buffer_size, reflection)))
	{
		return Slots;
	}

	for (const RHIShaderResourceDesc& Resource : reflection.Resources)
	{
		const EType Type = Resource.Type;
		const bool IsBuffer = Type == EType::STRUCTURED || Type == EType::BYTEADDRESS;
		const bool IsRead = IsBuffer || Type == EType::TEXTURE || Type == EType::TBUFFER;
		const bool IsWrite = Type == EType::UAV_RWTYPED || Type == EType::UAV_RWSTRUCTURED || Type == EType::UAV_RWBYTEADDRESS;
		const bool IsFlat = Type == EType::TEXTURE && Resource.Dimension == ERHI_SRV_DIMENSION::TEXTURE2D;
		const bool IsStorage = Type == EType::UAV_RWTYPED && Resource.Dimension == ERHI_SRV_DIMENSION::TEXTURE2D;
		const u32 Dimension = u32(Resource.Dimension);
		const ESlotKind Kind = Type == EType::STRUCTURED ? Structured : Type == EType::TEXTURE && Dimension < std::size(TextureKinds) ? TextureKinds[Dimension] : Other;

		for (u32 Slot = Resource.BindPoint; Slot < Resource.BindPoint + Resource.BindCount && Slot < 64; ++Slot)
		{
			const u64 Bit = 1ull << Slot;
			Slots.Kinds[Slot] = IsRead ? Kind : Slots.Kinds[Slot];
			Slots.Reads |= IsRead ? Bit : 0;
			Slots.Flat |= IsFlat ? Bit : 0;
			Slots.Buffers |= IsBuffer ? Bit : 0;
			Slots.Writes |= IsWrite ? u32(Bit) : 0;
			Slots.Storage |= IsStorage ? u32(Bit) : 0;
		}
	}

	return Slots;
}

static HRESULT create_shader(
		const char* const pTarget,
		u32 const* buffer,
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

		const ShaderSlots Slots = ReflectSlots(buffer, buffer_size);
		sps_result->Texture2DSlots = Slots.Flat;
		sps_result->ReadSlots = Slots.Reads;
		std::copy(std::begin(Slots.Kinds), std::end(Slots.Kinds), sps_result->Kinds);
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

		_RELEASE(svs_result->Signature);
		GRHI->GetInputSignature(buffer, buffer_size, &svs_result->Signature);

		const ShaderSlots Slots = ReflectSlots(buffer, buffer_size);
		svs_result->ReadSlots = Slots.Reads;
		svs_result->BufferSlots = Slots.Buffers;
		std::copy(std::begin(Slots.Kinds), std::end(Slots.Kinds), svs_result->Kinds);

		RHIShaderReflection reflection;
		const bool Reflected = SUCCEEDED(GRHI->ReflectShader(buffer, buffer_size, reflection));
		svs_result->Input = EVertexInput::None;
		for (const RHIShaderInputDesc& Input : reflection.Inputs)
		{
			if (!Input.SystemValueType)
			{
				svs_result->Input = !xr_strcmp(Input.SemanticName, "DRAWID") ? EVertexInput::Pulled : EVertexInput::Legacy;
				break;
			}
		}

		// Rain binds its instance buffer in the game. Only the editors bind the other structured instance buffers
		if (Reflected)
		{
			for (const RHIShaderResourceDesc& Resource : reflection.Resources)
			{
				if (Resource.Type != ERHI_SHADER_RESOURCE_TYPE::STRUCTURED)
				{
					continue;
				}

				if (!xr_strcmp(Resource.Name, "RainInstances"))
				{
					svs_result->RainInstanceSlot = Resource.BindPoint;
				}
				else if (Device.IsEditorMode() && !xr_strcmp(Resource.Name, "detail_buffer"))
				{
					svs_result->instance_buffer_slot = Resource.BindPoint;
				}
				else if (Device.IsEditorMode() && !xr_strcmp(Resource.Name, "editor_instances"))
				{
					svs_result->editor_instance_slot = Resource.BindPoint;
				}
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
		SCS* scs_result = (SCS*)result;
		_result = create_shader(pTarget, buffer, buffer_size, file_name, scs_result, disasm);
		const ShaderSlots Slots = ReflectSlots(buffer, buffer_size);
		scs_result->Texture2DSlots = Slots.Flat;
		scs_result->ReadSlots = Slots.Reads;
		scs_result->BufferSlots = Slots.Buffers;
		scs_result->WriteSlots = Slots.Writes;
		scs_result->StorageSlots = Slots.Storage;
		std::copy(std::begin(Slots.Kinds), std::end(Slots.Kinds), scs_result->Kinds);
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


namespace ShaderSource
{
static xr_string_view ReadShaderCRCLine(IReader& Reader)
{
	const char* Data = static_cast<const char*>(Reader.pointer());
	const size_t Available = size_t(Reader.elapsed());
	size_t Length = 1;
	while (Length < Available && Data[Length] != '\r' && Data[Length] != '\n')
	{
		++Length;
	}
	size_t Consumed = Length;
	while (Consumed < Available && (Data[Consumed] == '\r' || Data[Consumed] == '\n'))
	{
		++Consumed;
	}
	Reader.advance(intptr_t(Consumed));
	xr_string_view Line(Data, Length);
	while (!Line.empty() && Line.front() != 0 && u8(Line.front()) <= u8(' '))
	{
		Line.remove_prefix(1);
	}
	while (Line.size() > 1 && u8(Line.back()) <= u8(' '))
	{
		Line.remove_suffix(1);
	}
	return Line;
}
}

void GetShaderCRC(IReader* F, u32& final_crc, SStringVec& files)
{
	string_path pFileName = {};
	string_path pFilePath = {};

	while (!F->eof())
	{
		const xr_string_view Line = ShaderSource::ReadShaderCRCLine(*F);

		if (Line.starts_with('#') &&
			Line.substr(0, Line.find('\0')).find("#include") != xr_string_view::npos)
		{
			const xr_string IncludeLine(Line.data(), u32(Line.size()));
			if (_GetItem(IncludeLine.c_str(), 1, pFileName, '"'))
			{
				if(std::find(files.begin(), files.end(), pFileName) == files.end())
				{
					RImplementation.ShaderPath(pFilePath, pFileName);

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

		final_crc = crc32(Line.data(), Line.size(), final_crc);
	}
}

// Addons made for the layout before shaders left d3d11\ keep their files there: such a file wins over the root's
void CRender::ShaderPath(string_path& Out, const char* Name, const char* Extension)
{
	xr_strconcat(Out, "d3d11\\", Name, Extension);

	if (!FS.exist(_game_shaders_, Out))
	{
		xr_strconcat(Out, Name, Extension);
	}
}

class includer : public IRHIShaderInclude {
public:
	HRESULT Open(u32 IncludeType, const char* pFileName, const void* pParentData, const void** ppData, u32* pBytes) override
	{
		string_path pname;
		RImplementation.ShaderPath(pname, pFileName);
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

	HRESULT Close(const void* pData) override
	{
		xr_free(pData);
		return S_OK;
	}
};

namespace
{
// What making shaders took since the last report, summed over the threads that made them
std::atomic<u32> ShaderLoads{ 0 };
std::atomic<u32> ShaderCompiles{ 0 };
std::atomic<u64> ShaderSourceUs{ 0 };
std::atomic<u64> ShaderCacheUs{ 0 };
std::atomic<u64> ShaderCreateUs{ 0 };
std::atomic<u64> ShaderCompileUs{ 0 };
std::atomic<u64> ShaderCacheBytes{ 0 };

u64 TakeUs(CTimer& Timer)
{
	const u64 Us = u64(Timer.GetElapsed_ms_f() * 1000.f);
	Timer.Start();
	return Us;
}
} // namespace

void CRender::LogShaderTimes(const char* Stage)
{
	Msg("* shaders at %s: %u made, %u compiled; summed over threads: source checksum %llu ms, cache read %llu ms (%llu MB), create %llu ms, compile %llu ms", Stage, ShaderLoads.exchange(0), ShaderCompiles.exchange(0),
		ShaderSourceUs.exchange(0) / 1000, ShaderCacheUs.exchange(0) / 1000, ShaderCacheBytes.exchange(0) >> 20, ShaderCreateUs.exchange(0) / 1000, ShaderCompileUs.exchange(0) / 1000);
}

HRESULT	CRender::shader_compile(
	const char* name,
	u32 const* pSrcData,
	u32 SrcDataLen,
	const char* pFunctionName,
	const char* pTarget,
	u32 Flags,
	void*& result,
	const ShaderExternalMap& Defines)
{
	PROF_EVENT("CRender::shader_compile");
	PROF_EVENT_DYNAMIC(name);
	ShaderExternalMap AllDefines = GetShaderDefines(Defines);
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
	CTimer Stage;
	Stage.Start();
	++ShaderLoads;

	IReader R = IReader((char*)pSrcData, SrcDataLen);
	GetShaderCRC(&R, RealCodeCRC, crc_file_list);

	const u32 options_crc = DefinesCRC(AllDefines);
	ShaderSourceUs += TakeUs(Stage);

	Flags |= RHI_SHADER_OPTIMIZATION_LEVEL3;
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

				ShaderCacheBytes += file->elapsed();
				ShaderCacheUs += TakeUs(Stage);

				if(real_crc == ShaderCRC) 
				{
					inShader->m_crc1 = ShaderCRC;
					inShader->m_crc2 = CodeSRC;

					_result = create_shader(pTarget, (u32*)file->pointer(), file->elapsed(), file_name, result, o.disasm);
					ShaderCreateUs += TakeUs(Stage);
				}
			}
		}

		file->close();
	}

	if(FAILED(_result)) 
	{
		RHIBlob* pShaderBuf = nullptr;
		RHIBlob* pErrorBuf = nullptr;
		CTimer Compile;
		Compile.Start();
		++ShaderCompiles;

		includer Includer;
		{
			PROF_EVENT("CRender::D3DCompile");
			PROF_EVENT_DYNAMIC(name);
			_result = GRHI->CompileShader(
				pSrcData,
				SrcDataLen,
				name,
				defines,
				&Includer,
				pFunctionName,
				pTarget,
				Flags,
				&pShaderBuf,
				&pErrorBuf
			);
		}

		if (SUCCEEDED(_result))
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

			_result = create_shader(pTarget, (u32*)pShaderBuf->GetBufferPointer(), (u32)pShaderBuf->GetBufferSize(), file_name, result, o.disasm);
		}

		if (pErrorBuf)
		{
			Msg("! error: %s\n%s", file_name, (const char*)pErrorBuf->GetBufferPointer());
		}

		if (FAILED(_result) && !pErrorBuf)
		{
			Msg("Can't compile shader [%s] hr=0x%08x", file_name, _result);
		}
		_RELEASE(pShaderBuf);
		_RELEASE(pErrorBuf);
		ShaderCompileUs += TakeUs(Compile);
	}

	return _result;
}

RHIBlob* CRender::CompileStage(const char* Name, const char* Extension, const char* Target)
{
	string_path Path;
	string16 Entry;
	IReader* File = OpenShader(Path, Name, Extension, Entry);

	if (!File)
	{
		return nullptr;
	}

	const ShaderExternalMap Defines = GetShaderDefines({});
	RHIShaderMacro Macros[128];
	u32 Count = 0;

	for (const auto& [Key, Value] : Defines)
	{
		R_ASSERT(Count < std::size(Macros) - 1);
		Macros[Count++] = { Key.c_str(), Value.c_str() };
	}

	Macros[Count] = { nullptr, nullptr };
	includer Includer;
	RHIBlob* Code = nullptr;
	RHIBlob* Errors = nullptr;
	GRHI->CompileShader(File->pointer(), File->length(), Path, Macros, &Includer, Entry, Target, RHI_SHADER_PACK_MATRIX_ROW_MAJOR | RHI_SHADER_OPTIMIZATION_LEVEL3, &Code, &Errors);
	FS.r_close(File);

	if (Errors)
	{
		Msg("! error: %s\n%s", Path, (const char*)Errors->GetBufferPointer());
		_RELEASE(Errors);
	}

	return Code;
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

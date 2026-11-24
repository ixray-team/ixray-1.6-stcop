#pragma once

#include "r__dsgraph_structure.h"

#include "PSLibrary.h"

#include "r__types.h"
#include "ixr_rendertarget.h"

#include "HOM.h"
#include "DetailManager.h"
#include "ModelPool.h"
#include "WallmarksEngine.h"

#include "SMAP_Allocator.h"
#include "ShaderBindings.h"
#include "Light_DB.h"
#include "LightTrack.h"
class CGlowManager;
#include "r_sun_cascades.h"

#include "../../xrEngine/IRenderable.h"
#include "../../xrEngine/Fmesh.h"
#include "ixr_editor_hiz.h"
#include "RainPuddleWaterMap.h"
#include "HeightfieldShadowMap.h"
#include "PackedVertex.h"
#include "SkinningPass.h"
#include "NriPass.h"
#include "DynamicGeometry.h"
#include "FramePacer.h"
#include "ViewRecorder.h"
#include "GpuScene.h"
#include "LampSelection.h"
#include <atomic>

class dxRender_Visual;
struct IRender_Mesh;

// definition
class RENDER_API CRender :
	public IRender_interface,
	public pureFrame
{
public:
	bool vis_intersect = false;
	RainPuddleWaterMap RainPuddleWater;
	HeightfieldShadowMap HeightfieldShadows;
	void UpdateRainPuddleMode();
	CGlowManager* L_Glows = nullptr;

	enum
	{
		MMSM_OFF = 0,
		MMSM_ON,
		MMSM_AUTO,
		MMSM_AUTODETECT
	};

public:
	struct _options	
	{
		u32 HW_smap_FORMAT		: 32;
		u32 smapsize			: 16;
		    
		u32 nvstencil			: 1;
		u32 nvdbt				: 1;
		    
		u32 nullrt				: 1;
		    
		u32 distortion			: 1;
		u32 distortion_enabled	: 1;
		    
		u32 sunstatic			: 1;
		u32 noshadows			: 1;
		u32 disasm				: 1;
		u32 offscreen_reflecitons	: 1;

		u32 RainPuddles : 1;
		u32 ProceduralRainPuddles : 1;
		u32 GtaoHq : 1;
		u32 SslrHq : 1;
		u32 dx11_disable_motion_vectors : 1;
		u32 DofCompiled : 1;
	} o;

	struct _stats
	{
		u32		l_total,	l_visible;
		u32		l_shadowed,	l_unshadowed;
		s32		s_used,		s_merged,	s_finalclip;
		u32		o_queries,	o_culled;
	}			stats;
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

	GeometryHeap Geometry;
	SkinningPass Skinning;
	NriPass NriPasses;
	DynamicGeometry Dynamic;
	FramePacer Pacer;
	GpuScene Scene;
	LampSelection LampSeen;
	// The lamps that have a part with something moving to cast: the rest ask the game for no objects
	xr_vector<bool> LampWanted;
	GfxCmdList Backend;
	GfxCmdList& Imm() { return Backend; }

	R_dsgraph_structure GraphMain;
	// Static geometry of the main view on a level the scene does not draw whole, collected on a worker while the primary
	// thread collects the dynamic part, then moved into GraphMain
	R_dsgraph_structure GraphMainStatic;
	xr_task_group MainStaticTask;
	xr_array<R_dsgraph_structure, 6> GraphReflection;
	xr_array<R_dsgraph_structure, 3> GraphSun;
	R_dsgraph_structure GraphRain;
	R_dsgraph_structure GraphPass;	// primary thread passes that follow one another: lights, puddles
	R_dsgraph_structure GraphLampCollect;	// the lamp parts' portals and the spot shadow cache's classification, apart from the draw's

	R_dsgraph_structure& BuildPassGraph(GfxCmdList& Cmd, const GraphView& View); // the editors' only

	// Every graph with a view of its own
	template <class Callback>
	void ForEachViewGraph(Callback&& Function)
	{
		for (R_dsgraph_structure& Graph : GraphReflection)
		{
			Function(Graph);
		}
		for (R_dsgraph_structure& Graph : GraphSun)
		{
			Function(Graph);
		}
		Function(GraphRain);
		Function(GraphPass);
		Function(GraphLampCollect);
	}

	// Global vertex-buffer container
	xr_vector<FSlideWindowItem>									SWIs;
	xr_vector<ref_shader> ShadersLevel;
	xr_hash_map<shared_str, ref_shader> ShadersShared;
	struct LevelVertexChunk
	{
		static constexpr u16 Absent = 0xFFFF;
		xr_vector<u8> Data;
		u32 Count = 0;
		u32 Stride = 0;
		u16 Position = Absent;
		u16 Normal = Absent;
		u16 Tangent = Absent;
		u16 Binormal = Absent;
		u16 Color = Absent;
		u16 Tex0 = Absent;
		u16 Tex1 = Absent;
		bool Tex0Short4 = false;
	};
	xr_vector<LevelVertexChunk>									LevelVertices;
	xr_vector<xr_vector<u16>>									LevelIndices;
	xr_vector<PackedVertex>										LevelScratch;
	// A mesh of the level's geometry chunks: vertex buffer, first vertex, count, index buffer, first index, count. Block
	// is empty until UploadLevelMesh gives it a place in the heap
	struct LevelMesh
	{
		xr_array<u32, 6> Key;
		GeometryAlloc Block;
	};
	xr_vector<LevelMesh>										LevelMeshes;
	xr_map<xr_array<u32, 6>, u32>								LevelMeshIds;
	xr_vector<dxRender_Visual*>									Visuals;
	CPSLibrary													PSLibrary;

	CDetailManager*												Details;
	CDetailManager*												GetDetails() const;
	CModelPool*													Models;
	CWallmarksEngine*											Wallmarks;

	CRenderTarget*												Target;			// Render-target

	CLight_DB													Lights;
	SMAP_Allocator												LP_smap_pool;


	float														o_hemi			;
	float														o_hemi_cube[CROS_impl::NUM_FACES]	;
	float														o_sun			;


	bool														m_bMakeAsyncSS;
	bool														m_bFirstFrameAfterReset;	// Determines weather the frame is the first after resetting device.
	xr_vector<sun::cascade>										m_sun_cascades;

	xr_list<light*>												v_all_lights_dque;

private:
	sun::CascadePreparation PreparedSunCascades[3];
	// The cascades RenderFarSunCascade draws this frame, chosen when the frame starts. A task of SunCascadeTasks builds the
	// static part of each one's graph from its view; nothing else touches that graph until RenderFarSunCascade waits
	GraphView SunCascadeViews[3];
	u32 SunCascadeDrawn[3] = {};
	u32 SunCascadeDrawnCount = 0;
	xr_task_group SunCascadeTasks;
	// Their spatial queries, asked with the lamps' (LampQueries) when the frame has shadowed lamps
	xr_vector<ISpatialShared> SunCascadeQueries[3];
	bool IsSunCascadeQueried = false;
	STextureList SunShadowEmptyTextures;
	u32 SunUpdatedMask = 0;

	IRHIRenderTargetView* UIImageTargets[4] = {};
	IRHIDepthStencilView* UIImageDepth = nullptr;
	u32 UIImageWidth = 0;
	u32 UIImageHeight = 0;
	u32 UIImageFrame = u32(-1);

	void ResetSunShadowCache();

	// Loading / Unloading
	void							LoadVisuals					(IReader	*fs);
	void							LoadLights					(IReader	*fs);
	void							LoadPortals					(IReader	*fs);
	void							LoadSectors					(IReader	*fs);
	void							LoadVertexBuffers			(IReaderBase& fs);
	void							LoadIndexBuffers			(IReaderBase& fs);
	void							LoadSWIs					(IReaderBase& fs);

	// The spatial queries of the forward frame's shadowed lamps, a sphere each (LampLights), made on the workers before the
	// parts are drawn
	xr_vector<xr_vector<ISpatialShared>> LampQueries;

	void							BeginMainStatic				(bool dont_test_sectors);
	void							CollectMainStatic			(R_dsgraph_structure& Graph, bool dont_test_sectors);
	void							EndMainStatic				();

public:
	void							render_main					(GfxCmdList& Cmd);
	void RenderForwardLevel(GfxCmdList& Cmd);

	// Local lights of a frame whose level is drawn forward: gathered while the frame collects, then handed to the
	// surface shaders as one buffer
	xr_vector<light*> SurfaceLights;
	IRHIBuffer* SurfaceLightBuffer = nullptr;
	IRHIShaderResourceView* SurfaceLightView = nullptr;
	// The textures the frame's spots project, in the slots s_forward_projector0.. of the surface shaders
	ref_texture SurfaceProjectorDefault;
	xr_vector<CTexture*> SurfaceProjectors;
	// The empty light list the tiles' pass reads beside the decals (CreateForwardLists)
	IRHIBuffer* ForwardLightBuffer = nullptr;
	IRHIShaderResourceView* ForwardLightView = nullptr;
	IRHIBuffer* ForwardDecalBuffer = nullptr;
	IRHIShaderResourceView* ForwardDecalView = nullptr;
	// The same records as NRI reads them, for the frame
	nri::Descriptor* ForwardDecalRecords = nullptr;
	u32 ForwardDecals = 0;
	// The frame's local lights in view space, in the order of the surface lights' records: what the tiles of the
	// screen are marked by; how many the tiles were last marked by
	xr_vector<Hlsl::SceneLight> TileLights;
	u32 TiledLights = 0;
	// The decals' textures: a layer of one array per slot of the wallmark engine, see FillDecalLayer
	static constexpr u32 DecalLayers = 64;
	IRHISurface* DecalArray = nullptr;
	IRHIShaderResourceView* DecalArrayView = nullptr;
	IRHIRenderTargetView* DecalLayerTarget[DecalLayers] = {};
	shared_str DecalLayerTexture[DecalLayers];
	void UploadSurfaceLights(GfxCmdList& Cmd);
	void CreateForwardLists();
	void DispatchLightTiles(GfxCmdList& Cmd, nri::Descriptor* DecalRecords);
	void FillDecalLayer(GfxCmdList& Cmd, u32 Layer, const shared_str& Texture);
	u32 BuildForwardDecals(GfxCmdList& Cmd);
	void BindForwardDecals(GfxCmdList& Cmd);
	void							render_menu					(GfxCmdList& Cmd);
	bool RenderRainMap(GfxCmdList& Cmd);
	void BindRainTextures(GfxCmdList& Cmd);

	void							init_cacades				();
	void BeginFarSunCascades();
	void RenderFarSunCascade(GfxCmdList& Cmd);
	void CollectShadowCasters();
	u32 ShadowCastersFrame = u32(-1);
	// A lamp part's view of the dsgraph; a part drawn whole (Index: of LampParts), or in the editors, which submit their
	// scene into it; the part's graph, then its grass and translucent casters; its moving casters queued for the page batch
	GraphView MakeLampView(const light& L) const;
	// A shadow view's portals after Begin, then the level visuals the batch leaves to its graph, if it leaves any. A lamp
	// part's portals are traversed once a frame, by LampCollect (LampTiles); RenderLampShadow walks them again only then
	void TraverseShadowView(R_dsgraph_structure& Graph) const;
	bool RenderLampShadow(GfxCmdList& Cmd, CRenderTarget::SpotShadowPart& Part, u32 Index);
	bool RenderEditorLampShadow(GfxCmdList& Cmd, light& L);
	void DrawLampGraph(GfxCmdList& Cmd, light& L, bool HasGrass, ShadowKind Kind);
	void QueueLampTile(GfxCmdList& Cmd, const CRenderTarget::SpotShadowPart& Part, u32 Index);
	// The lamps' parts of the frame and what casts in them, read from the game; then their maps drawn
	void CollectForwardLampShadows();
	void RenderForwardLampShadows(GfxCmdList& Cmd);
	xr_vector<CRenderTarget::SpotShadowPart> LampParts;
	xr_vector<GpuTileView> LampTiles; // of LampParts: their moving casters out of the frame's table, the sectors their level is culled by
	xr_vector<light*> LampLights; // the lights LampParts are of, a spatial query each
	void RenderForwardPuddles(GfxCmdList& Cmd);
	ref_geom PuddleGeom;
	void RenderForwardOit(GfxCmdList& Cmd, const ref_rt& Scene);
	void SyncForwardMsaa();
	void BeginForwardMsaa();
	u32 ForwardMsaaApplied = u32(-1);
	u32 ForwardPassOnly = u32(-1); // the pass of its material the grass is drawn with, every pass when -1
	void RenderForwardOcclusion(GfxCmdList& Cmd);
	void RenderSkyBehind(GfxCmdList& Cmd);
	void EndForwardMsaa(GfxCmdList& Cmd);
	void CopyForwardLensScene(GfxCmdList& Cmd);
	class CROS_impl* ViewerLightTrack() const;
	int GetSunShadowCacheMode() const;
	void GetSunShadowConstants(u32 Index, Fmatrix& Transform, Fvector4& Capture, Fvector4& Warp) const;
	void DumpSunShadowCache() const;
	GraphView MakeGraphView(u32 Phase) const;
	// Starts a collection into the main graph with the current camera and phase
	void BeginMainCollect();
	GraphView SunView;

	// Graphs the pre-render stage collects in this frame, in order. The primary thread only waits
	// for the jobs it is about to draw.
	xr_vector<GraphCollectJob> GraphJobs;
	std::atomic<u32> GraphJobsDone{ 0 };
	u32 ReflectionJobsEnd = 0;

	void BeginGraphCollect();
	void CollectGraphs();
	void WaitGraphJobs(u32 Count);

	void begin_reflection_collect();
	void render_reflections(GfxCmdList& Cmd);

	bool IsRainPassEnabled() const;
	bool prepare_rain_collect();
	GraphView MakeRainView() const;
	void prepare_sun_collect(u32 Frame);
	GraphView MakeSunView(u32 Cascade) const;
	bool IsSunCascadeCollected(u32 Cascade) const;
	void RestoreSunCascadeChain();
	bool prepare_sun_cascade_xforms(u32 Frame);

	Fvector reflection_cam_pos;
	Fvector reflection_cam_dir;
	Fvector reflection_cam_top;
	Fvector reflection_cam_right;
	Fmatrix reflection_view = Fidentity;
	float reflection_near = 0.2f;
	float reflection_far = 1000.f;
	float ReflectionFog = 0.f; // the weather's fog distance, taken with the far plane when the faces are chosen
	IRender_Sector* reflection_sector = nullptr;
	u32 ReflectionFaces = 0; // the faces of the cube around the eye this frame collects and draws, a bit a face
	u32 ReflectionCursor = 0;
	const void* ReflectionCube = nullptr; // the cube whose faces have all been drawn once

	xr_array<Fmatrix, 3> sun_cascade_xforms{};
	Fvector sun_cull_cop{};
	u32 sun_cascade_count = 0;
	bool sun_restore_shafts = false;
	bool sun_saved_reset_chain = false;
	bool sun_collect_active = false;
	u32 SunCollectFrame = u32(-1);

	Fmatrix RainTransform = Fidentity;
	Fvector RainOrigin{};
	float RainFov = 0.f;
	bool RainActive = false;
	bool RainMapDrawn = false;
	u32 RainCollectFrame = u32(-1);

	bool is_render_cubemap = false;

public:
	ShaderElement*					rimp_select_sh_static		(const R_dsgraph_structure& Graph, dxRender_Visual* pVisual, float cdist_sq);
	ShaderElement*					rimp_select_sh_dynamic		(const R_dsgraph_structure& Graph, dxRender_Visual* pVisual, float cdist_sq, bool is_hud = false);
	u32								RegisterLevelMesh			(u32 VertexBuffer, u32 VertexBase, u32 VertexCount, u32 IndexBuffer, u32 IndexBase, u32 IndexCount);
	void							UploadLevelMesh				(IRender_Mesh& Mesh);
	void							UploadLevelLeftovers		();
	FSlideWindowItem*				getSWI						(int id);
	IRender_Portal*					getPortal					(int id);
	IRender_Sector*					getSectorActive				();
	IRenderVisual*					model_CreatePE				(const char* name);
	IRender_Sector*					detectSector				(const Fvector& P, Fvector& D);
	IRender_Sector*					detectLastSector			(const Fvector& P);
	int								translateSector				(IRender_Sector* pSector);

	virtual SurfaceParams getSurface(const char* nameTexture) override;

	Fvector avg_lit_color, avg_lit_dir;

	// The light tracker of the one who looks, taken when the frame begins: what is drawn with no tracker of its own
	// (the HUD, an icon) is lit by it
	CROS_impl* ViewerTrack = nullptr;
	// What the game let go while a frame that may still read it was in flight: freed when no frame is (FreeQueued)
	xr_vector<IRender_ObjectSpecific*> TrackersToFree;
	xr_vector<ref_texture> TexturesToFree;
	void FreeQueued();
	// Where the frames meet (r_render_thread pipelined): what OnFrame and the device's hooks do before the game's frame
	// when no frame is in flight
	void FrameSync();
	void CollectGraphsNow();
	void ClearDeferredModels();
	bool IsGraphCollectHere = false;

	ICF void						apply_object				(GfxCmdList& Cmd, IRender_ObjectSpecific* Lit)
	{
		// Editors do not track lighting per object (no CROS_impl): one hemi for the whole scene, the whole sun, handed
		// over as the tracker of the baked branch below hands its own
		if (Device.IsEditorMode())
		{
			EditorApplyHemi			();
			Cmd.Constants.Write<CBObject>().L_dynamic_props.set(0.5f, 0.5f, 0.5f, 0.5f * EditorHemi);
			return;
		}

		Cmd.Constants.Write<CBObject>().L_dynamic_props.set(0, 0, 0, 0);
		Cmd.Constants.Write<CBObject>().m_plmap_clamp[0].set(0, 0, 0, 1);
		Cmd.Constants.Write<CBObject>().m_plmap_xform = Fidentity;
		CROS_impl* Track = Lit ? static_cast<CROS_impl*>(Lit) : ViewerTrack;
		if (!Track)
			return;
		// Smoothed by the collect (SceneCollector::set_Object, render_main for the one who looks)
		CROS_impl& light_state = *Track;
		const float sun = 0.5f * light_state.drawn_sun();
		Cmd.Constants.Write<CBObject>().L_dynamic_props.set(sun, sun, sun, 0.5f * light_state.drawn_hemi());
		CopyMemory(o_hemi_cube, light_state.drawn_hemi_cube(), CROS_impl::NUM_FACES * sizeof(float));
	}
	IC void							EditorApplyHemi				()
	{
		o_hemi						= 0.75f*EditorHemi;
		o_sun						= 0.75f;
		for (float& Face : o_hemi_cube)
		{
			Face					= EditorHemi;
		}

		avg_lit_color = avg_lit_dir = { 0,0,0 };
	}
	IC void							apply_lmaterial				(GfxCmdList& Cmd)
	{
		CBDraw& Draw = Cmd.Constants.Write<CBDraw>();
		Cmd.GetView().View.transform_tiny(avg_lit_dir);
		Draw.L_model_light_color.set(avg_lit_color.x, avg_lit_color.y, avg_lit_color.z, 0);
		Draw.L_model_light_dir.set(avg_lit_dir.x, avg_lit_dir.y, avg_lit_dir.z, 0);
		avg_lit_color = avg_lit_dir = { 0,0,0 };

		CTexture* T = Cmd.GetPassTexture(CTexture::rstPixel + ShaderBindings::Texture("s_base"));
		if (!T)
		{
			return;
		}

		float mtl = T->m_material;

#ifdef	DEBUG_DRAW
		if (ps_r2_ls_flags.test(R2FLAG_GLOBALMATERIAL))	
		{
			mtl = ps_r2_gmaterial;
		}
#endif

		mtl += 0.50f;
		mtl *= 0.25f;

		Draw.L_material.set(o_hemi, o_sun, 0, mtl);
		Draw.hemi_cube_pos_faces.set(o_hemi_cube[CROS_impl::CUBE_FACE_POS_X],
			o_hemi_cube[CROS_impl::CUBE_FACE_POS_Y],
			o_hemi_cube[CROS_impl::CUBE_FACE_POS_Z], 0);
		Draw.hemi_cube_neg_faces.set(o_hemi_cube[CROS_impl::CUBE_FACE_NEG_X],
			o_hemi_cube[CROS_impl::CUBE_FACE_NEG_Y],
			o_hemi_cube[CROS_impl::CUBE_FACE_NEG_Z], 0);
	}

public:
	// feature level
	virtual GenerationLevel			get_generation			() override;

	virtual Fvector SunDirection() override { return Lights.SunDirection(); }
	// Logs what making shaders took since the last call, and starts the count again
	void LogShaderTimes(const char* Stage);
	virtual bool					is_sun_static			() override { return o.sunstatic || !ForwardDynamicLight(); }
	virtual u32 get_dx_level() override { return 0x000A0001; }

	virtual void					detail_trample_mark(float x, float y, float z, float radius, float weight, bool isActor = false) override
	{
		if (Details)
			Details->TrampleMark(x, y, z, radius, weight, isActor);
	}

	virtual bool					detail_trample_enabled() const override
	{
		extern int ps_trample_enabled;
		return ps_trample_enabled != 0;
	}

	virtual float					detail_trample_draw_radius() const override
	{
		extern float ps_trample_draw_radius;
		return ps_trample_draw_radius;
	}

	// Detail Layers Editor tool (brush overlay + ImGui window)
	virtual void renderImGuiDebugWindow_DetailLayersEditor() override;
	void DetailLayers_RenderBrush3D(GfxCmdList& Cmd);
	// Releases the lazily created brush render objects while the RHI/device is still
	// alive (called from destroy()). Never leave these statics to DLL detach - the
	// device is gone by then and their destructors crash on the imported DevicePtr.
	void DetailLayers_EditorDestroy();

	// Loading / Unloading
	virtual void create() override;
	virtual void destroy() override;
	virtual void reset_begin() override;
	virtual void reset_end() override;

	virtual void level_Load(IReader*) override;
	virtual void level_Unload() override;
	virtual void renderImGuiDebugWindow_SVGStorage() override;

	IRHISurface* load_texture(const char*	fname, u32& msize, bool bStaging = false) override;
	bool get_texture_metadata(const char* absolute_path, RHITextureMetadata* p_data) override;

	IRHISurface* texture_load(const char*	fname, u32& msize, bool bStaging = false);

	HRESULT							shader_compile			(
		const char*							name,
		u32 const* pSrcData,
		u32 SrcDataLen,
		const char*                          pFunctionName,
		const char*                          pTarget,
		u32 Flags,
		void*&							result,
		const ShaderExternalMap&		Defines);
	// The code of a stage the resource manager has no kind for (a mesh stage), compiled as it is asked for; null: it failed
	RHIBlob*						CompileStage			(const char* Name, const char* Extension, const char* Target);
	void							SetShaderCacheBypass	(bool bypass);
	bool							ShaderCacheBypassed		() const { return m_shaderCacheBypass; }

	struct PuddleBase 
	{
		Fmatrix m_world = Fidentity;

		float m_height = EPS;
		float m_radius = EPS;
	};

	xr_vector<PuddleBase> m_levels_puddles;
	Frect m_puddles_level_bound;

	void							LoadPuddles();

	// Information
	virtual void Statistics(CGameFont* F) override;
	virtual const char* getShaderPath() override { return ""; }
	void ShaderPath(string_path& Out, const char* Name, const char* Extension = "");
	IReader* OpenShader(string_path& Path, const char* Name, const char* Extension, string16& Entry);
	virtual ref_shader getShader(int id);
	virtual ref_shader getShaderShared(shared_str id);
	virtual IRender_Sector* getSector(int id) override;
	virtual IRenderVisual* getVisual(int id) override;
	virtual IRender_Sector* detectSector(const Fvector& P) override;
	virtual IRender_Target* getTarget() override;

	// Main
	void							set_Object					(IRenderable* O, R_dsgraph_structure* Graph = nullptr);
	virtual void					add_Occluder				(Fbox2&	bb_screenspace	) override;			// mask screen region as oclluded

	// wallmarks
	virtual void					add_StaticWallmark			(ref_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V, bool UseCameraDirection = false);
	virtual void					add_StaticWallmark			(IWallMarkArray *pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V, bool UseCameraDirection = false) override;
	virtual void					add_StaticWallmark			(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V) override;
	virtual void					clear_static_wallmarks		() override;
	virtual StaticWallmarkHandle::WallmarkHandlePtr add_DynamicWallmark(const wm_shader& S, const Fvector& P, float w, float h, float r, CDB::TRI* T, Fvector* V) override;
	virtual void					add_SkeletonWallmark		(intrusive_ptr<CSkeletonWallmark> wm);
	virtual void					add_SkeletonWallmark		(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size);
	virtual void					add_SkeletonWallmark		(const Fmatrix* xf, IKinematics* obj, IWallMarkArray *pArray, const Fvector& start, const Fvector& dir, float size) override;

	//
	virtual IBlender*				blender_create				(CLASS_ID cls);

	//
	virtual IRender_ObjectSpecific*	ros_create					(IRenderable*		parent) override;
	virtual void					ros_destroy					(IRender_ObjectSpecific* &) override;

	// Lighting
	virtual IRender_Light*			light_create				() override;
	virtual IRender_Glow*			glow_create					() override;

	// Models
	virtual IRenderVisual*			model_CreateParticles		(const char* name) override;
	virtual IRender_DetailModel*	model_CreateDM				(IReader* F);
	virtual IRenderVisual*			model_Create				(const char* name, IReader* data=0) override;
	virtual IRenderVisual*			model_CreateChild			(const char* name, IReader* data) override;
	virtual IRenderVisual*			model_Duplicate				(IRenderVisual*	V) override;
	virtual void					model_Delete				(IRenderVisual* &	V, bool bDiscard) override;
	virtual void					model_Delete_Deffered		(IRenderVisual* &	V) override;
	virtual void 					model_Delete				(IRender_DetailModel* & F);
	virtual void					models_Prefetch				() override;
	virtual void					models_Clear				(bool b_complete) override;
	void							model_Request				(const char* name, EModelRequestPriority Priority) override;
	EModelLoadState					model_GetState				(const char* name) override;
	void							models_WaitRequests			() override;
	void							models_GetLoaded			(xr_vector<shared_str>& Names) override;

	// Occlusion culling
	virtual bool					occ_visible					(vis_data&	V) override;
	virtual bool					occ_visible					(Fbox&		B) override;
	virtual bool					occ_visible					(sPoly&		P) override;

	// Main
	virtual void					Calculate					() override;
	virtual void					Render						() override;
	virtual void					RenderUI					(xr_span<const UIIconVisual> Visuals, const Fmatrix& View, const Fmatrix& Project, Fcolor* Color = nullptr) override;
	// The level frame's reads of the game: the main view's visibility and its moving objects. CollectFrame takes them
	// ahead of Render, on the thread that hands the frame to the render thread; Render takes them itself otherwise
	void CollectFrame();

	// With r_render_thread the game's thread calls Render to have the scene drawn while it records its own draws
	// (dxRenderDeviceRender::RunFrame): RecordRender keeps the call for the render thread and has the game make, each
	// into a list, the draws it makes at four places inside the scene's frame. The scene's draw plays them there
	struct SceneDraws
	{
		DrawRecorder::List ItemUi;
		DrawRecorder::List ActiveItemUi;
		DrawRecorder::List PostMain;
		DrawRecorder::List PostDistort;
		// The way Render takes, as the game answered when the lists were made: 0 a level, 1 the menu's, 2 nothing
		u8 Path = 0;
	};

	void RecordRender();
	void DrawIcon(xr_span<const UIIconVisual> Visuals, const Fmatrix& View, const Fmatrix& Project, Fcolor* Color, Irect Scissor);
	xr_vector<xr_unique_ptr<SceneDraws>> RecordedScenes;
	u32 RecordedSceneCount = 0;
	const SceneDraws* PlayedScene = nullptr;
	DrawRecorder::List FrameDraws;
	void TakeWeatherEffects();
	void DrawSky();
	void CollectForwardLevel();
	u32 CollectedFrame = u32(-1);
	virtual SurfaceParams			BeginUIImage				() override;
	virtual void					EndUIImage					() override;
	virtual IDSGraphManager*		GetMainGraph				() override { return EditorGraph; }

	virtual void					Screenshot					(ScreenshotMode mode=SM_NORMAL, const char* name = 0) override;
	virtual void					Screenshot					(ScreenshotMode mode, CMemoryWriter& memory_writer) override;
	virtual void					ScreenshotAsyncBegin		() override;
	virtual void					ScreenshotAsyncEnd			(CMemoryWriter& memory_writer) override;
	virtual void		_BCL		OnFrame						() override;

	virtual CDB::MODEL* GetHOMModel() override;
	virtual xr_vector<u32>* GetHOMInvaltids() override;

	// Render mode
	virtual void					rmNear						() override;
	virtual void					rmFar						() override;
	virtual void					rmNormal					() override;

	// Constructor/destructor/loader
	CRender														();
	virtual ~CRender											() override;

	ShaderExternalMap				GetShaderDefines			(const ShaderExternalMap& Defines) const;
	xr_string						getShaderParams				(const ShaderExternalMap& Defines) const;
	xr_string						getShaderParamsDebug		(const ShaderExternalMap& Defines) const;

	bool							NeedMotionVectors			() const;
	bool HasReflectionCube() const;
	bool							MotionVectorsDisabled		() const;
	void							SyncMotionVectors			();
	void							SyncDof						();
	// Writes the renderer's variables from the effective video settings when they change
	void ApplyVideoSettings();
	void RegisterVideoSettingsSupport(bool IsRegistered);
	u32 VideoSettingsApplied = u32(-1);
	void SyncShadingQuality();
	u64 ShaderKeyApplied = 0;

	// What the renderer made for its features
	bool HasUpscaler = false;
	void ApplyPresetOptions();

	virtual bool					InIndoor					() override { return pLastSector!=pOutdoorSector; };
	virtual size_t					SectorsCount				() override { return Sectors.size(); }

public:
	// Shaders created by this thread get the editor instancing variant. Per thread: objects load
	// on worker threads while the main thread creates instanced shaders, and a shared flag
	// compiled their regular shaders as instanced.
	void							SetEditorInstancing			(bool Value);
	bool							IsEditorInstancing			() const;

	void							Initialize					();
	void							ShutDown					();
	void							OnDeviceCreate				();
	void							OnDeviceDestroy				();

	void							model_Render				(IRenderVisual* V, const Fmatrix& Transform, int Priority, bool StrictB2F, float LOD);
	void							model_RenderSingle			(IRenderVisual* V, const Fmatrix& Transform, float LOD);

	void							EditorBeginOverlay			();
	void							EditorPresent				(IRHIRenderTargetView* Destination);
	void							EditorEndFrame				();
	Fcolor							EditorClearColor			= { 0.f, 0.f, 0.f, 1.f };
	bool							EditorIsLit					() const;

	CEditorHiZ						EditorHiZ;
	bool							EditorOcclusionCulling		= true;

	// Lighting window of the editors. The compilers bake hemi for the game; the editors use one value.
	float							EditorHemi					= 0.5f;
	bool							EditorDynamicLights			= true;
	bool							EditorLightShadows			= true;

	// The graph the editor scene is submitted into: the main one, or a shadow view's while that view collects
	R_dsgraph_structure*			EditorGraph					= &GraphMain;

	void							EditorRenderMain			(GfxCmdList& Cmd);
	// Collects the editor scene into a graph begun with its view
	void							EditorRenderSubspace		(GfxCmdList& Cmd, R_dsgraph_structure& Graph);
	void							EditorCollect				(GfxCmdList& Cmd, bool Shadows);
	void							EditorSyncLights			();
	xr_vector<light*>				EditorLights;

private:
	void							EditorCreate				();
	void							EditorDestroy				();
	void							EditorRenderUnlit			(GfxCmdList& Cmd);
	// Depth for the helpers drawn at the viewport size over the (possibly upscaled) scene
	IRHIDepthStencilView*			EditorOverlayDepth			(GfxCmdList& Cmd);

	IBlender*						EditorUnlitBlender			= nullptr;
	ref_shader						EditorUnlitShader;
	IBlender*						EditorDepthUpscaleBlender	= nullptr;
	ref_shader						EditorDepthUpscaleShader;

public:

private:
	bool							m_shaderCacheBypass = false;

protected:
	virtual void					ScreenshotImpl				(ScreenshotMode mode, const char* name, CMemoryWriter* memory_writer) override;

private:
	FS_FileSet						m_file_set;
};

extern RENDER_API CRender			RImplementation;

ICF u32 R_dsgraph_structure::ViewPhase() const
{
	return OwnsView ? ViewParams.Phase : RImplementation.phase;
}

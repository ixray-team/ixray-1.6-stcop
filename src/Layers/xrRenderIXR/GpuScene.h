/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#pragma once

namespace nri
{
struct Buffer;
struct CommandBuffer;
} // namespace nri

namespace Hlsl
{
struct alignas(16) uint4
{
	u32 x, y, z, w;
};

#include "../../../shaders/common/scene_tables.hlsli"
}

static_assert(sizeof(Hlsl::SceneMesh) == 32);
static_assert(sizeof(Hlsl::SceneInstance) == 48);
static_assert(sizeof(Hlsl::SceneTransform) == 96);
static_assert(sizeof(Hlsl::SceneCluster) == 48);
static_assert(sizeof(Hlsl::SceneCard) == 160);
static_assert(sizeof(Hlsl::SceneMaterial) == 16 * SCENE_MATERIAL_MAPS);
static_assert(sizeof(Hlsl::SceneModel) == 192 && SCENE_MODELS <= 1u << SCENE_VISIBLE_MODEL_SHIFT);
static_assert(SCENE_DYNAMIC_RUNS < 1u << (SCENE_VISIBLE_TILE_SHIFT - SCENE_VISIBLE_MODEL_SHIFT) && SCENE_VIEW_TILES <= 1u << (31 - SCENE_VISIBLE_TILE_SHIFT));
static_assert(sizeof(Hlsl::SceneGrassBin) == 48);
static_assert(sizeof(Hlsl::SceneGrassView) == 6768);
static_assert(sizeof(Hlsl::SceneGrassWind) == 160);
static_assert(sizeof(Hlsl::SceneView) == 2832);
static_assert(sizeof(Hlsl::SceneClusterLod) == 48);
static_assert(sizeof(Hlsl::SceneRange) == 48);
static_assert(sizeof(Hlsl::SceneTreeLight) == 64);
static_assert(sizeof(Hlsl::SceneLight) == 64);
static_assert(sizeof(Hlsl::SceneLightShadow) == 80);
static_assert(sizeof(Hlsl::SceneDecal) == 80);
static_assert(sizeof(Hlsl::SceneLightBlock) == 32 + 4 * SCENE_LIGHT_BINS + 64 * SCENE_LIGHTS + 80 * SCENE_LIGHT_SHADOWS);

class CFrustum;

// The shadow views other than the sun cascades, as the stats count what their graphs still draw
enum class ShadowKind : u32
{
	Lamp,
	Rain,
	SpotCache,
	Count
};

// The views that call renderable_Render of moving objects, as the frame's table of moving objects counts them
enum class ObjectView : u32
{
	Main,
	Lamp,
	Decor,
	Cascade,
	Count
};

// A lamp part drawn into a tile of a spot shadow target, its moving casters out of the frame's table: its view, the
// sectors its portals reached and the object of the table it leaves out (IsOnly: the one object it casts, a decor object).
// IsPartial: an object it would cast was not called, its shadow off the screen, so what it casts is not baked
struct GpuTileView
{
	Fmatrix Transform;
	u32 Sectors[SCENE_VIEW_SECTORS / 32] = {};
	u32 Object = SCENE_OBJECT_NONE;
	bool IsOnly = false;
	bool IsPartial = false;
};

struct GpuDepthDraw
{
	IRHIDepthStencilView* Target;
	u32 Slice = 0;
	const RHIViewport* Area = nullptr;	// the part of the target drawn to, all of it when null
	Fvector Look{};						// the way an orthographic view looks
	const Fvector* Eye = nullptr;		// the eye of a perspective view, which has no Look
	const Fmatrix* Transform = nullptr;	// the view's matrix and frustum, where the graph does not hold them
	const CFrustum* Frustum = nullptr;
	const u32* Sectors = nullptr;		// the sectors its portals reached (GpuTileView::Sectors), where the graph did not traverse them
	bool IsSpot = false;				// the fixed state of the spot shadow maps instead of the shadow pass state
	bool IsMain = false;				// the main view: its own clusters and distance rules
	u32 HizPart = 0;					// the binned main view in two parts: 1 what the frame before showed, 2 the rest against the depth pyramid
	bool IsForward = false;				// grass: only the cull, for the forward frame's own draws of what it finds
	bool IsBinned = false;				// the visible clusters are sorted by the pipeline of their material
	bool IsFace = false; // a face of the cube around the eye: nothing is dropped for its size, detail is picked for the face
	u32 Face = 0;
	bool IsKept = false;				// the target outlives the frame: nothing that depends on the camera; trees stay as they stand
	ShadowKind Kind = ShadowKind::Lamp;	// any other shadow view, as the stats count it
};

class dxRender_Visual;
struct SPass;
struct Shader;
class IRender_Sector;
class R_dsgraph_structure;
class CFrustum;
class GfxCmdList;
class IRHIDepthStencilView;
struct GraphView;
class IRenderable;
class IDSGraphManager;
class CObject;

// Everything that is drawn, as persistent records on the GPU. So far: level geometry, written once
// at level load, and shadow casters that a view culls and draws in one batch: level geometry as
// clusters, and the meshes of the moving objects as model records written every frame (the frame's table): opaque, or
// cut out by a texture of the alpha array.
class GpuScene
{
public:
	// Needs the level's vertex and index chunks, so it runs before they are released
	// A detail model for the scene's tables: its material and its index data, the far mesh first
	struct GrassModel
	{
		Shader* Material;
		const u16* Indices;
		u32 IndexCount;
		u32 FarCount;
		u32 VertexBase;
		u32 Page;
		u32 Cluster = u32(-1); // out: its first cluster, when the scene can draw it
	};

	void Load(const xr_vector<dxRender_Visual*>& Visuals, const xr_vector<IRender_Sector*>& Sectors, xr_vector<GrassModel>& Details);
	void Unload();
	using DepthDraw = GpuDepthDraw;

	// The instances of one grass model and wind variant this frame (CDetail::SlotItem records)
	struct GrassBin
	{
		const void* Items;
		u32 Count;
		float Radius; // around the root
		float OwnRadius; // of the sphere Radius - OwnRadius above the root
		u32 Cluster; // GrassModel::Cluster
		u32 FarTriangles; // of its far mesh, 0 without one
		u32 Triangles;
		const Fvector4* Wave; // wave, wind and both a frame ago: the sway of its wind variant
		u32 Variant; // 0: stands still, 1 and 2: the wind variants
	};

	bool HasDepthBatch() const { return ClusterCount != 0; }
	// Draws the frame's grass into a depth target in one pass, culled against the view on the GPU.
	// The instances are uploaded once per frame. Cmd holds the constants the
	// grass shaders read. False when it cannot: the caller then draws the grass itself.
	// The forward frame's grass as the blades the slots keep in the scene's buffer (GrassSlotStore): a word a blade,
	// where it is stored and its bin above SCENE_GRASS_STORE_BITS. The bins then hold no blades of their own
	struct GrassRuns
	{
		const u32* Words;
		u32 Count;
		class GrassSlotStore* Store;
	};

	bool DrawGrass(GfxCmdList& Cmd, u32 Frame, const GrassBin* Bins, u32 BinCount, const CFrustum& Frustum, const DepthDraw& Draw, const GrassRuns* Runs = nullptr);
	// Draws the level's clusters the graph's view sees into a depth target; false when there is no batch. A graph that
	// collects pass 1 (a lamp's, with r4_translucent_shadows) has the casters of a later priority drawn too
	bool DrawDepth(GfxCmdList& Cmd, R_dsgraph_structure& Graph, const DepthDraw& Draw);
	// Views with cull lists and sets of their own (View < OwnViewCount: a sun cascade of the forward frame, a page of its
	// lamps' shadows), each recorded into a command buffer of its own. On the primary thread their batches are kept, not
	// recorded: PrepareView for a cascade (its moving casters are the frame's table, CollectCasters), and from KeepDepth(View)
	// on every DrawDepth and DrawTiles. PrepareViews once ends the keeping (the constant blocks, the targets wrapped and
	// handed over writable, the shared buffers made readable). Then RecordView on any thread, the view's batches in order,
	// touching nothing but its own lists and the command buffer, or RecordViews, the same into the old backend's list.
	// FinishViews once recording is done
	static constexpr u32 OwnViewCount = 4;
	bool PrepareView(GfxCmdList& Cmd, R_dsgraph_structure& Graph, const DepthDraw& Draw, u32 View);
	void KeepDepth(u32 View);
	void PrepareViews(GfxCmdList& Cmd);
	u32 PreparedViews(u32 (&Views)[OwnViewCount]) const; // the views with batches, in order
	void RecordView(nri::CommandBuffer& Commands, u32 View);
	void RecordViews(GfxCmdList& Cmd);
	void FinishViews();
	// The moving casters of a lamp part, out of the frame's table, into its Area of a spot shadow target: queued, then
	// culled and drawn with those of the other parts of the target, up to SCENE_VIEW_TILES of them, in one cull and one
	// draw by DrawTiles (also when the target changes or the tiles are full), or kept (KeepDepth). False when the scene cannot
// draw them
	bool QueueTile(GfxCmdList& Cmd, IRHIDepthStencilView* Target, u32 Slice, const RHIViewport& Area, const GpuTileView& View);
	void DrawTiles(GfxCmdList& Cmd);
	// The lamp parts of the frame, before CollectCasters: BeginLamps, then for each part that casts what moves HitLamp, with
	// Graph traversed for the part (the objects of Found in a sector its portals reached, inside a frustum they left there,
	// but Ignore, and, unless the main view calls it or the part's cache holds a bake, whose shadow can reach the main view;
	// the reached sectors into View), or HitDecor; Part is the index into CRender::LampParts. After CollectCasters: the
	// object of the table an object is (SCENE_OBJECT_NONE for none), the records a tile view casts as its cull tests them
	// with a hash of them (0 when one of them can change without its record: skinned, or one has none: IsPartial), and what the batch cannot draw of
	// the part's objects into its graph
	void BeginLamps();
	void HitLamp(const R_dsgraph_structure& Graph, const xr_vector<ISpatialShared>& Found, const IRenderable* Ignore, u32 Part, GpuTileView& View);
	void HitDecor(IRenderable* Object, u32 Part);
	u32 ObjectOf(const IRenderable* Object) const;
	u32 TestTile(const GpuTileView& View, u64& Hash) const;
	void InsertLampLeftovers(R_dsgraph_structure& Graph, u32 Part) const;
	// The main view of the forward frame: what it sees of the level, sorted by pipeline
	// IsSecond: after BuildHiZ, what the first part did not draw and the depth so far does not hide
	bool CullForward(R_dsgraph_structure& Graph, bool IsSecond = false);
	// The level's LOD cards the main view shows, after its clusters are drawn: against this frame's depth pyramid where it
	// has one. DrawForward draws them (Parts 8, 16); a card the scene cannot draw stays in the graph's mapLOD
	void CullCards(R_dsgraph_structure& Graph);
	// The frame's table of moving objects. BeginObjects, before the main view collects: the calls of renderable_Render of
	// the frame go to Objects (SceneCollector), which writes what they add straight into the table: the opaque meshes whose
	// materials have draws of the scene as records (CullForward tests them with the level's first part), those of the HUD
	// by the same rules, drawn by the HUD's projection and never culled, the moving surfaces of the sorted lists as records
	// and entries of Main's sorted lists, which DrawSorted draws in the lists' order. What the scene cannot draw waits in
	// the leftover list with its reason. CollectObjects, in place of the main view's own collection: the lights, glows,
	// particle systems and moving objects the main view's tests keep (its frustum, sectors and portals, the occlusion map,
	// fog, size), their skeletons posed once, then renderable_Render of each once, in the order of the spatial query.
	// BuildObjects, once the HUD is collected: a model shader made after the load joins the scene, with its textures, when a
	// frame first meets it, and its leftovers are taken; the rest of the leftover list goes to Main's lists, which draw it
	// as they drew the graph's packets. A level shader on a moving object is drawn as a model (ShaderVariant::ClusterModel).
	// CollectCasters, once a frame after the sun cascades' portals are traversed and the lamp parts hit (BeginLamps): the
	// records of the main view's objects the batch can cast are flagged, then the objects a cascade's collection or a lamp
	// part would call that the main view did not, and whose shadow can reach the main view, are called once, in the shadow
	// phase (those only lamps see posed first; a far skeleton keeps its pose up to the engine's slow interval,
	// CKinematics::HoldPose): their records follow the main view's, and what the batch cannot draw goes to the graphs of
	// the cascades and lamp parts that would have collected it. UploadModels writes the table once a frame (the one
	// skinning pass first), at CollectCasters or before the main view's cull
	void BeginObjects(R_dsgraph_structure& Main);
	IDSGraphManager& Objects();
	void CollectObjects(R_dsgraph_structure& Main);
	void BuildObjects(R_dsgraph_structure& Main);
	void CollectCasters(R_dsgraph_structure* Cascades, const u32* Drawn, u32 DrawnCount, xr_vector<ISpatialShared>* Queries);
	void UploadModels(GfxCmdList& Cmd);
	// A face of the cube around the eye: the level's clusters in its view, a draw a pipeline, as CullForward leaves
	// them. The main view's cull must come after the faces are drawn: the two share their buffers
	bool CullFace(R_dsgraph_structure& Graph, const Fvector& Eye, const Fmatrix& Transform, u32 Face);
	// The pyramid of the farthest depths of the frame so far, for the second part of the main view's cull
	// IsLoose: a texel hides where most of it was drawn, not all of it (see scene_hiz.cs.hlsl)
	void BuildHiZ(GfxCmdList& Cmd, IRHIDepthStencilView* Depth, bool IsLoose = false);
	// The forward frame draws part of the level from the scene's clusters
	bool IsForwardBinned() const;
	// The scene draws every static of the level the main view or a face of the cube around the eye shows
	bool IsStaticBinned() const;
	// The main view takes what the scene leaves to the graph from GetMainStatics, without a walk of the sectors: every
	// level whose statics the scene draws whole, and one with leftovers while the lists are short. A long list is
	// tested a visual at a time with no tree to drop whole branches by: 93 thousand of them were ten milliseconds
	bool IsMainListed() const { return IsStaticBinned() || (IsForwardBinned() && MainListed <= 4096); }
	// The forward frame draws its grass from the scene's grass cull; and whether this frame's cull has run
	bool HasForwardGrass() const;
	// The depth pass of what is cut out by its texture shades a block of two by two pixels a run, see USE_COARSE_ALPHA
	static bool IsCoarseAlpha();
	// The device shades at a coarse rate with a coverage of its own for every pixel; r_variable_shading is not asked
	static bool IsVariableRate();
	bool IsForwardGrassReady() const;
	// The frame's records for the forward draw's pixel stage: 0 the local lights, 1 the decals
	void SetForwardRecords(u32 Kind, const void* Data, u32 Bytes);
	// The forward frame reads the texture from the buckets alone: its whole copy is loaded only when a draw binds it
	bool HasNoWholeCopy(const CTexture* Texture) const;
	// The level textures an old-path draw loaded whole again, to the log
	static void LogWholeCopies();
	void LogTexturePools() const;
	// The forward draws take the passes of their materials again: after the materials are compiled again
	void RefreshForward();
	// What CullForward found, a draw a pipeline: the depth passes into Targets and Depth, or the colour passes
	// Parts: bit 0 the first part of the cull (and the grass), bit 1 the second; bit 3 the LOD cards cut out, bit 4 blended
	// where they fade in; bit 5 the segments DrawSorted or DrawDistortion has queued; bit 6 the blended bins, both parts;
	// bit 7 with bit 5: DrawIcon's segments, clipped by the old backend's scissor; bit 8: the id pass, the level's bins
	// alone, one target of ids; bit 9: everything but the level's bins
	// FaceSlice: the slice of the cube targets a face is drawn into, with the pipelines made for a face
	// Parts 1024: the models' opaque bins alone, for a frame that lights its surfaces later: their depth pass keeps the
	// surface in the last of Targets, their colour pass draws their HUD parts; 2048: every bin but those
	bool DrawForward(GfxCmdList& Cmd, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth, bool IsColour, u32 Parts = 1, const u32* FaceSlice = nullptr);
	// The frame draws the level as ids and shades a pixel of it once: high shading and lighting quality, no
	// multisampling
	bool IsVisibilityFrame() const;
	// The surfaces of the level's pixels from their ids, where Depth still holds the triangle an id names: what Light
	// lights, into Surface, and the word the screen-space passes read, into Thin
	void Resolve(GfxCmdList& Cmd, IRHISurface* Ids, IRHISurface* Depth, IRHISurface* Surface, IRHISurface* Thin);
	// Lights the surfaces Resolve wrote, into Colour
	// Thin: that word; Mirror: what the reflection passes traced, none without them
	void Light(GfxCmdList& Cmd, IRHISurface* Surface, IRHISurface* Depth, IRHISurface* Thin, IRHISurface* Mirror, IRHISurface* SkyBehind, IRHISurface* Colour);
	// The main view's sorted surfaces (Graph.mapSorted, Graph.mapHUDSorted with IsHud) into Targets and Depth, back to
	// front: the scene draws those it holds as clusters or model records (BuildObjects), neighbours of one pass in one
	// draw, the graph the rest between them; the particle effects leave the list, are recorded as the stream's batches
	// before the walk and submitted at their keys. IsWeighed: the surfaces weighed
	// order-independently (WBOIT), which leave the list and are drawn in any order, the scene's a draw a pass
	void DrawSorted(GfxCmdList& Cmd, R_dsgraph_structure& Graph, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth, bool IsWeighed, bool IsHud = false);
	// The distortion of the world, then the HUD's (Graph.mapDistort, mapHUDDistort), or the scope's mask (IsScope,
	// mapHUDScopeMask), into Target and Depth in the lists' insertion order, as DrawSorted draws: the scene the XML blends'
	// Special elements it holds as cluster passes (ShaderVariant::ClusterModel), the graph the rest and the particle effects
	// between them
	void DrawDistortion(GfxCmdList& Cmd, R_dsgraph_structure& Graph, IRHIRenderTargetView* Target, IRHIDepthStencilView* Depth, bool IsScope);
	// What a 3D icon's picture is made of: every visual's parts' materials and meshes, its pose and where it stands, the
	// device's view and projection, the icons' light and the streamed heads of the frame
	u32 IconKey(xr_span<const UIIconVisual> Visuals);
	// Whether the interface's icon target still holds the picture of Key at Place, its scissor there
	bool IsIconKept(const Irect& Place, u32 Key);
	// A 3D icon of the interface into Target and Depth at Place, by the view the command list holds: its visuals' mesh parts as
	// records of the table's top (IconModels a frame) with their interface element's cluster pass, the opaque ones in
	// their order, then the blended ones back to front, neighbours of one pass in one draw. Its picture is kept under Key
	// when every part was drawn
	void DrawIcon(GfxCmdList& Cmd, xr_span<const UIIconVisual> Visuals, const Irect& Place, u32 Key, IRHIRenderTargetView* Target, IRHIDepthStencilView* Depth);
	// The icon target was made again, a level loads or the materials were compiled again: no picture is kept
	void ForgetIcons();
	// What the main view's graph draws itself of the second priority, counted for the stats line before it draws it
	void CountGraphPasses(const R_dsgraph_structure& Graph);

	void EndFrame();

	// The visuals of a sector the batch does not draw, or null when a view has to walk the sector. IsFirstOnly: for a view
	// that collects the first pass alone, without those whose shadow puts nothing there
	const xr_vector<dxRender_Visual*>* GetLeftover(u32 Sector, bool IsFirstOnly) const { return Sector < Leftover[IsFirstOnly].size() ? &Leftover[IsFirstOnly][Sector] : nullptr; }
	// What the main view collects itself of a sector when the scene draws every other static (IsStaticBinned), in the
	// order a walk of the sector meets it: the sorted surfaces the scene holds as clusters, whose visibility and order the
	// graph's lists keep, and the LOD groups with such a surface or with a card the scene cannot draw. Null: walk the sector
	const xr_vector<dxRender_Visual*>* GetMainStatics(u32 Sector) const { return IsMainListed() && Sector < MainStatics.size() ? &MainStatics[Sector] : nullptr; }
	// The batch leaves a shadow view of these passes (Begin) no level visual: the view walks its portals alone
	bool IsShadowBinned(const R_dsgraph_structure& Graph) const;
	// What a shadow view's graph draws itself, counted for the stats line right before it does; IsTranslucent: its
	// translucent pass too
	void CountGraph(ShadowKind Kind, const R_dsgraph_structure& Graph, bool IsTranslucent = true);

	// A renderable_Render call made for an object of a view: render_memory_stats prints them against the table
	void CountRendered(ObjectView View, IRenderable* Object);

private:
	void CreatePasses(const xr_vector<Hlsl::SceneCluster>& Records, const xr_vector<Hlsl::SceneClusterLod>& Lods, const xr_vector<u32>& Words);
	// Casters: model records of the view from First, or of the frame's models for a view with bins
	void Cull(R_dsgraph_structure& Graph, const DepthDraw& Draw, u32 Casters, u32 First = 0);
	// The view block of a cull; false when the cull keeps what it wrote before (a face of the cube)
	bool MakeCullBlock(R_dsgraph_structure& Graph, const DepthDraw& Draw, u32 Casters, u32 First, Hlsl::SceneView& Block);
	// What a cull needs of the shared buffers before its own lists: done once, copied, made writable
	void PrepareCull(nri::CommandBuffer& Commands, const Hlsl::SceneView& Block);
	void Dispatch(const Hlsl::SceneView& Block);
	void Raster(GfxCmdList& Cmd, IRHIDepthStencilView* Target, u32 Slice, const RHIViewport* Area, nri::Pipeline* Opaque, nri::Pipeline* Alpha);

	nri::Buffer* Meshes = nullptr;
	nri::Buffer* Instances = nullptr;
	nri::Buffer* Transforms = nullptr;
	xr_vector<xr_vector<dxRender_Visual*>> Leftover[3]; // per sector: with a shadow element, of the first pass, all of them
	xr_vector<xr_vector<dxRender_Visual*>> MainStatics;
	u32 MainListed = 0; // how many the lists hold
	u32 SectorCount = 0;
	u32 ClusterCount = 0;
};

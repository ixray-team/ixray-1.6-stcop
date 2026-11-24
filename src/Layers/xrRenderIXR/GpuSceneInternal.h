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

#include "GpuScene.h"
#include "SceneCollector.h"
#include "../../../shaders/common/debug_view.hlsli"
#include "GrassThin.h"

#include <bit>
#include <bitset>

#include "FHierrarhyVisual.h"
#include "FVisual.h"
#include "FTreeVisual.h"
#include "FLOD.h"
#include "FProgressive.h"
#include "SkeletonCustom.h"
#include "SkeletonX.h"
#include "NriPass.h"
#include "StateManager/dx10SamplerStateCache.h"
#include "MaterialFeatures.h"
#include "GpuTextures.h"
#include "TextureDecode.h"
#include "dxRenderDeviceRender.h"
#include "r__sector.h"

// The engine defines Device as a macro; NRI and NriDevice use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

#include <NRI.h>
#include <Extensions/NRIHelper.h>
#include <Extensions/NRIMeshShader.h>
#include <Extensions/NRIStreamer.h>
#include <Extensions/NRIWrapperD3D11.h>
#include <Extensions/NRIWrapperD3D12.h>

u32 GetStaticCullSteps(bool Shadow, float Fov, Hlsl::float4* Steps);
void GetTreeWave(Fvector4& Wave, Fvector4& Wind, float& Scale);
extern float r_ssaLOD_A, r_ssaLOD_B;
extern float r_ssaGLOD_start, r_ssaGLOD_end;

namespace GpuSceneInternal
{
}

using namespace GpuSceneInternal;

namespace GpuSceneInternal
{
constexpr u32 FrameCount = 4;
constexpr u32 VertexStride = 32;
constexpr u32 PageSlots = GeometryHeap::MaxPages;
constexpr u32 GrassCells = 16;

// ERHI_COMPARISON keeps the D3D numbering, which starts at 1
const nri::CompareOp Compare[] = { nri::CompareOp::NONE, nri::CompareOp::NEVER, nri::CompareOp::LESS, nri::CompareOp::EQUAL, nri::CompareOp::LESS_EQUAL, nri::CompareOp::GREATER, nri::CompareOp::NOT_EQUAL, nri::CompareOp::GREATER_EQUAL, nri::CompareOp::ALWAYS };

struct Tracked
{
	nri::Buffer* Buffer = nullptr;
	nri::AccessStage State = {};
};

extern nri::CoreInterface Api;
extern nri::HelperInterface Help;
extern nri::StreamerInterface StreamApi;
extern nri::PipelineLayout* CullLayout;
extern nri::PipelineLayout* DrawLayout;
extern nri::Pipeline* CullPipeline;
struct DepthPipelines
{
	nri::Format Format;
	bool IsSpot;
	nri::Pipeline* Opaque;
	nri::Pipeline* Alpha;
	nri::Pipeline* Tiled = nullptr;
	nri::Pipeline* TiledAlpha = nullptr;
	nri::Pipeline* Mesh = nullptr; // the solid list as meshlets (scene_depth.ms.hlsl), where the device has mesh stages

	nri::Pipeline* Solid() const
	{
		return Mesh && ps_r_mesh_shaders ? Mesh : Opaque;
	}
};

extern xr_vector<Hlsl::SceneMaterial> LevelMaterials; // the level's materials as loaded, for the resolve's own table
extern nri::MeshShaderInterface MeshApi;
extern nri::PipelineLayout* MeshLayout; // DrawLayout for a mesh stage: null where the device has none

extern xr_vector<DepthPipelines> Pipelines;
extern nri::DescriptorPool* Pool;
extern nri::DescriptorSet* CullSet;
extern nri::DescriptorSet* DrawSet;
extern nri::DescriptorSet* AlphaSet;
extern nri::Streamer* Streamer;
extern nri::Descriptor* ViewBlock;
extern nri::Descriptor* SkinView;
extern nri::Descriptor* SkinFiller;
extern nri::Buffer* Clusters;
extern nri::Buffer* Materials;
extern nri::Buffer* TreeLights;
extern nri::Buffer* ClusterLods;
extern nri::Descriptor* ClusterLodView;
extern nri::Descriptor* TreeLightView;
extern Tracked Counters, Visible, VisibleAlpha;
extern nri::Buffer* Ranges;
extern Tracked Chunks, CullGroups;
extern Tracked Density;
extern bool IsDensityBuilt;
extern nri::PipelineLayout* DensityLayout;
extern nri::Pipeline* DensityPipeline;
extern nri::DescriptorSet* DensitySet;
extern xr_vector<u32> GrassStreams;
extern u32 RangeCount;
extern nri::PipelineLayout* RangeLayout;
extern nri::Pipeline* RangePipeline;
extern nri::DescriptorSet* RangeSet;

// The lists a cull writes and the sets that read them: the shared ones, or those of a view recorded on its own
struct ViewLists
{
	Tracked& Counters;
	Tracked& Visible;
	Tracked& VisibleAlpha;
	Tracked& Chunks;
	Tracked& CullGroups;
	nri::DescriptorSet* CullSet;
	nri::DescriptorSet* RangeSet;
	nri::DescriptorSet* DrawSet;
	nri::DescriptorSet* AlphaSet;
	nri::DescriptorPool* Pool;
};

ViewLists SharedLists();

// The constant blocks of a cull, streamed on the primary thread: the view of the range pass, the view of the pass over
// its first list and, of a view with pipeline lists, of each band's pass over the cut-out list
struct CullBlocks
{
	u32 Range = 0;
	u32 Bands[1 + SCENE_BANDS] = {};
	u32 BandCount = 0;
};

extern nri::DescriptorPool* OwnPool;

// A cull and its two indirect draws kept for RecordView: its view block, where it draws and with what. PrepareViews
// streams the blocks and wraps the target, one wrapper for the batches of a target in a row (Texture null for the rest)
struct DepthBatch
{
	Hlsl::SceneView Block;
	CullBlocks Blocks;
	IRHIDepthStencilView* Target;
	u32 Slice;
	RHIViewport Drawn;
	nri::Pipeline* Opaque;
	nri::Pipeline* Alpha;
	nri::Texture* Texture;
	nri::Descriptor* SliceView;
};

struct OwnView
{
	Tracked Counters, Visible, VisibleAlpha, Chunks, CullGroups;
	nri::DescriptorSet* CullSet = nullptr;
	nri::DescriptorSet* RangeSet = nullptr;
	nri::DescriptorSet* DrawSet = nullptr;
	nri::DescriptorSet* AlphaSet = nullptr;
	xr_vector<DepthBatch> Batches; // recorded in order, one after another, through these lists

	ViewLists Lists()
	{
		return { Counters, Visible, VisibleAlpha, Chunks, CullGroups, CullSet, RangeSet, DrawSet, AlphaSet, OwnPool };
	}
};

static_assert(GpuScene::OwnViewCount >= SCENE_CASTER_VIEWS);
extern OwnView OwnViews[GpuScene::OwnViewCount];
extern u32 KeptView; // the view DrawDepth and DrawTiles keep their batches for, OwnViewCount for none
extern Tracked Seen;
extern bool IsSeenCleared;
extern nri::Descriptor* HizRead;
extern nri::PipelineLayout* HizLayout;
extern nri::Pipeline* HizPipelines[2];
extern u32 HizWidth;
extern u32 HizHeight;
extern ref_cs HizShaders[2];
void ReleaseHiZ();
// The depth pyramid of the frame before as scene_occlusion.hlsli reads it: its view, the scale of its projection with the
// near plane, depth = x + y / view z, its size with its last level. false when that frame built none, or the camera cut
bool HizOfFrameBefore(Fmatrix& View, Fvector4& Scale, Fvector4& Depth, Fvector4& Size);
extern nri::Buffer* CullStatsCopy;
extern u32 CullStatsStep;
constexpr u64 GrassArgBytes = SCENE_GRASS_WORDS * sizeof(u32);
// With the arguments of a draw a model and mesh after them
constexpr u64 GrassDrawBytes = GrassArgBytes + SCENE_GRASS_BINS * 2 * sizeof(nri::DrawIndexedDesc);
extern GpuTextures Textures;
extern nri::Texture* Alpha;
extern nri::Descriptor* AlphaView;
extern Tracked Feedback;
// One a frame in flight: the one read is the oldest, which the GPU has long finished. Mapping the one just written
// makes D3D11 wait for the GPU
// Read back when eight frames old: a map of a copy the device has not reached makes the frame wait for it
constexpr u32 FeedbackFrames = 8;
// Room past the level's own counts for what joins the scene after it is loaded: the clusters and the shader
// pairs of models. The lists and counters are made this much larger, so nothing is made again when one joins
constexpr u32 ModelClusters = 1u << 16;
// A part of the visible list is ClusterCount + ModelClusters long: the level's bins, then in the main view's first
// part the runs of the frame's models. A depth view's dynamic runs stay inside the first part
static_assert(SCENE_DYNAMIC_CLUSTERS <= ModelClusters);
extern nri::Buffer* ModelUploads[FrameCount];
extern Tracked Models;
extern nri::Descriptor* ModelView;
struct ModelMaterial
{
	u32 Bin;
	u32 Material;
	bool IsBound = false; // HasBoundMap: a sorted list draws it with BoundPass
};

extern xr_hash_map<Shader*, ModelMaterial> ModelBinIds;
extern xr_vector<ref_shader> ModelShaders;
extern u32 LevelBinCount;
// Why a mesh part of the main view or of its sorted lists stays in the graph, as the forward cull's stats count them.
// Priority: its element's first pass is of the second priority; Off: the scene takes no models (r_scene_models 0, a level
// the scene does not bin)
enum class ModelLeft : u32
{
	Taken,
	Shader,
	Env,
	Joining,
	Incomplete,
	NoRoom,
	Emissive,
	Element,
	Kind,
	Page,
	Hidden,
	Triangles,
	Caps,
	Priority,
	Off,
	Count
};

extern xr_hash_map<Shader*, ModelLeft> ModelRejects;
extern xr_hash_map<Shader*, u32> MaterialIds;
extern u32 MaterialCapacity;
extern size_t BinLimit;
extern xr_hash_map<CTexture*, GpuTextures::Ref> TextureRefs;
// The textures the buckets have no place for, bound as they are to every forward draw (SCENE_TEXTURE_BOUND)
extern xr_vector<ref_texture> BoundTextures;
// A map's place among them, the tail of the reference: Absent with Why when it has none
GpuTextures::Ref BindTexture(CTexture* Texture, const char*& Why);
// A material that reads one of them: only its draws are made with SCENE_BUCKETS_BOUND
bool HasBoundMap(const Hlsl::SceneMaterial& Record);
// The pass with its pixel stage made again with SCENE_BUCKETS_BOUND, made once a pass and held in BoundPasses (the pass,
// the one made); the pass itself when its pixel stage is made from no file
const ref_pass& BoundPass(const SPass* Pass);
extern xr_hash_map<const SPass*, std::pair<ref_pass, ref_pass>> BoundPasses;
constexpr u32 NoEnv = u32(-1);
extern xr_hash_map<CTexture*, u32> EnvSlots;
extern nri::Descriptor* EnvArrayView;
extern xr_vector<Hlsl::SceneModel> ModelRecords; // the frame's table
extern u32 ModelOpaque; // the records the cull takes, those of the opaque bins; the sorted lists' follow them
extern xr_vector<CSkeletonX*> ModelSkins; // per record: the part the skinning pass makes the vertices of, or none
// The records from ShadowFirst only the shadow views draw: where each stands, its sphere read once the skeletons are posed
extern u32 ShadowFirst;
extern xr_vector<std::pair<Fmatrix, const Fsphere*>> ShadowSpheres;
constexpr u32 NoRecord = u32(-1);
// The table's top belongs to the 3D icons of the interface, drawn after the frame's records are written (DrawIcon)
constexpr u32 IconModels = 512;
constexpr u32 FrameModels = SCENE_MODELS - IconModels;
// The materials of icons' parts whose pixel stage reads the material table: joined by the next frame's JoinModels
extern xr_vector<ref_shader> IconJoins;

// A mesh part of the world an object of the table added in the main view, once a visual: its record, NoRecord for none
struct MainPart
{
	dxRender_Visual* Visual;
	Fmatrix World;
	float Ssa;
	u32 Object;
	u32 Record;
};

extern xr_vector<MainPart> MainParts;

// A mesh part or particle effect a call of renderable_Render added (SceneCollector), as the graph's packet held it
struct MovingLeaf
{
	dxRender_Visual* Visual;
	IRenderable* Object;
	Fmatrix World;
	float Ssa;
	float Distance; // squared, from the eye; of a HUD part from the HUD's origin
	u32 Id; // the object of the table it was added for, SCENE_OBJECT_NONE for none
	bool IsHud;
};

extern SceneCollector Collector; // the target of the frame's calls of renderable_Render
// The table's stats lines of the frame before, when render_memory_stats asks, before the frame's collection resets it
// (GpuSceneObjects.cpp)
void LogFrameObjects();

// The main view's: its record, its entry of the graph's sorted lists, or the leftover list BuildObjects hands the graph
// (GpuSceneModels.cpp)
void AddMainLeaf(R_dsgraph_structure& Graph, const MovingLeaf& Leaf, bool IsInvisible, bool IsDistortion, const bool (&Mask)[3]);
// The shadow phase's, of an object only the shadow views call: its casters' records, or its views' leftovers
// (GpuSceneObjects.cpp)
void AddShadowLeaf(const MovingLeaf& Leaf, bool IsInvisible, const bool (&Mask)[3]);
// The size test of a moving visual in a view that is not a shadow's (r__dsgraph_build.cpp)
bool IsMovingValuable(const dxRender_Visual& Visual, const Fmatrix& World, const Fvector& Camera, float Fov);

inline void RequestMovingSkin(dxRender_Visual* Visual, const Fmatrix& World)
{
	const bool IsPart = Visual->Type == MT_SKELETON_GEOMDEF_ST || Visual->Type == MT_SKELETON_GEOMDEF_PM;
	CSkeletonX* Mesh = IsPart ? static_cast<CSkeletonX*>(Visual) : nullptr;

	if (Mesh && Mesh->IsSkinned() && Mesh->visible && Mesh->Parent)
	{
		RImplementation.Skinning.Request(*Mesh, World);
	}
}

// A part is tested by the sphere of its skeleton, as the graph tests the object
inline const Fsphere& PartSphere(const dxRender_Visual* Visual)
{
	const bool IsPart = Visual->Type == MT_SKELETON_GEOMDEF_ST || Visual->Type == MT_SKELETON_GEOMDEF_PM;
	return IsPart ? static_cast<const CSkeletonX*>(Visual)->Parent->vis.sphere : Visual->vis.sphere;
}
// A moving surface of a sorted list the scene draws: its record, the element its runs are drawn with
struct SortedModel
{
	const dxRender_Visual* Visual;
	u32 Model;
	const ShaderElement* Element;
	bool IsBound = false;
};

extern xr_vector<SortedModel> SortedModels; // of the frame ModelFrame, by visual
// Where a plain mesh stood, by the address of its visual: this frame, the frame before. The graph keeps no earlier
// place of it
using ModelPlace = std::pair<const dxRender_Visual*, Fmatrix>;
extern xr_vector<ModelPlace> ModelWorlds[2];
extern u32 ModelFrame;
extern u32 ModelRuns;
extern u32 ModelsLeft[2][u32(ModelLeft::Count)];
extern xr_vector<std::pair<shared_str, u32>> ShadersLeft, CullStatsShaders;
extern u32 ModelCapacity;
// Why a moving caster of a shadow view stays in its graph (MakeCaster)
enum class CasterLeft : u32
{
	Taken,
	Kind,
	Shader,
	State,
	Alpha,
	Page,
	Hidden,
	Triangles,
	Caps,
	Count
};
// Why the moving packets stayed, and what the graph then drew itself: its static and its moving packets of the first pass,
// and the packets of its translucent pass
struct CasterCounts
{
	u32 Left[u32(CasterLeft::Count)] = {};
	u32 Drawn[3] = {};
};
extern CasterCounts CascadeCounts[SCENE_CASTER_VIEWS]; // of the frame's table in each sun cascade
bool TakesCasters();
// Why the depth batch cannot draw a moving caster's pass as it is, else Taken with the slice of the alpha array cutting it
CasterLeft CasterPass(const SPass& Pass, bool IsPart, u32& Alpha);
// The record of a moving caster's pass the batch draws, its skinned part asked of the skinning pass, else why it cannot.
// HasRoom: a record is left; Runs: what the view has left, less what it takes. IsDetailed: a rigid progressive mesh at its
// full detail, else at the one the main view picked last
CasterLeft MakeCaster(const SPass& Pass, dxRender_Visual* Visual, const Fmatrix& Matrix, bool IsDetailed, bool HasRoom, u32& Runs, Hlsl::SceneModel& Record, CSkeletonX*& Skin);
// What a graph draws of a moving mesh the scene does not: its element's passes as packets, of the priorities of Lists (bit 0
// the first, bit 1 the second) or only the pass Only (r__dsgraph_build.cpp)
void InsertPasses(R_dsgraph_structure& Graph, const ShaderElement* Element, dxRender_Visual* Visual, IRenderable* Object, const Fmatrix& World, float Ssa, u32 Lists = 3, u32 Only = u32(-1));
// What the shadow views of a kind left to their graphs: in the frame Frame, and in the one before
struct KindCounts
{
	u32 Frame = u32(-1);
	CasterCounts Now, Last;
};

extern KindCounts KindsLeft[u32(ShadowKind::Count)];
CasterCounts& CountsOf(ShadowKind Kind); // of this frame
extern string1024 MainLeftLine;
extern u32 MainLeftovers; // the level visuals the main view draws from the graph, counted at the load
extern u32 ShadowLeftovers[2]; // the level visuals the shadow views walk, as GpuScene::Leftover 0 and 1 hold them
extern u32 MainPackets;
extern u32 CardCount;
extern u32 CardStart;
extern u32 CardFrame;
extern nri::Buffer* CardTable;
extern Tracked CardArgs;
extern nri::PipelineLayout* CardLayout;
extern nri::Pipeline* CardCull;
extern nri::DescriptorPool* CardPool;
extern nri::DescriptorSet* CardSet;
extern ref_shader CardSource;
extern ref_pass CardPasses[2];
extern bool CardsBound; // a card the scene draws has a map bound as it is: both passes are made with SCENE_BUCKETS_BOUND
extern xr_hash_map<CTexture*, u32> AlphaSlices;
extern nri::Buffer* FeedbackCopies[FeedbackFrames];
constexpr u64 FeedbackBytes = SCENE_TEXTURE_STREAMS * sizeof(u32);
extern nri::Descriptor* SceneTableViews[4];
extern nri::Descriptor* ClusterIndexView;
void BuildAlpha(const xr_vector<GpuTextures::Ref>& Refs, const xr_vector<IRHISurface*>& Wholes);
void BuildEnv(const xr_vector<CTexture*>& Cubes);
void CreateCards(const xr_vector<Hlsl::SceneCard>& Records);

struct GrassSet
{
	IRHIBuffer* Source = nullptr; // the old backend uploads the instances
	nri::Buffer* Items = nullptr;
	Tracked Args, Visible;
	nri::Descriptor* Views[4] = {}; // instances; arguments and visible list written; visible list read
	// A frame its own: the GPU may run a frame's cull after the next frame has named its runs
	nri::DescriptorSet* Sets[FrameCount] = {};
	nri::DescriptorSet* DrawSet = nullptr; // the scene's draw layout with the instances and the visible list
	u32 Capacity = 0;
	u32 Frame = u32(-1);
	u32 Uploaded = 0; // instances of the frame the buffer has: a shadow view has fewer than the main view
	// Where the instances are this frame: a grid over them, each cell with its box and the models in it
	Fbox Cells[GrassCells * GrassCells];
	std::bitset<SCENE_GRASS_BINS> CellBins[GrassCells * GrassCells];
	u32 CellFrame = u32(-1);
};

extern GrassSet Grass;
static_assert(GrassSlotStore::Bits == SCENE_GRASS_STORE_BITS);
extern nri::Buffer* GrassRunBuffers[FrameCount];
extern nri::Descriptor* GrassRunViews[FrameCount];
extern u32 GrassRunCapacity;
extern u32 GrassRunFrame;
void ReleaseGrass(GrassSet& Set);
nri::Descriptor* MakeSliceView(nri::Texture* Texture, u32 Slice);
extern nri::PipelineLayout* GrassLayout;
extern nri::Pipeline* GrassCullPipeline;
extern nri::DescriptorPool* GrassPool;
extern nri::Descriptor* GrassBlock;
extern ref_cs GrassCullShader;
void ReleaseResolve();
// The heads of the streamed textures as the frame's forward draws read them
nri::Descriptor* ForwardHeadsView();
nri::Descriptor* ForwardLightsView(); // the local lights of the frame (CRender::UploadSurfaceLights)
extern nri::Descriptor* VertexPageViews[PageSlots];
extern nri::Buffer* IndexPageBuffers[PageSlots];
extern nri::Descriptor* IndexPageViews[PageSlots];
extern IRHIBuffer* PageSources[PageSlots];
extern nri::DescriptorPool* GrassDrawPool;
extern ref_vs GrassVertex;
extern ref_ps GrassPixels;

struct GrassPipeline
{
	nri::Format Format;
	bool IsSpot;
	nri::Pipeline* Pipeline;
};

extern xr_vector<GrassPipeline> GrassPipelines;
extern bool HasAlpha;
void ViewSize(u32& Width, u32& Height);

// One draw of the forward frame: the clusters whose materials share the shaders and the state of its passes
struct SceneBin
{
	ref_pass Depth; // none when the frame draws no depth first
	ref_pass Colour;
	u32 Start;
	u32 Count;
	u32 Order; // when it is drawn: 0 opaque, 1 terrain, 2 cut out by its texture; the grass is 3; 4 blended, after the sky
	nri::Pipeline* DepthPipelines[4] = {}; // by the number of colour targets
	nri::Pipeline* ColourPipeline = nullptr;
	nri::Pipeline* OverdrawPipelines[2] = {}; // DebugView_OverdrawRaster, DebugView_OverdrawShaded
	ref_shader Source; // a material the passes are of, held: a reset makes the grass models and their shaders again
	ref_pass Face; // none for a material that is not drawn into the cube around the eye
	nri::Pipeline* FacePipeline = nullptr;
	u32 Variant = ShaderVariant::Cluster; // the element of Source the passes are of
	bool IsBound = false; // its materials have a map bound as it is: its passes are the element's BoundPass
	bool IsEmissive = false; // its materials give off light by a map: no frame that lights a surface later has room for it
	// Of a model's bin in a frame that lights its surfaces later: the depth pass that keeps the surface, the colour pass
	// of its HUD parts alone
	ref_ps SurfacePixel;
	ref_ps HudPixel;
	nri::Pipeline* SurfacePipeline = nullptr;
	nri::Pipeline* HudPipeline = nullptr;
};

extern xr_vector<SceneBin> Bins;

// A face of the cube around the eye keeps what its cull wrote, in a part of the lists of its own after the main
// view's two, while the eye stays within r_reflections_cull_reach of where it was culled and sees the same sectors. The cull
// takes everything that near to the face's frustum, so the list holds for any eye inside the reach
constexpr u32 FaceParts = 6;
extern bool IsFaceCulled[FaceParts];
extern xr_vector<ref_texture> DroppedTextures;
extern SceneBin GrassPass;

// What a draw of a detail model needs, by the model's first cluster: its indices in GrassIndices, its vertices, its
// material
struct GrassDraw
{
	u32 IndexStart;
	u32 Vertices;
	u32 Material;
};

extern xr_hash_map<u32, GrassDraw> GrassDraws;
extern nri::Buffer* GrassIndices;
extern Hlsl::SceneGrassView GrassForwardBlock;
extern u32 GrassForwardFrame;
void ReleaseForward();
extern xr_vector<nri::Descriptor*> Views;
extern xr_vector<nri::Buffer*> Borrowed;
extern ref_vs DepthShaders[4];
extern ref_ps AlphaPixels;
extern ref_vs WaveShader, WaveAlphaShader;
extern u32 ViewOffset;
extern u32 ForwardViewOffset;
extern u32 TileCount;
extern SState* DepthState;
void Require(nri::Result Result, const char* Failure);
nri::Buffer* CreateBuffer(u64 Size, u32 Stride, nri::BufferUsageBits Usage, const char* Name);
nri::Buffer* CreateTable(const void* Data, size_t Bytes, u32 Stride, const char* Name);

template <class T>
nri::Buffer* CreateTable(const xr_vector<T>& Records, const char* Name)
{
	return CreateTable(Records.data(), Records.size() * sizeof(T), sizeof(T), Name);
}

nri::Buffer* WrapBuffer(IRHIBuffer* Source, u32 Stride);
nri::Descriptor* MakeView(nri::Buffer* Buffer, nri::BufferView Type, nri::Format Format = nri::Format::UNKNOWN, u32 Stride = 0);
nri::PipelineLayout* CreateLayout(nri::StageBits Stages, const nri::DescriptorRangeDesc* Ranges, u32 RangeCount, u32 Anisotropy = 1);
nri::ShaderDesc ShaderCode(RHIObject* Shader, nri::StageBits Stage);
void SetBlocks(nri::CommandBuffer& Commands, u32 View = ViewOffset);
void Move(nri::CommandBuffer& Commands, Tracked& Item, nri::AccessBits Access, nri::StageBits Stages);
void WrapPages();
u32 SplitLeftovers(xr_vector<xr_vector<dxRender_Visual*>> (&Lists)[3]);
bool IsModelVertex(const SPass* Pass);
bool IsModelShader(const Shader& Material);

// Why the main view does not draw a level visual of the material from the clusters, Batched when it does. A model's
// cluster pass reads model records, so a level visual of a model shader stays in the graph
u32 EnvOf(const Shader& Material);
// Variant: the element whose passes the bin draws, ShaderVariant::ClusterModel for a level shader on a moving object
u32 FindBin(Shader* Material, const Hlsl::SceneMaterial& Record, size_t First, u32 Variant = ShaderVariant::Cluster);

// The maps of SceneMaterial, in its order
const char* const MapNames[SCENE_MATERIAL_MAPS] = { "s_base", "s_bump", "s_bumpX", "s_detail", "s_detailBump", "s_detailBumpX", "s_hemi", "s_specular", "s_lmap",
	"s_mask", "s_dt_r", "s_dt_g", "s_dt_b", "s_dt_a", "s_dn_r", "s_dn_g", "s_dn_b", "s_dn_a", "s_dn_rX", "s_dn_gX", "s_dn_bX", "s_dn_aX", "s_dt_spec_r", "s_dt_spec_g", "s_dt_spec_b", "s_dt_spec_a" };

Hlsl::SceneMaterial EmptyMaterial();
const SPass* MaterialPass(const Shader* Material);
void ReadMaterial(const SPass& Pass, Hlsl::SceneMaterial& Record, CTexture* (&Maps)[SCENE_MATERIAL_MAPS], bool IsCard = false);
CTexture* MirroredCube(const Shader& Material);
bool IsBatchState(const SPass& Pass);
bool IsNamed(const shared_str& Name, const char* Prefix);
void MakeCardPasses();
nri::Buffer& ModelUpload(u32 Frame);

struct MeshKey
{
	u32 Page;
	u32 VertexBase;
	u32 IndexStart;
	u32 IndexCount;

	bool operator==(const MeshKey&) const = default;
};

struct Caster
{
	dxRender_Visual* Visual;
	const IRender_Mesh* Mesh;
	u32 Instance;
	u32 Transform;
	const Fmatrix* World;
	bool IsWaving;
	bool IsAlpha;
	CTexture* Base;
	u32 Views; // SCENE_CLUSTER_MAIN, SCENE_CLUSTER_DEPTH
	const FSlideWindow* Window; // of a progressive mesh: its most detailed level, null for the whole index data
	const SPass* Pending; // of a caster of the shadows alone, see ClassifyCaster
	bool IsSorted = false; // a blended visual the sorted list draws from its clusters: its mesh as built, not welded
};

// Why a surface of the frame's sorted list stays in the graph; for a level visual decided at load
enum class SortedLeft : u32
{
	Taken,
	Moving,
	Level,
	Pass,
	Incomplete,
	Env,
	Mesh,
	Weighed,
	Caps,
	Count
};

// A blended level visual the sorted list can draw: its clusters, which no cull takes
struct SortedStatic
{
	u32 First = 0;
	u32 Count = 0;
	SortedLeft Why = SortedLeft::Mesh;
	bool IsBound = false;
};

extern xr_hash_map<const dxRender_Visual*, SortedStatic> SortedStatics;
extern xr_vector<u32> SortedStreams; // the streamed textures of their materials: always asked for
constexpr u32 SortedEntries = 1u << 16; // of the visible list past the cards a frame: the clusters the list draws
extern u32 SortedStart;

// A draw of the list: the clusters of neighbours of one pass, from Start of the visible list
struct SortedSegment
{
	const SPass* Pass;
	u32 Start;
	u32 Count;
};

extern xr_vector<SortedSegment> SortedSegments; // what DrawForward draws for Parts 32
// A texture of a pass's list the forward draws bind by its register: one its pixel stage reads in a range of plain
// textures; the buckets, the tables and the records are the scene's own
bool IsForwardRead(const SPass& Pass, u32 Stage);
// A register holds one texture in a forward draw: false when the pass binds another texture than Bound has at one,
// else its textures go into Bound
bool AddForwardReads(const SPass& Pass, const CTexture* (&Bound)[64]);
// Count for a visual whose material is not sorted; else why it stays in the graph, recorded for the visual
SortedLeft KeepSorted(const dxRender_Visual& Visual, const Hlsl::SceneMaterial& Record);
void ReleaseSorted();
void ReleaseSortedPipelines();
// A pass's pipeline for the sample count of its targets; made again when that changed
nri::Pipeline*& SortedPipeline(const SPass* Pass, u32 Samples);
// The entries of the segments into the visible list, before the pass draws them
void UploadSorted(nri::CommandBuffer& Commands);
// The counts of the frame before taken with the forward cull's, and printed with them
void CopySortedCounts();
void LogSortedCounts();

u32 BuildClusters(const xr_vector<Caster>& Casters, xr_vector<Hlsl::SceneInstance>& Instances, xr_vector<Hlsl::SceneCluster>& Records, xr_vector<Hlsl::SceneClusterLod>& Lods, xr_vector<u32>& Words);
} // namespace GpuSceneInternal

template <>
struct std::hash<MeshKey>
{
	size_t operator()(const MeshKey& Key) const noexcept
	{
		return size_t(crc32(&Key, sizeof(Key)));
	}
};

namespace GpuSceneInternal
{
u32 AddCasterPlanes(Hlsl::float4* Planes, u32 Count, const Fvector& Look);
bool WrapSkin();
CullBlocks StreamCull(const Hlsl::SceneView& Block);
void RecordCull(nri::CommandBuffer& Commands, const ViewLists& Lists, const Hlsl::SceneView& Block, const CullBlocks& Blocks);
} // namespace GpuSceneInternal

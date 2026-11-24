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
#include "stdafx.h"
#include "GpuSceneInternal.h"

namespace GpuSceneInternal
{
nri::CoreInterface Api;
nri::MeshShaderInterface MeshApi;
nri::PipelineLayout* MeshLayout = nullptr;
nri::HelperInterface Help;
nri::StreamerInterface StreamApi;

nri::PipelineLayout* CullLayout = nullptr;
nri::PipelineLayout* DrawLayout = nullptr;
nri::Pipeline* CullPipeline = nullptr;
xr_vector<DepthPipelines> Pipelines;
nri::DescriptorPool* Pool = nullptr;
nri::DescriptorSet* CullSet = nullptr;
nri::DescriptorSet* DrawSet = nullptr;
nri::DescriptorSet* AlphaSet = nullptr;
nri::Streamer* Streamer = nullptr;
nri::Descriptor* ViewBlock = nullptr;
// The skinning pass's view of its output the sets hold: it makes another when the output grows
nri::Descriptor* SkinView = nullptr;
nri::Descriptor* SkinFiller = nullptr;
nri::Buffer* Clusters = nullptr;
nri::Buffer* Materials = nullptr;
nri::Buffer* TreeLights = nullptr;
nri::Buffer* ClusterLods = nullptr;
nri::Descriptor* ClusterLodView = nullptr;
nri::Descriptor* TreeLightView = nullptr;
Tracked Counters, Visible, VisibleAlpha;
// The cull's first level: the runs of clusters, the chunks of the runs a view may see, the groups of its second level
nri::Buffer* Ranges = nullptr;
Tracked Chunks, CullGroups;
// What a cluster lays of its texture over a metre, measured once on the GPU: the cull asks for texture heads by it
Tracked Density;
bool IsDensityBuilt = false;
nri::PipelineLayout* DensityLayout = nullptr;
nri::Pipeline* DensityPipeline = nullptr;
nri::DescriptorSet* DensitySet = nullptr;
xr_vector<u32> GrassStreams; // the streamed textures of the grass: asked for while there is grass
u32 RangeCount = 0;
nri::PipelineLayout* RangeLayout = nullptr;
nri::Pipeline* RangePipeline = nullptr;
nri::DescriptorSet* RangeSet = nullptr;

ViewLists SharedLists()
{
	return { Counters, Visible, VisibleAlpha, Chunks, CullGroups, CullSet, RangeSet, DrawSet, AlphaSet, Pool };
}

// A sun cascade or a page of the lamps' shadows of the forward frame: lists and sets of its own made at load, so that
// the views can be recorded at the same time, and the batches the primary thread kept for this frame's recording
nri::DescriptorPool* OwnPool = nullptr;

OwnView OwnViews[GpuScene::OwnViewCount];
u32 KeptView = GpuScene::OwnViewCount;

// The main view's memory of what it showed, and the depth pyramid its second part is tested against
Tracked Seen;
bool IsSeenCleared = false;
nri::Descriptor* HizRead = nullptr;
nri::PipelineLayout* HizLayout = nullptr;
nri::Pipeline* HizPipelines[2] = {}; // of a single-sampled depth, of a multisampled one
u32 HizWidth = 0;
u32 HizHeight = 0;
ref_cs HizShaders[2];
// The forward cull's counts on request: asked, then copied in a frame and read a few frames later
nri::Buffer* CullStatsCopy = nullptr;
u32 CullStatsStep = 0;
GpuTextures Textures;
nri::Texture* Alpha = nullptr;
nri::Descriptor* AlphaView = nullptr;

// What the cull writes about the texture heads it needed, and the copy the CPU reads frames later
Tracked Feedback;
// The frame's table of moving objects (BuildObjects, CollectCasters): written a frame into the upload buffer of its slot and copied by the cull
// into Models, which the cull and the draws read. Their bins follow the level's and hold no level clusters
nri::Buffer* ModelUploads[FrameCount] = {};
Tracked Models;
nri::Descriptor* ModelView = nullptr;
xr_hash_map<Shader*, ModelMaterial> ModelBinIds;
xr_vector<ref_shader> ModelShaders; // held: a bin is found by the address of its shader
u32 LevelBinCount = 0;
xr_hash_map<Shader*, ModelLeft> ModelRejects; // the shaders that could not join, held in ModelShaders too
// Every material record by its shader from the load on; the models made later append theirs, up to MaterialCapacity
xr_hash_map<Shader*, u32> MaterialIds;
u32 MaterialCapacity = 0;
size_t BinLimit = 0; // the bins the counters have room for
xr_hash_map<CTexture*, GpuTextures::Ref> TextureRefs; // where the textures of the materials are in the buckets
xr_hash_map<CTexture*, u32> EnvSlots;
nri::Descriptor* EnvArrayView = nullptr;
xr_vector<Hlsl::SceneModel> ModelRecords;
xr_vector<ModelPlace> ModelWorlds[2];
u32 ModelFrame = u32(-1);
u32 ModelRuns = 0;
u32 ModelsLeft[2][u32(ModelLeft::Count)] = {}; // of the world, of the HUD; the HUD's taken too
// The mesh parts left as not of a model shader, by the vertex shader of their pass: this frame, the frame copied
xr_vector<std::pair<shared_str, u32>> ShadersLeft, CullStatsShaders;
// The records Models and its uploads hold: the frame's table of SCENE_MODELS, which every shadow view casts from
u32 ModelCapacity = 0;
CasterCounts CascadeCounts[SCENE_CASTER_VIEWS];
KindCounts KindsLeft[u32(ShadowKind::Count)];
string1024 MainLeftLine = {}; // the load's count, printed again with the forward cull's
u32 MainLeftovers = 0;
u32 ShadowLeftovers[2] = {};
u32 MainPackets = 0; // static packets the main view's graph holds this frame, and those of the frame whose counts were copied
// The level's LOD cards the scene draws: their table, the cull that keeps those the main view shows into the visible list
// past everything else (CardStart, SCENE_CARD_WORDS a card), and the passes of the card material compiled for the scene,
// cut out (the graph's ModelLighting) and fading in (NormalLQ). A card the scene cannot draw stays in the graph's mapLOD
u32 CardCount = 0;
u32 CardStart = 0;
u32 CardFrame = u32(-1); // the frame the cards were culled in
nri::Buffer* CardTable = nullptr;
Tracked CardArgs;
nri::PipelineLayout* CardLayout = nullptr;
nri::Pipeline* CardCull = nullptr;
nri::DescriptorPool* CardPool = nullptr;
nri::DescriptorSet* CardSet = nullptr;
ref_shader CardSource;
ref_pass CardPasses[2];
bool CardsBound = false;
xr_hash_map<const SPass*, std::pair<ref_pass, ref_pass>> BoundPasses;
// The slice of the alpha array of each alpha-tested texture of the level
xr_hash_map<CTexture*, u32> AlphaSlices;
nri::Buffer* FeedbackCopies[FeedbackFrames] = {};

// Clusters, transforms, instances and materials, as the draws bind them
nri::Descriptor* SceneTableViews[4] = {};
nri::Descriptor* ClusterIndexView = nullptr;
GrassSet Grass;
// The frame's runs of stored blades, as the grass cull reads them
nri::Buffer* GrassRunBuffers[FrameCount] = {};
nri::Descriptor* GrassRunViews[FrameCount] = {};
u32 GrassRunCapacity = 0;
u32 GrassRunFrame = u32(-1);
nri::PipelineLayout* GrassLayout = nullptr;
nri::Pipeline* GrassCullPipeline = nullptr;
nri::DescriptorPool* GrassPool = nullptr;
nri::Descriptor* GrassBlock = nullptr;
ref_cs GrassCullShader;

nri::Descriptor* VertexPageViews[PageSlots] = {};
nri::Buffer* IndexPageBuffers[PageSlots] = {};
nri::Descriptor* IndexPageViews[PageSlots] = {};
IRHIBuffer* PageSources[PageSlots] = {}; // the vertex buffers of the pages wrapped

nri::DescriptorPool* GrassDrawPool = nullptr;
// scene_depth compiled for grass, depth only
ref_vs GrassVertex;
ref_ps GrassPixels;
xr_vector<GrassPipeline> GrassPipelines;
bool HasAlpha = false;

// The part of a surface the frame draws into: the view, when the surface is as large as the scene's depth
void ViewSize(u32& Width, u32& Height)
{
	if (Width == u32(AllocHalfTarget.x) && Height == u32(AllocHalfTarget.y))
	{
		Width = u32(HalfTarget.x);
		Height = u32(HalfTarget.y);
	}
}
xr_vector<SceneBin> Bins;
bool IsFaceCulled[FaceParts] = {};
// The textures whose whole copies the forward frame let go at load: the level's shaders hold them until it unloads
xr_vector<ref_texture> DroppedTextures;
// The grass of the forward frame: the passes of the detail models' material, and what this frame's cull was given
SceneBin GrassPass;
xr_hash_map<u32, GrassDraw> GrassDraws;
nri::Buffer* GrassIndices = nullptr;
Hlsl::SceneGrassView GrassForwardBlock;
u32 GrassForwardFrame = u32(-1);
xr_vector<nri::Descriptor*> Views;
xr_vector<nri::Buffer*> Borrowed;
ref_vs DepthShaders[4]; // by IsAlpha | IsTiled << 1
ref_ps AlphaPixels;
ref_vs WaveShader, WaveAlphaShader;
u32 ViewOffset = 0;
u32 ForwardViewOffset = 0; // the view block of the main view's first part: the grass cull reads the pyramid by it
u32 TileCount = 0;
SState* DepthState = nullptr;

// What the scene cannot go on without. A removed device is what fails these: the flush finds it and reports why
void Require(nri::Result Result, const char* Failure)
{
	if (Result != nri::Result::SUCCESS)
	{
		GRHI->DevicePtr->Flush();
		CHECK_OR_EXIT(false, Failure);
	}
}

nri::Buffer* CreateBuffer(u64 Size, u32 Stride, nri::BufferUsageBits Usage, const char* Name)
{
	nri::BufferDesc Desc = {};
	Desc.size = Size;
	Desc.structureStride = Stride;
	Desc.usage = Usage;

	nri::Buffer* Buffer = nullptr;
	Require(Api.CreateCommittedBuffer(*GRHI->Nri->Device, nri::MemoryLocation::DEVICE, 0.0f, Desc, Buffer), "The graphics device could not create a scene buffer.");

	Api.SetDebugName(Buffer, Name);
	return Buffer;
}

// Stride 0 makes a buffer that is read through a typed view
nri::Buffer* CreateTable(const void* Data, size_t Bytes, u32 Stride, const char* Name)
{
	nri::Buffer* Buffer = CreateBuffer(std::max<u64>(Bytes, std::max(Stride, 4u)), Stride, nri::BufferUsageBits::SHADER_RESOURCE, Name);

	if (Bytes)
	{
		nri::BufferUploadDesc Upload = {};
		Upload.data = Data;
		Upload.buffer = Buffer;
		Upload.after.access = nri::AccessBits::SHADER_RESOURCE;
		Help.UploadData(*GRHI->Nri->Queue, nullptr, 0, &Upload, 1);
	}

	return Buffer;
}

nri::Buffer* WrapBuffer(IRHIBuffer* Source, u32 Stride)
{
	void* Native = GRHI->DevicePtr->ReadExternalBuffer(Source);

	nri::Buffer* Buffer = nullptr;

	if (GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		nri::WrapperD3D12Interface Wrapper;
		nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::WrapperD3D12Interface), &Wrapper);
		nri::BufferD3D12Desc Desc = {};
		Desc.d3d12Resource = static_cast<ID3D12Resource*>(Native);
		Desc.structureStride = Stride;
		Wrapper.CreateBufferD3D12(*GRHI->Nri->Device, Desc, Buffer);
	}
	else
	{
		nri::WrapperD3D11Interface Wrapper;
		nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::WrapperD3D11Interface), &Wrapper);
		nri::BufferD3D11Desc Desc = {};
		Desc.d3d11Resource = static_cast<ID3D11Resource*>(Native);
		Wrapper.CreateBufferD3D11(*GRHI->Nri->Device, Desc, Buffer);
	}

	Borrowed.push_back(Buffer);
	return Buffer;
}

nri::Descriptor* MakeView(nri::Buffer* Buffer, nri::BufferView Type, nri::Format Format, u32 Stride)
{
	nri::BufferViewDesc Desc = {};
	Desc.buffer = Buffer;
	Desc.type = Type;
	Desc.format = Format;
	Desc.structureStride = Stride;

	nri::Descriptor* View = nullptr;
	Api.CreateBufferView(Desc, View);
	Views.push_back(View);
	return View;
}

nri::PipelineLayout* CreateLayout(nri::StageBits Stages, const nri::DescriptorRangeDesc* Ranges, u32 RangeCount, u32 Anisotropy)
{
	nri::RootDescriptorDesc Roots[2] = {};
	for (u32 Root = 0; Root < 2; ++Root)
	{
		Roots[Root].registerIndex = Root;
		Roots[Root].descriptorType = nri::DescriptorType::CONSTANT_BUFFER;
		Roots[Root].shaderStages = Stages;
	}

	nri::RootSamplerDesc Sampler = {};
	Sampler.desc.filters = { nri::Filter::LINEAR, nri::Filter::LINEAR, nri::Filter::LINEAR };
	Sampler.desc.anisotropy = u8(Anisotropy);
	Sampler.desc.mipMax = 16.0f;
	Sampler.desc.addressModes = { nri::AddressMode::REPEAT, nri::AddressMode::REPEAT, nri::AddressMode::REPEAT };
	Sampler.shaderStages = nri::StageBits(u32(Stages) & u32(nri::StageBits::FRAGMENT_SHADER | nri::StageBits::COMPUTE_SHADER));

	nri::DescriptorSetDesc SetDesc = {};
	SetDesc.ranges = Ranges;
	SetDesc.rangeNum = RangeCount;

	nri::PipelineLayoutDesc Desc = {};
	Desc.rootDescriptors = Roots;
	Desc.rootDescriptorNum = 2;
	Desc.rootSamplers = &Sampler;
	Desc.rootSamplerNum = u32(Sampler.shaderStages) != 0;
	Desc.descriptorSets = &SetDesc;
	Desc.descriptorSetNum = 1;
	Desc.shaderStages = Stages;

	nri::PipelineLayout* Layout = nullptr;
	Api.CreatePipelineLayout(*GRHI->Nri->Device, Desc, Layout);
	return Layout;
}

nri::ShaderDesc ShaderCode(RHIObject* Shader, nri::StageBits Stage)
{
	const void* Code = nullptr;
	size_t Size = 0;
	const bool HasCode = GRHI->DevicePtr->GetShaderCode(Shader, Code, Size);
	R_ASSERT(HasCode);

	nri::ShaderDesc Desc = {};
	Desc.stage = Stage;
	Desc.bytecode = Code;
	Desc.size = Size;
	return Desc;
}

// The view block is all the cull and the draws read: it stands in both places of the layout
void SetBlocks(nri::CommandBuffer& Commands, u32 View)
{
	nri::SetRootDescriptorDesc Root = {};
	Root.descriptor = ViewBlock;
	Root.offset = View;
	Api.CmdSetRootDescriptor(Commands, Root);
	Root.rootDescriptorIndex = 1;
	Api.CmdSetRootDescriptor(Commands, Root);
}

void Move(nri::CommandBuffer& Commands, Tracked& Item, nri::AccessBits Access, nri::StageBits Stages)
{
	nri::BufferBarrierDesc Barrier = {};
	Barrier.buffer = Item.Buffer;
	Barrier.before = Item.State;
	Item.State = { Access, Stages };
	Barrier.after = Item.State;

	nri::BarrierDesc Desc = {};
	Desc.buffers = &Barrier;
	Desc.bufferNum = 1;
	Api.CmdBarrier(Commands, Desc);
}

// The heap's pages as the sets read them. A page the heap opens after the load is wrapped at the end of a frame: the
// long-lived sets take it in the slot a filler held, which no draw read, as nothing on a page is drawn by the scene
// before it is wrapped; the forward set of the next frame is written with it
void WrapPages()
{
	bool IsNew = false;

	for (u32 Page = 0; Page < RImplementation.Geometry.PageCount(); ++Page)
	{
		IRHIBuffer* Source = RImplementation.Geometry.VertexBuffer(Page);

		if (Source && Source != PageSources[Page])
		{
			PageSources[Page] = Source;
			VertexPageViews[Page] = MakeView(WrapBuffer(Source, VertexStride), nri::BufferView::STRUCTURED_BUFFER, nri::Format::UNKNOWN, VertexStride);
			IndexPageBuffers[Page] = WrapBuffer(RImplementation.Geometry.IndexBuffer(Page), 0);
			IndexPageViews[Page] = MakeView(IndexPageBuffers[Page], nri::BufferView::BUFFER, nri::Format::R16_UINT);
			IsNew = true;
		}
	}

	if (!IsNew || !DrawSet)
	{
		return;
	}

	// vertex.hlsli counts its pages down from the last register
	nri::Descriptor* Reversed[PageSlots];

	for (u32 Page = 0; Page < PageSlots; ++Page)
	{
		Reversed[Page] = VertexPageViews[PageSlots - 1 - Page];
	}

	const nri::UpdateDescriptorRangeDesc Updates[6] = { { DrawSet, 0, 4, VertexPageViews, PageSlots }, { DrawSet, 1, 2, IndexPageViews, PageSlots }, { AlphaSet, 0, 4, VertexPageViews, PageSlots },
		{ AlphaSet, 1, 2, IndexPageViews, PageSlots }, { DensitySet, 2, 0, Reversed, PageSlots }, { Grass.DrawSet, 0, 4, VertexPageViews, PageSlots } };
	Api.UpdateDescriptorRanges(Updates, Grass.DrawSet ? 6 : 5);

	for (const OwnView& Own : OwnViews)
	{
		if (Own.DrawSet)
		{
			const nri::UpdateDescriptorRangeDesc Pages[4] = { { Own.DrawSet, 0, 4, VertexPageViews, PageSlots }, { Own.DrawSet, 1, 2, IndexPageViews, PageSlots }, { Own.AlphaSet, 0, 4, VertexPageViews, PageSlots },
				{ Own.AlphaSet, 1, 2, IndexPageViews, PageSlots } };
			Api.UpdateDescriptorRanges(Pages, 4);
		}
	}

	Msg("* [scene] geometry page opened after the load: %u pages wrapped", RImplementation.Geometry.PageCount());
}

namespace
{
// Whether a level visual's shadow element puts anything into a view of the first pass alone (the sun cascades, the rain
// map): a pass of priority 0 or 1, or the element sorted back to front at that priority
bool HasFirstPass(const dxRender_Visual& Visual)
{
	// A group picks its meshes by distance as a view walks it
	if (Visual.Type == MT_MESH_LODS)
	{
		return true;
	}

	const ShaderElement* Element = Visual.shader._get() ? Visual.shader->E[ShaderVariant::Shadow]._get() : nullptr;
	if (!Element)
	{
		return false;
	}

	const auto IsFirst = [Element](const ref_pass& Pass) { return Pass && (Pass->iPriority != u8(-1) ? Pass->iPriority : Element->flags.iPriority) / 2 == 0; };
	return (Element->flags.bStrictB2F && Element->flags.iPriority / 2 == 0) || std::any_of(Element->passes.begin(), Element->passes.end(), IsFirst);
}
} // namespace

// The leftovers a view walks, by the shadow elements the materials have now, out of all of them (2): 0 those with one,
// 1 those that put anything into a view of the first pass alone; how many of these. ShadowLeftovers counts both
u32 SplitLeftovers(xr_vector<xr_vector<dxRender_Visual*>> (&Lists)[3])
{
	const xr_vector<xr_vector<dxRender_Visual*>>& All = Lists[2];
	Lists[0].assign(All.size(), xr_vector<dxRender_Visual*>());
	Lists[1].assign(All.size(), xr_vector<dxRender_Visual*>());
	ShadowLeftovers[0] = ShadowLeftovers[1] = 0;

	for (size_t Sector = 0; Sector < All.size(); ++Sector)
	{
		for (dxRender_Visual* Visual : All[Sector])
		{
			if (Visual->Type == MT_MESH_LODS || (Visual->shader._get() && Visual->shader->E[ShaderVariant::Shadow]._get()))
			{
				Lists[0][Sector].push_back(Visual);
				++ShadowLeftovers[0];
			}

			if (HasFirstPass(*Visual))
			{
				Lists[1][Sector].push_back(Visual);
				++ShadowLeftovers[1];
			}
		}
	}

	return ShadowLeftovers[1];
}

// The model blenders name their vertex shaders model_*, the XML blends r1_model_*, deffer_model, models_*: no level
// surface's has the word
bool IsModelVertex(const SPass* Pass)
{
	return Pass && Pass->vs && Pass->vs->cName.size() && strstr(*Pass->vs->cName, "model");
}

// A material of moving objects the forward frame can draw from the scene: a model's, with a cluster element
bool IsModelShader(const Shader& Material)
{
	const ShaderElement* Element = Material.E[ShaderVariant::Cluster]._get();
	return IsModelVertex(Element && !Element->passes.empty() ? Element->passes.back()._get() : nullptr);
}

bool HasBoundMap(const Hlsl::SceneMaterial& Record)
{
	for (u32 Map = 0; Map < SCENE_MATERIAL_MAPS; ++Map)
	{
		const u32 Tail = (&Record.base)[Map * 4];

		if (Tail != GpuTextures::Absent && (Tail >> 16) >= SCENE_TEXTURE_BUCKETS)
		{
			return true;
		}
	}

	return false;
}

const ref_pass& BoundPass(const SPass* Pass)
{
	auto [Found, IsNew] = BoundPasses.try_emplace(Pass);

	if (!IsNew)
	{
		return Found->second.second;
	}

	Found->second.first._set(const_cast<SPass*>(Pass));
	Found->second.second = Found->second.first;
	const ref_ps Pixels = Pass && Pass->ps ? DEV->CreatePSWith(*Pass->ps, "SCENE_BUCKETS_BOUND") : ref_ps();

	if (Pixels)
	{
		SPass Proto;
		Proto.state = Pass->state;
		Proto.ps = Pixels;
		Proto.vs = Pass->vs;
		Proto.gs = Pass->gs;
		Proto.hs = Pass->hs;
		Proto.ds = Pass->ds;
		Proto.cs = Pass->cs;
		Proto.constants = Pass->constants;
		Proto.T = Pass->T;
		Proto.iPriority = Pass->iPriority;
		Proto.Features = Pass->Features;
		Proto.M = Pass->M;
		Found->second.second = DEV->_CreatePass(Proto);
	}

	return Found->second.second;
}

// The bin of a material's draw, from the bin First on: made when none has its passes and the counters have room. A
// material with a map bound as it is draws with the BoundPass of its passes, in a bin of its own beside the others'
u32 FindBin(Shader* Material, const Hlsl::SceneMaterial& Record, size_t First, u32 Variant)
{
	const auto IsSame = [](const SPass* Left, const SPass* Right)
	{
		return Left == Right || (Left && Right && Left->vs._get() == Right->vs._get() && Left->ps._get() == Right->ps._get() && Left->state._get() == Right->state._get());
	};

	const ShaderElement* Element = Material ? Material->E[Variant]._get() : nullptr;
	if (!(Record.flags & SCENE_MATERIAL_COMPLETE) || !Element || Element->passes.empty() || !Element->passes.back())
	{
		return SCENE_MATERIAL_NO_PIPELINE;
	}

	const bool IsBound = HasBoundMap(Record);
	const ref_pass& Own = Element->passes.back();
	const ref_pass Colour = IsBound ? BoundPass(Own._get()) : Own;
	const ref_pass Depth = Element->passes.size() < 2 ? ref_pass() : IsBound ? BoundPass(Element->passes[0]._get()) : Element->passes[0];
	const auto Found = std::find_if(Bins.begin() + First, Bins.end(), [&](const SceneBin& Bin) { return IsSame(Bin.Colour._get(), Colour._get()) && IsSame(Bin.Depth._get(), Depth._get()); });
	const u32 Index = u32(Found - Bins.begin());

	if (Found == Bins.end() && Bins.size() >= BinLimit)
	{
		return SCENE_MATERIAL_NO_PIPELINE;
	}

	if (Found == Bins.end())
	{
		// What hides comes first: a cut out pixel runs its shader wherever nothing is in front of it yet. What blends is
		// drawn over everything opaque, after the sky (Parts 64)
		const GfxPipelineState& State = *Colour->state->state;
		Bins.push_back({ Depth, Colour, 0, 0, State.Blend.RenderTarget[0].BlendEnable ? 4u : Element->flags.bLandscape ? 1u : State.AlphaRef ? 2u : 0u });
		Bins.back().Source._set(Material);
		Bins.back().Variant = Variant;
		Bins.back().IsBound = IsBound;
		// What gives off light has its map in the place of a terrain's mask
		Bins.back().IsEmissive = ((Record.flags & SCENE_MATERIAL_TERRAIN) && !Element->flags.bLandscape) || (Colour->Features & MaterialFeature::Unlit);
		const ShaderElement* Face = Material->E[ShaderVariant::ClusterFace]._get();
		const ref_pass FacePass = Face && !Face->passes.empty() && Variant == ShaderVariant::Cluster ? Face->passes[0] : ref_pass();
		Bins.back().Face = IsBound ? BoundPass(FacePass._get()) : FacePass;
		// The faces keep their lists where the count of bins put them
		std::fill(std::begin(IsFaceCulled), std::end(IsFaceCulled), false);
	}

	return Index;
}

Hlsl::SceneMaterial EmptyMaterial()
{
	Hlsl::SceneMaterial Empty = {};
	Empty.alpha = SCENE_TEXTURE_ABSENT;

	for (u32 Map = 0; Map < SCENE_MATERIAL_MAPS; ++Map)
	{
		(&Empty.base)[Map * 4] = SCENE_TEXTURE_ABSENT;
		(&Empty.base)[Map * 4 + 1] = SCENE_TEXTURE_ABSENT;
	}

	return Empty;
}

// The pass of a material that reads its maps, null for none: the forward frame draws depth first, then its colour
const SPass* MaterialPass(const Shader* Material)
{
	const ShaderElement* Element = Material ? Material->E[ShaderVariant::NormalHQ]._get() : nullptr;
	if (!Element || Element->passes.empty() || !Element->passes[0] || !Element->passes[0]->T)
	{
		return nullptr;
	}

	const SPass* Last = Element->passes.back()._get();
	return Last && Last->T ? Last : Element->passes[0]._get();
}

// What the pass was compiled with is what the record says of the material; Maps: the texture of each map, or null.
// IsCard: of an LOD card, whose views are bound by registers other surfaces bind other textures to
void ReadMaterial(const SPass& Pass, Hlsl::SceneMaterial& Record, CTexture* (&Maps)[SCENE_MATERIAL_MAPS], bool IsCard)
{
	static const struct
	{
		u32 Feature;
		u32 Flag;
	} FeatureFlags[] = {
		{ MaterialFeature::Pbr, SCENE_MATERIAL_PBR },
		{ MaterialFeature::Bump, SCENE_MATERIAL_BUMP },
		{ MaterialFeature::AlphaTest, SCENE_MATERIAL_AREF },
		{ MaterialFeature::Detail, SCENE_MATERIAL_DETAIL },
		{ MaterialFeature::DetailBump, SCENE_MATERIAL_DETAIL_BUMP },
		{ MaterialFeature::LightmapHemi, SCENE_MATERIAL_LM_HEMI },
		{ MaterialFeature::TreeWave, SCENE_MATERIAL_TREE },
		{ MaterialFeature::Layers, SCENE_MATERIAL_LAYERS },
		{ MaterialFeature::SteepParallax, SCENE_MATERIAL_STEEP },
		{ MaterialFeature::VertexLit, SCENE_MATERIAL_VERTEX_LIT },
		{ MaterialFeature::Placed, SCENE_MATERIAL_PLACED },
		{ MaterialFeature::Still, SCENE_MATERIAL_STILL },
	};
	static const xr_array<u32, SCENE_MATERIAL_MAPS> Stages = []()
	{
		xr_array<u32, SCENE_MATERIAL_MAPS> Made;

		for (u32 Map = 0; Map < SCENE_MATERIAL_MAPS; ++Map)
		{
			Made[Map] = CTexture::rstPixel + ShaderBindings::Texture(MapNames[Map]);
		}

		return Made;
	}();
	const u32 MaskStage = CTexture::rstPixel + ShaderBindings::Texture("s_mask");
	const u32 EnvStage = CTexture::rstPixel + ShaderBindings::Texture("s_env");
	// Maps of other names: what an emissive surface gives off takes the place of the mask (such a surface is no terrain),
	// the views of an LOD card and their hemi those of the base and the hemi
	static const std::pair<u32, u32> Aliases[] = { { CTexture::rstPixel + ShaderBindings::Texture("s_emission"), 9 }, { CTexture::rstPixel + ShaderBindings::Texture("s_base0"), 0 },
		{ CTexture::rstPixel + ShaderBindings::Texture("s_hemi0"), 6 } };
	const auto* AliasEnd = std::begin(Aliases) + (IsCard ? std::size(Aliases) : 1);

	for (const auto& Item : FeatureFlags)
	{
		Record.flags |= (Pass.Features & Item.Feature) ? Item.Flag : 0u;
	}

	Record.detail_scale = Pass.constants && Pass.constants->DetailScale ? *Pass.constants->DetailScale : 1.0f;
	std::fill(std::begin(Maps), std::end(Maps), nullptr);

	for (const auto& [Stage, Texture] : *Pass.T)
	{
		Record.flags |= Stage == MaskStage ? SCENE_MATERIAL_TERRAIN : 0u;
		const auto Alias = std::find_if(std::begin(Aliases), AliasEnd, [Stage](const std::pair<u32, u32>& Item) { return Item.first == Stage; });
		const auto Map = Alias != AliasEnd ? Stages.begin() + Alias->second : std::find(Stages.begin(), Stages.end(), Stage);
		const auto Env = Stage == EnvStage ? EnvSlots.find(Texture._get()) : EnvSlots.end();
		Record.env = Env != EnvSlots.end() ? Env->second : Record.env;

		if (Map != Stages.end() && Texture._get())
		{
			Maps[Map - Stages.begin()] = Texture._get();
		}
	}

	Record.gloss = Maps[0] ? Maps[0]->m_material : Record.gloss;
}

// The cube a material mirrors, null for none
CTexture* MirroredCube(const Shader& Material)
{
	const SPass* Pass = MaterialPass(&Material);
	const u32 EnvStage = CTexture::rstPixel + ShaderBindings::Texture("s_env");

	if (!Pass)
	{
		return nullptr;
	}

	for (const auto& [Stage, Texture] : *Pass->T)
	{
		if (Stage == EnvStage && Texture._get())
		{
			return Texture._get();
		}
	}

	return nullptr;
}

// Where the cube array holds what the material mirrors: 0 for none, NoEnv for a cube it does not hold
u32 EnvOf(const Shader& Material)
{
	CTexture* Cube = MirroredCube(Material);
	const auto Found = Cube ? EnvSlots.find(Cube) : EnvSlots.end();
	return !Cube ? 0u : Found != EnvSlots.end() ? Found->second : NoEnv;
}

// A shadow pass of a moving object that the batch's pipelines draw as it would: their cull, bias and depth test
bool IsBatchState(const SPass& Pass)
{
	const RHIRasterizerDesc& Raster = Pass.state->state->Raster;
	const RHIRasterizerDesc& Batch = DepthState->state->Raster;

	return Raster.CullMode == Batch.CullMode && Raster.FrontCounterClockwise == Batch.FrontCounterClockwise && Raster.DepthBias == Batch.DepthBias &&
		Raster.SlopeScaledDepthBias == Batch.SlopeScaledDepthBias && Raster.DepthClipEnable == Batch.DepthClipEnable &&
		Pass.state->state->Depth.DepthFunc == DepthState->state->Depth.DepthFunc;
}

bool IsNamed(const shared_str& Name, const char* Prefix)
{
	return Name.size() && !strncmp(*Name, Prefix, xr_strlen(Prefix));
}

// The card material's two passes as the scene draws them: its shaders compiled again with SURFACE_CLUSTER, its state.
// None when they are not the card shaders
void MakeCardPasses()
{
	for (u32 Kind = 0; Kind < 2; ++Kind)
	{
		const ShaderElement* Element = CardSource ? CardSource->E[Kind ? ShaderVariant::NormalLQ : ShaderVariant::ModelLighting]._get() : nullptr;
		const SPass* Pass = Element && Element->passes.size() == 1 ? Element->passes[0]._get() : nullptr;

		if (!Pass || !Pass->vs || !Pass->ps || !Pass->state || !IsNamed(Pass->vs->cName, "lod_static") || !IsNamed(Pass->ps->cName, "lod_static"))
		{
			CardPasses[0] = nullptr;
			CardPasses[1] = nullptr;
			return;
		}

		ShaderExternalMap Defines;
		Defines["SURFACE_CLUSTER"] = "1";

		if (Pass->state->state->Blend.AlphaToCoverageEnable)
		{
			Defines["USE_ALPHA_COVERAGE"] = "1";
		}

		SPass Proto;
		Proto.state = Pass->state;
		Proto.constants = Pass->constants;
		Proto.T = Pass->T;
		Proto.M = Pass->M;
		Proto.vs = DEV->_CreateVS("lod_static", Defines);
		Proto.ps = DEV->_CreatePS("lod_static", Defines);
		const ref_pass Made = DEV->_CreatePass(Proto);
		CardPasses[Kind] = CardsBound ? BoundPass(Made._get()) : Made;
	}
}
} // namespace GpuSceneInternal

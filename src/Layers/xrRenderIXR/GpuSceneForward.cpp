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
namespace
{
u32 CullStatsFrame = 0;
u32 CullStatsBlades = 0; // the blades the grass cull was given in the frame that was copied
const char* const ModelLeftNames[u32(ModelLeft::Count)] = { "taken", "no element for a moving object", "an environment cube outside the array", "joins later", "a map neither in the buckets nor bound", "no room for its material or draw", "emissive", "other element", "not a mesh", "heap page", "hidden part", "triangles", "frame caps", "second priority", "the scene takes no models" };
u32 CullStatsModels[3] = {}; // of the frame whose counts were copied: models taken, their runs, mesh parts left in the graph
u32 CullStatsLeft[2][u32(ModelLeft::Count)] = {};
u32 CullStatsBins = 0;
const char* const CasterLeftNames[u32(CasterLeft::Count)] = { "taken", "not a mesh", "other shadow shader", "other raster state", "cut out by a texture outside the alpha array", "heap page", "hidden part", "triangles", "caps" };
CasterCounts CullStatsCasters[SCENE_CASTER_VIEWS]; // of the frame whose counts were copied
CasterCounts CullStatsKinds[u32(ShadowKind::Count)];
u32 CullStatsMainPackets = 0;
nri::Pipeline* CardPipelines[2] = {};
u32 OverdrawFrame = u32(-1);

constexpr u32 ForwardBinRegister = 8; // SceneForwardBlock, in the place of a block the level's surfaces do not read
constexpr u32 ForwardHeadsRegister = 9; // SceneHeadsBlock, likewise
constexpr u32 ForwardGrassRegister = 10; // SceneGrassBlock, likewise
struct ForwardHeads
{
	u32 Frame[4];
};
u32 ForwardHeadsRoot = 0;
u32 ForwardMaterialRoot = 0; // CBMaterial: a pipeline's alpha reference
u32 ForwardGrassRoot = 0;
u32 ForwardSamples = 1; // of the targets the main view's pipelines were made for
// The id pass: a pipeline for what is cut out by its texture and what is not, of each cull mode
nri::Pipeline* IdPipelines[2][4] = {};
ref_vs IdVertex[2];
ref_ps IdPixel[2];

void ReleaseIdPipelines()
{
	for (nri::Pipeline*& Pipeline : std::span(&IdPipelines[0][0], 8))
	{
		if (Pipeline)
		{
			Api.DestroyPipeline(Pipeline);
			Pipeline = nullptr;
		}
	}

	for (u32 Cut = 0; Cut < 2; ++Cut)
	{
		IdVertex[Cut] = nullptr;
		IdPixel[Cut] = nullptr;
	}
}

// The pipelines of the main view; a face of the cube keeps its own, made for its single-sampled targets
void ReleaseViewPipelines(SceneBin& Bin)
{
	nri::Pipeline** const Made[] = { &Bin.DepthPipelines[0], &Bin.DepthPipelines[1], &Bin.DepthPipelines[2], &Bin.DepthPipelines[3], &Bin.ColourPipeline, &Bin.OverdrawPipelines[0], &Bin.OverdrawPipelines[1],
		&Bin.SurfacePipeline, &Bin.HudPipeline };

	for (nri::Pipeline** Pipeline : Made)
	{
		if (*Pipeline)
		{
			Api.DestroyPipeline(*Pipeline);
			*Pipeline = nullptr;
		}
	}

	// Made of the passes' pixel stages as they were: a stage compiled for other settings writes other targets
	Bin.SurfacePixel = nullptr;
	Bin.HudPixel = nullptr;
}
u32 ForwardStreamFrame = u32(-1);
// The pixel stage's registers: ranges of the set, in the order of the layout. Structured buffers stand alone
struct ForwardRange
{
	u32 First;
	u32 Count;
	bool IsRecords;
};
constexpr u32 ForwardFirstRange = 3;
constexpr u32 ForwardBucketRange = 4;
constexpr u32 ForwardMaterialRange = 5;
constexpr u32 ForwardBoundRange = 12;
// The vertex stage's first ranges: the tables, the frame's models and the skinned output; the lists and the index data
// of the heap's pages; the vertices of the pages. The textures bound as they are come past the registers a pass binds
constexpr ForwardRange ForwardRanges[] = { { 0, 8, true }, { 10, 3 + PageSlots, false }, { 64 - PageSlots, PageSlots, true }, { 0, 6, false }, { 6, SCENE_TEXTURE_BUCKETS, false },
	{ 6 + SCENE_TEXTURE_BUCKETS, 1, true }, { 23, 29, false }, { 52, 1, true }, { 53, 3, false }, { 56, 1, true }, { 57, 1, true }, { 58, 1, false }, { 64, SCENE_TEXTURE_BOUND, false } };
constexpr u32 ForwardSlots = 64 + SCENE_TEXTURE_BOUND;
// No range of it is written by a pixel: one output buffer in a pass makes the D3D11 backend of NRI bind its outputs
// again from slot 0, which takes the render targets away
constexpr u32 ForwardRecordKinds = 3;
constexpr u32 ForwardHeadRecords = 2;
// The local lights, the decals, the heads of the streamed textures. The heads are records and not a constant block:
// a pixel reads one word of 8 KB by index, which a block made many times slower
constexpr u32 ForwardRecordRanges[ForwardRecordKinds] = { 9, 7, 10 };
constexpr u32 ForwardRecordBytes[ForwardRecordKinds] = { 192 * 48, (SCENE_DECALS + 1) * sizeof(Hlsl::SceneDecal), SCENE_TEXTURE_STREAMS * sizeof(u32) };
constexpr u32 ForwardRecordStrides[ForwardRecordKinds] = { 48, sizeof(Hlsl::SceneDecal), sizeof(u32) };
nri::Buffer* ForwardRecordBuffers[ForwardRecordKinds][FrameCount] = {};
nri::Descriptor* ForwardRecordViews[ForwardRecordKinds][FrameCount] = {};
nri::PipelineLayout* ForwardLayout = nullptr;
// A frame's sets: a draw whose passes bind another texture to a register an earlier draw of the frame bound takes a set
// of its own, as D3D12 reads a set when the frame runs. Pools of ForwardPoolSets, as many as the frame needs
constexpr u32 ForwardPoolSets = 16;
nri::DescriptorPoolDesc ForwardPoolDesc = {};
xr_vector<nri::DescriptorPool*> ForwardPools[FrameCount];
u32 ForwardSetCount = 0;
nri::DescriptorSet* ForwardSet = nullptr;
nri::Descriptor* ForwardSetViews[ForwardSlots] = {}; // what ForwardSet holds in the pixel stage's plain ranges
u32 ForwardFrame = u32(-1);
xr_vector<nri::Descriptor*> ForwardBlockViews;
u32 ForwardBinRoot = 0;
xr_vector<u8> ForwardBinBlock;
nri::Descriptor* ForwardVisibleView = nullptr;

void ReleaseCardPipelines()
{
	for (nri::Pipeline*& Pipeline : CardPipelines)
	{
		if (Pipeline)
		{
			Api.DestroyPipeline(Pipeline);
			Pipeline = nullptr;
		}
	}
}
} // namespace

bool IsForwardRead(const SPass& Pass, u32 Stage)
{
	const u32 Slot = Stage - CTexture::rstPixel;

	if (Stage < CTexture::rstPixel || Slot >= 64 || !Pass.ps || !(Pass.ps->ReadSlots & (1ull << Slot)))
	{
		return false;
	}

	for (u32 Range = ForwardFirstRange; Range < std::size(ForwardRanges); ++Range)
	{
		const ForwardRange& Item = ForwardRanges[Range];

		if (Slot >= Item.First && Slot < Item.First + Item.Count)
		{
			return !Item.IsRecords && Range != ForwardBucketRange;
		}
	}

	return false;
}

bool AddForwardReads(const SPass& Pass, const CTexture* (&Bound)[64])
{
	if (!Pass.T || !Pass.ps)
	{
		return true;
	}

	Pass.ps->CompileState.WaitCompiled();

	for (const auto& [Stage, Texture] : *Pass.T)
	{
		if (Texture._get() && IsForwardRead(Pass, Stage) && Bound[Stage - CTexture::rstPixel] && Bound[Stage - CTexture::rstPixel] != Texture._get())
		{
			return false;
		}
	}

	for (const auto& [Stage, Texture] : *Pass.T)
	{
		if (Texture._get() && IsForwardRead(Pass, Stage))
		{
			Bound[Stage - CTexture::rstPixel] = Texture._get();
		}
	}

	return true;
}

xr_vector<ref_texture> BoundTextures;

GpuTextures::Ref BindTexture(CTexture* Texture, const char*& Why)
{
	const auto Found = std::find_if(BoundTextures.begin(), BoundTextures.end(), [Texture](const ref_texture& Item) { return Item._get() == Texture; });
	const u32 Index = u32(Found - BoundTextures.begin());
	Why = Index == SCENE_TEXTURE_BOUND ? "no bound register left" : "";

	if (Found == BoundTextures.end() && (Why[0] || !GpuTextures::IsPlain(*Texture, Why)))
	{
		return {};
	}

	if (Found == BoundTextures.end())
	{
		Texture->EnsureLoaded();
		BoundTextures.emplace_back()._set(Texture);
	}

	return { (SCENE_TEXTURE_BUCKETS + Index) << 16, GpuTextures::Absent, 0 };
}

void ReleaseForward()
{
	ReleaseCardPipelines();
	ReleaseSorted();
	CardPasses[0] = nullptr;
	CardPasses[1] = nullptr;
	CardSource = nullptr;
	CardsBound = false;
	BoundPasses.clear();

	if (CardTable)
	{
		Api.DestroyDescriptorPool(CardPool);
		Api.DestroyPipeline(CardCull);
		Api.DestroyPipelineLayout(CardLayout);
		Api.DestroyBuffer(CardArgs.Buffer);
		Api.DestroyBuffer(CardTable);
		CardTable = nullptr;
		CardArgs = {};
	}

	CardCount = 0;
	CardFrame = u32(-1);
	ReleaseIdPipelines();

	for (SceneBin& Bin : Bins)
	{
		for (nri::Pipeline* Pipeline : { Bin.DepthPipelines[0], Bin.DepthPipelines[1], Bin.DepthPipelines[2], Bin.DepthPipelines[3], Bin.ColourPipeline, Bin.OverdrawPipelines[0], Bin.OverdrawPipelines[1], Bin.FacePipeline,
				 Bin.SurfacePipeline, Bin.HudPipeline })
		{
			if (Pipeline)
			{
				Api.DestroyPipeline(Pipeline);
			}
		}
	}

	Bins.clear();
	LevelBinCount = 0;

	for (nri::Buffer*& Upload : ModelUploads)
	{
		if (Upload)
		{
			Api.DestroyBuffer(Upload);
			Upload = nullptr;
		}
	}

	ReleaseViewPipelines(GrassPass);
	GrassPass = {};
	GrassDraws.clear();

	if (GrassIndices)
	{
		Api.DestroyBuffer(GrassIndices);
		GrassIndices = nullptr;
	}
	GrassForwardFrame = u32(-1);

	if (TreeLights)
	{
		Api.DestroyBuffer(TreeLights);
		TreeLights = nullptr;
	}

	for (xr_vector<nri::DescriptorPool*>& Frame : ForwardPools)
	{
		for (nri::DescriptorPool* Item : Frame)
		{
			Api.DestroyDescriptorPool(Item);
		}

		Frame.clear();
	}

	for (nri::Descriptor* View : ForwardBlockViews)
	{
		Api.DestroyDescriptor(View);
	}

	for (u32 Kind = 0; Kind < ForwardRecordKinds; ++Kind)
	{
		for (u32 Frame = 0; Frame < FrameCount; ++Frame)
		{
			if (ForwardRecordBuffers[Kind][Frame])
			{
				Api.DestroyDescriptor(ForwardRecordViews[Kind][Frame]);
				Api.DestroyBuffer(ForwardRecordBuffers[Kind][Frame]);
				ForwardRecordBuffers[Kind][Frame] = nullptr;
			}
		}
	}

	if (ForwardLayout)
	{
		Api.DestroyPipelineLayout(ForwardLayout);
		ForwardLayout = nullptr;
	}

	ForwardBlockViews.clear();
	ForwardSet = nullptr;
	ForwardVisibleView = nullptr;
	ForwardFrame = u32(-1);
}

namespace
{
// The constant blocks and the samplers of the old backend's forward shaders, the scene's tables for the vertex stage,
// the texture buckets and what the frame hands a surface for the pixel stage
void CreateForward(const CShaderConstants& Constants)
{
	nri::Device& Gpu = *GRHI->Nri->Device;
	const nri::StageBits Vertex = nri::StageBits::VERTEX_SHADER;
	const nri::StageBits Pixel = nri::StageBits::FRAGMENT_SHADER;

	nri::RootDescriptorDesc Roots[32] = {};
	nri::Buffer* ConstantBuffer = StreamApi.GetStreamerConstantBuffer(*Streamer);
	Constants.ForEachBlock([&](u32 Slot, const void*, u32 Size)
	{
		const u32 Root = u32(ForwardBlockViews.size());
		R_ASSERT(Root < 32);
		Roots[Root].registerIndex = Slot;
		Roots[Root].descriptorType = nri::DescriptorType::CONSTANT_BUFFER;
		Roots[Root].shaderStages = Vertex | Pixel;

		if (Slot == ForwardBinRegister)
		{
			ForwardBinRoot = Root;
			ForwardBinBlock.assign(Size, 0);
		}

		if (Slot == ForwardHeadsRegister)
		{
			ForwardHeadsRoot = Root;
			Size = sizeof(ForwardHeads);
		}

		if (Slot == CBMaterial::Slot)
		{
			ForwardMaterialRoot = Root;
		}

		if (Slot == ForwardGrassRegister)
		{
			ForwardGrassRoot = Root;
			Size = sizeof(Hlsl::SceneGrassView);
		}

		nri::BufferViewDesc View = {};
		View.buffer = ConstantBuffer;
		View.type = nri::BufferView::CONSTANT_BUFFER;
		View.size = (Size + 255) & ~255u;
		nri::Descriptor* Block = nullptr;
		Api.CreateBufferView(View, Block);
		ForwardBlockViews.push_back(Block);
	});
	R_ASSERT(!ForwardBinBlock.empty());

	nri::RootSamplerDesc Samplers[ShaderBindings::SamplerCount] = {};
	u32 SamplerCount = 0;

	for (u32 Register = 0; Register < ShaderBindings::SamplerCount; ++Register)
	{
		if (RHIObject* State = SSManager.GetState(Register))
		{
			RHISampleDesc Source = {};
			State->GetDesc(&Source);
			Samplers[SamplerCount].registerIndex = Register;
			Samplers[SamplerCount].shaderStages = Pixel;
			Samplers[SamplerCount++].desc = NriSampler(Source);
		}
	}

	nri::DescriptorRangeDesc Ranges[std::size(ForwardRanges)] = {};
	u32 Records = 0;
	u32 Textures = 0;

	for (u32 Range = 0; Range < std::size(ForwardRanges); ++Range)
	{
		const ForwardRange& Item = ForwardRanges[Range];
		const bool IsTextures = !Item.IsRecords && Range != 1;
		const nri::DescriptorType Type = Item.IsRecords ? nri::DescriptorType::STRUCTURED_BUFFER : Range == 1 ? nri::DescriptorType::BUFFER : nri::DescriptorType::TEXTURE;
		// The grass buffers of the first two ranges are set after the frame's first part has bound the set
		const nri::DescriptorRangeBits Flags = Range < 2 ? nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET : IsTextures && Range != ForwardBucketRange ? nri::DescriptorRangeBits::PARTIALLY_BOUND : nri::DescriptorRangeBits::NONE;
		Ranges[Range] = { Item.First, Item.Count, Type, Range < ForwardFirstRange ? Vertex : Pixel, Flags };
		Records += Item.IsRecords ? Item.Count : 0;
		Textures += IsTextures ? Item.Count : 0;
	}

	nri::DescriptorSetDesc SetDesc = {};
	SetDesc.ranges = Ranges;
	SetDesc.rangeNum = u32(std::size(ForwardRanges));

	nri::PipelineLayoutDesc Desc = {};
	Desc.rootDescriptors = Roots;
	Desc.rootDescriptorNum = u32(ForwardBlockViews.size());
	Desc.rootSamplers = Samplers;
	Desc.rootSamplerNum = SamplerCount;
	Desc.descriptorSets = &SetDesc;
	Desc.descriptorSetNum = 1;
	Desc.shaderStages = Vertex | Pixel;
	Api.CreatePipelineLayout(Gpu, Desc, ForwardLayout);

	ForwardPoolDesc = {};
	ForwardPoolDesc.descriptorSetMaxNum = ForwardPoolSets;
	ForwardPoolDesc.structuredBufferMaxNum = Records * ForwardPoolSets;
	ForwardPoolDesc.bufferMaxNum = ForwardRanges[1].Count * ForwardPoolSets;
	ForwardPoolDesc.textureMaxNum = Textures * ForwardPoolSets;
	ForwardVisibleView = MakeView(Visible.Buffer, nri::BufferView::BUFFER, nri::Format::R32_UINT);
}

// The old backend's own buffers of these records are streams of its upload ring: nothing another device can wrap
void CreateForwardRecords()
{
	for (u32 Kind = 0; Kind < ForwardRecordKinds; ++Kind)
	{
		for (u32 Frame = 0; Frame < FrameCount; ++Frame)
		{
			nri::BufferDesc Desc = {};
			Desc.size = ForwardRecordBytes[Kind];
			Desc.structureStride = ForwardRecordStrides[Kind];
			Desc.usage = nri::BufferUsageBits::SHADER_RESOURCE;
			Api.CreateCommittedBuffer(*GRHI->Nri->Device, nri::MemoryLocation::HOST_UPLOAD, 0.0f, Desc, ForwardRecordBuffers[Kind][Frame]);
			std::memset(Api.MapBuffer(*ForwardRecordBuffers[Kind][Frame], 0, Desc.size), 0, Desc.size);
			Api.UnmapBuffer(*ForwardRecordBuffers[Kind][Frame]);

			nri::BufferViewDesc View = {};
			View.buffer = ForwardRecordBuffers[Kind][Frame];
			View.type = nri::BufferView::STRUCTURED_BUFFER;
			View.structureStride = ForwardRecordStrides[Kind];
			Api.CreateBufferView(View, ForwardRecordViews[Kind][Frame]);
		}
	}
}
} // namespace

// The sets take the skinning pass's view again when it has made a larger output; true when they did
bool WrapSkin()
{
	nri::Descriptor* Output = RImplementation.Skinning.GetView();

	if (!Output || Output == SkinView)
	{
		return false;
	}

	SkinView = Output;
	const nri::UpdateDescriptorRangeDesc Updates[2] = { { DrawSet, 0, 4 + PageSlots, &SkinView, 1 }, { AlphaSet, 0, 4 + PageSlots, &SkinView, 1 } };
	Api.UpdateDescriptorRanges(Updates, 2);

	for (const OwnView& Own : OwnViews)
	{
		if (Own.DrawSet)
		{
			const nri::UpdateDescriptorRangeDesc OwnSkin[2] = { { Own.DrawSet, 0, 4 + PageSlots, &SkinView, 1 }, { Own.AlphaSet, 0, 4 + PageSlots, &SkinView, 1 } };
			Api.UpdateDescriptorRanges(OwnSkin, 2);
		}
	}

	return true;
}

namespace
{
// IsAfterDepth: the depth pass of the same shaders has drawn the surface. Overdraw: the debug view that counts
// layers, DebugView_OverdrawRaster or DebugView_OverdrawShaded, 0 for the frame's own pipeline. IsSorted: a pass of the
// sorted list, its depth test and colour masks as its state says
// Vertex, Pixel: the shaders in the place of the pass's own, its states kept
nri::Pipeline* CreateForwardPipeline(const SPass& Pass, bool IsDepth, bool IsAfterDepth, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth, u32 Overdraw = 0, bool IsTwoSided = false, bool IsSorted = false,
	SVS* Vertex = nullptr, SPS* Pixel = nullptr)
{
	// The id pass has no pixel stage where nothing is cut out: a vertex stage of its own takes the pixel stage given
	Pixel = Pixel || Vertex ? Pixel : Pass.ps._get();
	Vertex = Vertex ? Vertex : Pass.vs._get();

	static const nri::CullMode Culls[] = { nri::CullMode::NONE, nri::CullMode::NONE, nri::CullMode::FRONT, nri::CullMode::BACK };
	const RHIRasterizerDesc& Raster = Pass.state->state->Raster;

	if (!Vertex || !Vertex->vs)
	{
		return nullptr;
	}

	nri::ShaderDesc Shaders[2] = {};
	u32 ShaderCount = 0;
	Vertex->CompileState.WaitCompiled();
	Shaders[ShaderCount++] = ShaderCode(Vertex->vs, nri::StageBits::VERTEX_SHADER);

	if (Pixel)
	{
		Pixel->CompileState.WaitCompiled();

		if (Pixel->ps)
		{
			Shaders[ShaderCount++] = ShaderCode(Pixel->ps, nri::StageBits::FRAGMENT_SHADER);
		}
	}

	// The depth pass leaves the colour alone and writes the thin surface and the velocity beside it
	static const nri::ColorWriteBits DepthMasks[4] = { nri::ColorWriteBits::NONE, nri::ColorWriteBits::RGBA, nri::ColorWriteBits::R | nri::ColorWriteBits::G, nri::ColorWriteBits::R | nri::ColorWriteBits::G };
	// ERHI_BLEND and ERHI_BLEND_OP keep the D3D numbering, which starts at 1
	static const nri::BlendFactor Factors[] = { nri::BlendFactor::ZERO, nri::BlendFactor::ZERO, nri::BlendFactor::ONE, nri::BlendFactor::SRC_COLOR, nri::BlendFactor::ONE_MINUS_SRC_COLOR, nri::BlendFactor::SRC_ALPHA,
		nri::BlendFactor::ONE_MINUS_SRC_ALPHA, nri::BlendFactor::DST_ALPHA, nri::BlendFactor::ONE_MINUS_DST_ALPHA, nri::BlendFactor::DST_COLOR, nri::BlendFactor::ONE_MINUS_DST_COLOR, nri::BlendFactor::SRC_ALPHA_SATURATE,
		nri::BlendFactor::ZERO, nri::BlendFactor::ZERO, nri::BlendFactor::CONSTANT_COLOR, nri::BlendFactor::ONE_MINUS_CONSTANT_COLOR, nri::BlendFactor::SRC1_COLOR, nri::BlendFactor::ONE_MINUS_SRC1_COLOR,
		nri::BlendFactor::SRC1_ALPHA, nri::BlendFactor::ONE_MINUS_SRC1_ALPHA };
	static const nri::BlendOp Ops[] = { nri::BlendOp::ADD, nri::BlendOp::ADD, nri::BlendOp::SUBTRACT, nri::BlendOp::REVERSE_SUBTRACT, nri::BlendOp::MIN, nri::BlendOp::MAX };
	const RHIBlendDesc& Blend = Pass.state->state->Blend;
	nri::ColorAttachmentDesc Colours[4] = {};

	for (u32 Target = 0; Target < TargetCount; ++Target)
	{
		// The colour pass blends as its state says
		const RHIRenderTargetBlendDesc& Own = Blend.RenderTarget[Blend.IndependentBlendEnable ? Target : 0];
		Colours[Target].format = nri::nriConvertDXGIFormatToNRI(u32(Targets[Target]->GetSurface()->GetFormat()));
		// A depth pass leaves its first target, the frame's colour, alone; the grass's one target is its surface
		Colours[Target].colorWriteMask = IsDepth && !(Pixel && TargetCount == 1) ? DepthMasks[Target] : IsSorted ? nri::ColorWriteBits(Own.RenderTargetWriteMask & 15u) : nri::ColorWriteBits::RGBA;
		Colours[Target].blendEnabled = !IsDepth && Own.BlendEnable;
		Colours[Target].colorBlend = { Factors[u32(Own.SrcBlend) % std::size(Factors)], Factors[u32(Own.DestBlend) % std::size(Factors)], Ops[u32(Own.BlendOp) % std::size(Ops)] };
		Colours[Target].alphaBlend = { Factors[u32(Own.SrcBlendAlpha) % std::size(Factors)], Factors[u32(Own.DestBlendAlpha) % std::size(Factors)], Ops[u32(Own.BlendOpAlpha) % std::size(Ops)] };
	}

	if (Overdraw)
	{
		Colours[0].blendEnabled = true;
		Colours[0].colorBlend = { nri::BlendFactor::ONE, nri::BlendFactor::ONE, nri::BlendOp::ADD };
		Colours[0].alphaBlend = Colours[0].colorBlend;
	}

	nri::GraphicsPipelineDesc Desc = {};
	Desc.pipelineLayout = ForwardLayout;
	Desc.inputAssembly.topology = nri::Topology::TRIANGLE_LIST;
	// Grass is drawn from both sides, whatever its blender says: the old draws override the pass the same way
	Desc.rasterization.cullMode = IsTwoSided ? nri::CullMode::NONE : Culls[u32(Raster.CullMode)];
	Desc.rasterization.frontCounterClockwise = Raster.FrontCounterClockwise != 0;
	Desc.rasterization.shadingRate = GpuScene::IsVariableRate();
	Desc.outputMerger.colors = Colours;
	Desc.outputMerger.colorNum = TargetCount;
	Desc.outputMerger.depthStencilFormat = nri::nriConvertDXGIFormatToNRI(NriDepthFormat(Depth->GetSurface()->GetFormat()));
	const RHIDepthStencilDesc& DepthTest = Pass.state->state->Depth;
	const nri::CompareOp SortedCompare = DepthTest.DepthEnable ? Compare[u32(DepthTest.DepthFunc) % std::size(Compare)] : nri::CompareOp::ALWAYS;
	Desc.outputMerger.depth.compareOp = Overdraw == DebugView_OverdrawRaster ? nri::CompareOp::ALWAYS : IsAfterDepth ? nri::CompareOp::EQUAL : IsSorted ? SortedCompare : nri::CompareOp::LESS_EQUAL;
	// The count of every layer leaves the depth to the pass that drew it before; with no such pass (no occlusion) it
	// writes its own, or the sky drawn after it covers the whole count
	Desc.outputMerger.depth.write = Overdraw ? Overdraw == DebugView_OverdrawShaded || !ForwardOcclusion() : !IsAfterDepth && Pass.state->state->Depth.DepthWriteMask == RHI_DEPTH_WRITE_MASK_ALL;

	Desc.shaders = Shaders;
	Desc.shaderNum = ShaderCount;

	nri::MultisampleDesc Multisample = {};
	Multisample.sampleMask = nri::ALL;
	Multisample.sampleNum = nri::Sample_t(Depth->GetSurface()->GetSampleDescCount());
	Multisample.alphaToCoverage = Blend.AlphaToCoverageEnable;
	Desc.multisample = Multisample.sampleNum > 1 ? &Multisample : nullptr;

	nri::Pipeline* Pipeline = nullptr;
	PROF_EVENT("ScenePipelineCreate");
	return Api.CreateGraphicsPipeline(*GRHI->Nri->Device, Desc, Pipeline) == nri::Result::SUCCESS ? Pipeline : nullptr;
}

// The id pass of a bin: the bin's depth test, cull mode and alpha reference, the id shaders, one target of ids
void CreateIdShaders()
{
	for (u32 Cut = 0; Cut < 2 && !IdVertex[1]; ++Cut)
	{
		ShaderExternalMap Defines;

		if (Cut)
		{
			Defines["USE_AREF"] = "1";
		}

		IdVertex[Cut] = DEV->_CreateVS("common\\scene_id", Defines);
		IdPixel[Cut] = DEV->_CreatePS("common\\scene_id", Defines);
	}
}

nri::Pipeline* CreateIdPipeline(const SPass& Pass, bool IsCut, IRHIRenderTargetView* const* Targets, IRHIDepthStencilView* Depth)
{
	return CreateForwardPipeline(Pass, false, false, Targets, 1, Depth, 0, false, false, IdVertex[IsCut]._get(), IdPixel[IsCut]._get());
}
} // namespace

nri::Descriptor* ForwardHeadsView()
{
	return ForwardRecordViews[ForwardHeadRecords][ForwardFrame % FrameCount];
}

nri::Descriptor* ForwardLightsView()
{
	return ForwardRecordViews[0][RFrame.dwFrame % FrameCount];
}
} // namespace GpuSceneInternal

void GpuScene::RefreshForward()
{
	SplitLeftovers(Leftover);
	ReleaseIdPipelines();

	if (Bins.empty() && !GrassPass.Source)
	{
		return;
	}

	GRHI->DevicePtr->Flush();

	const auto Refresh = [](SceneBin& Bin)
	{
		const ShaderElement* Element = Bin.Source ? Bin.Source->E[Bin.Variant]._get() : nullptr;

		if (!Element || Element->passes.empty() || !Element->passes.back())
		{
			return;
		}

		Bin.Colour = Element->passes.back();
		Bin.Depth = Element->passes.size() > 1 ? Element->passes[0] : ref_pass();
		const ShaderElement* Face = Bin.Source->E[ShaderVariant::ClusterFace]._get();
		Bin.Face = Face && !Face->passes.empty() && Bin.Variant == ShaderVariant::Cluster ? Face->passes[0] : ref_pass();

		if (Bin.IsBound)
		{
			Bin.Colour = BoundPass(Bin.Colour._get());
			Bin.Depth = BoundPass(Bin.Depth._get());
			Bin.Face = BoundPass(Bin.Face._get());
		}

		for (nri::Pipeline** Pipeline : { &Bin.DepthPipelines[0], &Bin.DepthPipelines[1], &Bin.DepthPipelines[2], &Bin.DepthPipelines[3], &Bin.ColourPipeline, &Bin.OverdrawPipelines[0], &Bin.OverdrawPipelines[1], &Bin.FacePipeline,
				 &Bin.SurfacePipeline, &Bin.HudPipeline })
		{
			if (*Pipeline)
			{
				Api.DestroyPipeline(*Pipeline);
				*Pipeline = nullptr;
			}
		}

		Bin.SurfacePixel = nullptr;
		Bin.HudPixel = nullptr;
	};

	BoundPasses.clear();

	for (SceneBin& Bin : Bins)
	{
		Refresh(Bin);
	}

	Refresh(GrassPass);
	ReleaseCardPipelines();
	ReleaseSortedPipelines();
	MakeCardPasses();

	// A model shader compiled again may share its draw with others no more, or read other maps: every one joins again,
	// in its own record, into the model bin of its passes now. The shaders stay held: TextureRefs knows their textures
	// by address
	ModelBinIds.clear();
	ModelRejects.clear();
}

void GpuScene::LogWholeCopies()
{
	u32 Back = 0;
	u64 BackBytes = 0;

	for (const ref_texture& Texture : DroppedTextures)
	{
		if (Texture->pSurface)
		{
			BackBytes += Texture->flags.MemoryUsage;

			if (Back++ < 12)
			{
				Msg("* [scene] whole copy taken back: %s", Texture->cName.c_str());
			}
		}
	}

	if (!DroppedTextures.empty())
	{
		Msg("* [scene] whole copies taken back: %u of %u textures, %llu MB by their files", Back, u32(DroppedTextures.size()), BackBytes >> 20);
	}
}

void GpuScene::LogTexturePools() const
{
	Textures.LogPools();
	LogWholeCopies();
	CullStatsStep = CullStatsStep ? CullStatsStep : 1;
}

bool GpuScene::HasNoWholeCopy(const CTexture* Texture) const
{
	return std::binary_search(DroppedTextures.begin(), DroppedTextures.end(), Texture, [](const auto& Left, const auto& Right) { return std::less<const CTexture*>()(&*Left, &*Right); });
}

bool GpuScene::IsForwardBinned() const
{
	return ClusterCount && !Bins.empty();
}

bool GpuScene::IsStaticBinned() const
{
	return IsForwardBinned() && !MainLeftovers;
}

bool GpuScene::HasForwardGrass() const
{
	return GrassPass.Colour && ClusterCount != 0;
}

bool GpuScene::IsVariableRate()
{
	if (!GRHI || !GRHI->Nri || !GRHI->Nri->Device)
	{
		return false;
	}

	nri::CoreInterface Core = {};
	// Coverage written at a coarse rate is of the second tier: on the first, declaring it turns the rate off
	return nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::CoreInterface), &Core) == nri::Result::SUCCESS && Core.GetDeviceDesc(*GRHI->Nri->Device).tiers.shadingRate >= 2;
}

bool GpuScene::IsCoarseAlpha()
{
	return ps_r_variable_shading && !ps_r_msaa && IsVariableRate();
}

bool GpuScene::IsForwardGrassReady() const
{
	return GrassForwardFrame == RFrame.dwFrame;
}

void GpuScene::SetForwardRecords(u32 Kind, const void* Data, u32 Bytes)
{
	if (!IsForwardBinned() && !HasForwardGrass())
	{
		return;
	}

	if (!ForwardRecordBuffers[0][0])
	{
		CreateForwardRecords();
	}

	nri::Buffer& Buffer = *ForwardRecordBuffers[Kind][RFrame.dwFrame % FrameCount];
	Bytes = std::min(Bytes, ForwardRecordBytes[Kind]);
	std::memcpy(Api.MapBuffer(Buffer, 0, Bytes), Data, Bytes);
	Api.UnmapBuffer(Buffer);
}

bool GpuScene::DrawForward(GfxCmdList& Cmd, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth, bool IsColour, u32 Parts, const u32* FaceSlice)
{
	if (!(IsForwardBinned() || HasForwardGrass()) || !TargetCount || TargetCount > 4 || (!(Parts & 103) && CardFrame != RFrame.dwFrame) || ((Parts & 32) && SortedSegments.empty()))
	{
		return false;
	}

	// A face of the cube around the eye reads nothing of the screen and leaves the frame's bookkeeping alone
	const bool IsFace = FaceSlice != nullptr;

	GPU_EVENT(SceneDraw);

	const bool IsBinned = IsForwardBinned();
	const u32 Samples = Depth->GetSurface()->GetSampleDescCount();

	// The sorted segments keep a pipeline a pass with its sample count: the scope's mask is drawn past the multisampled part
	if (!IsFace && !(Parts & 32) && Samples != ForwardSamples)
	{
		PROF_EVENT("ForwardSamples");
		GRHI->DevicePtr->Flush();
		ReleaseViewPipelines(GrassPass);
		ReleaseCardPipelines();
		ReleaseSortedPipelines();
		ReleaseIdPipelines();

		for (SceneBin& Bin : Bins)
		{
			ReleaseViewPipelines(Bin);
		}

		ForwardSamples = Samples;
	}

	if (!ForwardLayout)
	{
		PROF_EVENT("ForwardCreate");
		CreateForward(Cmd.Constants);
	}

	if (!ForwardRecordBuffers[0][0])
	{
		CreateForwardRecords();
	}

	// What the passes read of the frame: the textures of their lists, the same for a register in all of them though
	// a pass lists only what its shaders read, and what the frame binds by hand for every surface
	IRHISurface* Reads[ForwardSlots];
	nri::Descriptor* SlotViews[ForwardSlots] = {};
	u32 ReadCount = 0;
	const auto Read = [&](u32 Slot, IRHISurface* Surface)
	{
		if (!Surface || Slot >= ForwardSlots || SlotViews[Slot])
		{
			return;
		}

		if (nri::Descriptor* View = RImplementation.NriPasses.WrapRead(Surface))
		{
			Reads[ReadCount++] = Surface;
			SlotViews[Slot] = View;
		}
	};
	// What NRI made is read through its own view, in the state it keeps
	const auto ReadView = [&](u32 Slot, CTexture* Texture)
	{
		nri::Descriptor* View = Texture->BoundView();

		if (!View)
		{
			Read(Slot, Texture->pSurface);
		}
		else if (Slot < ForwardSlots && !SlotViews[Slot])
		{
			SlotViews[Slot] = View;
		}
	};
	const auto ReadTexture = [&](const char* Name, const ref_texture& Texture)
	{
		if (Texture)
		{
			ReadView(ShaderBindings::Texture(Name), Texture._get());
		}
	};

	// The cube a pass binds is read from the cube array by the material
	SlotViews[ShaderBindings::Texture("s_env")] = EnvArrayView;

	// What a pass's pixel stage reads of its list outside the buckets is bound as it is: the maps below the buckets of an
	// XML blend's surface, whose base alone is in them, a sequence at its frame (bind); what the frame hands it
	const auto ReadPass = [&](const SPass& Pass)
	{
		if (!Pass.T || !Pass.ps)
		{
			return;
		}

		Pass.ps->CompileState.WaitCompiled();

		for (const auto& [Stage, Texture] : *Pass.T)
		{
			if (!Texture._get() || !IsForwardRead(Pass, Stage))
			{
				continue;
			}

			const u32 Slot = Stage - CTexture::rstPixel;
			Texture->EnsureLoaded();

			if (Slot && Slot < ForwardRanges[ForwardBucketRange].First)
			{
				Texture->bind(Stage);
			}

			ReadView(Slot, Texture._get());
		}
	};

	// A register holds one texture in a draw: the sorted segments draw alone (FlushRuns splits them where they clash),
	// and the blended bins (Parts 64), whose registers the others bind other textures to, apart from them
	for (const SceneBin& Bin : Bins)
	{
		if (Bin.Count && !(Parts & 32) && (Bin.Order == 4) == ((Parts & 64) != 0))
		{
			ReadPass(*Bin.Colour);
		}
	}

	for (const SortedSegment& Segment : SortedSegments)
	{
		ReadPass(*Segment.Pass);
	}

	CRenderTarget& Frame = *RImplementation.Target;
	ReadTexture("s_rain_splash", Frame.t_rain_splash);
	ReadTexture("s_rain_streaks", Frame.t_rain_streaks);
	Read(ShaderBindings::Texture("s_decals"), RImplementation.DecalArray);

	for (u32 Slot = 0; Slot < RImplementation.SurfaceProjectors.size(); ++Slot)
	{
		string64 Name;
		xr_sprintf(Name, "s_forward_projector%u", Slot);
		ReadView(ShaderBindings::Texture(Name), RImplementation.SurfaceProjectors[Slot]);
	}

	if (Frame.rt_LightTiles)
	{
		Read(ShaderBindings::Texture("s_light_tiles"), Frame.rt_LightTiles->pSurface);
	}

	// The blended surfaces of the XML blends do not list it
	Read(ShaderBindings::Texture("s_heightfield_shadow"), RImplementation.HeightfieldShadows.Shadow());

	if (RImplementation.o.ProceduralRainPuddles)
	{
		ReadTexture("s_puddles_normal_forward", Frame.t_puddle_normal);
		ReadTexture("s_puddles_ripples_forward", Frame.t_puddle_ripples);
		ReadTexture("s_puddles_water_height_forward", Frame.t_puddle_water);
	}

	if (Frame.SnowInMaterial)
	{
		ReadTexture("s_snow_base_forward", Frame.t_snow_base);
		ReadTexture("s_snow_bumpX_forward", Frame.t_snow_bump_x);
	}

	// An icon may be drawn in the frame after a reset, which draws no scene: the reflections' targets are made by the
	// first scene
	if (ForwardReflections() && IsColour && !IsFace && Frame.SslrOutput())
	{
		Read(ShaderBindings::Texture("s_refl"), Frame.SslrOutput()->pSurface);
	}

	if (IsColour && !IsFace && Frame.rt_SkyBehind)
	{
		Read(ShaderBindings::Texture("s_sky_behind"), Frame.rt_SkyBehind->pSurface);
	}

	// A material's map names one of these by its register; what is no single 2D surface shows the placeholder
	for (u32 Index = 0; Index < BoundTextures.size(); ++Index)
	{
		CTexture& Texture = *BoundTextures[Index];
		Texture.EnsureLoaded();
		IRHISurface* Surface = Texture.surface_get();
		const bool IsPlain = Surface && Surface->GetTextureType() == ERHI_RESOURCE_DIMENSION::TEXTURE2D && Surface->GetArraySize() == 1 && Surface->GetSampleDescCount() <= 1;
		ReadView(ForwardRanges[ForwardBoundRange].First + Index, IsPlain ? &Texture : DEV->_GetTexturePlaceholder());

		if (Surface)
		{
			Surface->Release();
		}
	}

	SlotViews[24] = AlphaView;
	const bool IsSkinMoved = WrapSkin();

	// The set of the frame's draws so far, unless this draw binds another texture where they bound one
	const bool IsNewFrame = ForwardFrame != RFrame.dwFrame;
	bool IsClash = false;

	for (u32 Slot = 0; Slot < ForwardSlots && !IsNewFrame; ++Slot)
	{
		IsClash |= SlotViews[Slot] && ForwardSetViews[Slot] && SlotViews[Slot] != ForwardSetViews[Slot];
	}

	xr_vector<nri::DescriptorPool*>& FramePools = ForwardPools[RFrame.dwFrame % FrameCount];

	if (IsNewFrame || IsClash)
	{
		PROF_EVENT("ForwardFrameSet");

		if (IsNewFrame)
		{
			ForwardFrame = RFrame.dwFrame;
			ForwardSetCount = 0;

			for (nri::DescriptorPool* Item : FramePools)
			{
				Api.ResetDescriptorPool(*Item);
			}
		}

		if (ForwardSetCount / ForwardPoolSets == FramePools.size())
		{
			Require(Api.CreateDescriptorPool(*GRHI->Nri->Device, ForwardPoolDesc, FramePools.emplace_back()), "The graphics device could not create a scene descriptor pool.");
		}

		Api.AllocateDescriptorSets(*FramePools[ForwardSetCount / ForwardPoolSets], *ForwardLayout, 0, &ForwardSet, 1, 0);
		++ForwardSetCount;
		std::fill(std::begin(ForwardSetViews), std::end(ForwardSetViews), nullptr);

		const bool HasGrass = GrassForwardFrame == ForwardFrame && Grass.Views[0];
		nri::Descriptor* Lists[3 + PageSlots] = { ClusterIndexView, ForwardVisibleView, HasGrass ? Grass.Views[3] : ClusterIndexView };
		std::copy(std::begin(IndexPageViews), std::end(IndexPageViews), Lists + 3);
		nri::Descriptor* Pages[PageSlots];
		nri::Descriptor* Buckets[SCENE_TEXTURE_BUCKETS];

		for (u32 Page = 0; Page < PageSlots; ++Page)
		{
			Pages[Page] = VertexPageViews[PageSlots - 1 - Page];
		}

		for (u32 Bucket = 0; Bucket < SCENE_TEXTURE_BUCKETS; ++Bucket)
		{
			Buckets[Bucket] = Textures.View(Bucket);
		}

		const u32 Slot = ForwardFrame % FrameCount;
		nri::Descriptor* Tables[8] = { SceneTableViews[0], SceneTableViews[1], SceneTableViews[2], SceneTableViews[3], TreeLightView, HasGrass ? Grass.Views[0] : TreeLightView, ModelView, SkinView ? SkinView : SkinFiller };
		const nri::UpdateDescriptorRangeDesc Updates[8] = {
			{ ForwardSet, 0, 0, Tables, 8 }, { ForwardSet, 1, 0, Lists, 3 + PageSlots }, { ForwardSet, 2, 0, Pages, PageSlots },
			{ ForwardSet, ForwardBucketRange, 0, Buckets, SCENE_TEXTURE_BUCKETS }, { ForwardSet, ForwardMaterialRange, 0, &SceneTableViews[3], 1 },
			{ ForwardSet, ForwardRecordRanges[0], 0, &ForwardRecordViews[0][Slot], 1 }, { ForwardSet, ForwardRecordRanges[1], 0, &ForwardRecordViews[1][Slot], 1 },
			{ ForwardSet, ForwardRecordRanges[ForwardHeadRecords], 0, &ForwardRecordViews[ForwardHeadRecords][Slot], 1 } };
		Api.UpdateDescriptorRanges(Updates, 8);
	}
	else if (IsSkinMoved)
	{
		const nri::UpdateDescriptorRangeDesc SkinUpdate = { ForwardSet, 0, 7, &SkinView, 1 };
		Api.UpdateDescriptorRanges(&SkinUpdate, 1);
	}

	// The grass is culled after the frame's first part is drawn: its buffers are set once they are known. Only the
	// grass shaders read these two slots
	if (GrassForwardFrame == ForwardFrame && Grass.Views[0])
	{
		const nri::UpdateDescriptorRangeDesc GrassUpdates[2] = { { ForwardSet, 0, 5, &Grass.Views[0], 1 }, { ForwardSet, 1, 2, &Grass.Views[3], 1 } };
		Api.UpdateDescriptorRanges(GrassUpdates, 2);
	}

	nri::UpdateDescriptorRangeDesc FrameUpdates[ForwardSlots] = {};
	u32 UpdateCount = 0;

	for (u32 Range = ForwardFirstRange; Range < std::size(ForwardRanges); ++Range)
	{
		const ForwardRange& Item = ForwardRanges[Range];

		for (u32 Slot = Item.First; Slot < Item.First + Item.Count && !Item.IsRecords && Range != ForwardBucketRange; ++Slot)
		{
			if (SlotViews[Slot])
			{
				ForwardSetViews[Slot] = SlotViews[Slot];
				FrameUpdates[UpdateCount++] = { ForwardSet, Range, Slot - Item.First, &SlotViews[Slot], 1 };
			}
		}
	}

	Api.UpdateDescriptorRanges(FrameUpdates, UpdateCount);

	// The models read the index data of the pages too
	const bool HasModels = IsBinned && !IsFace && ((ModelFrame == RFrame.dwFrame && !ModelRecords.empty()) || (Parts & 128));

	for (u32 Page = 0; Page < RImplementation.Geometry.PageCount(); ++Page)
	{
		if (IRHIBuffer* Source = RImplementation.Geometry.VertexBuffer(Page))
		{
			GRHI->DevicePtr->ReadExternalBuffer(Source);

			if (HasModels)
			{
				GRHI->DevicePtr->ReadExternalBuffer(RImplementation.Geometry.IndexBuffer(Page));
			}
		}
	}

	const SPass* GrassDrawn = !IsFace && GrassIndices && GrassForwardFrame == RFrame.dwFrame && (Parts & 6) && !(Parts & (256 | 2048)) && (!(Parts & 1024) || (!IsColour && (Parts & 4))) ? (IsColour ? GrassPass.Colour._get() : GrassPass.Depth._get()) : nullptr;

	if (GrassDrawn)
	{
		GRHI->DevicePtr->ReadExternalBuffer(Grass.Source);
	}

	// The wrappers of the old backend's targets live for this pass
	nri::Texture* Wrapped[5] = {};
	nri::Descriptor* TargetViews[5] = {};
	nri::AttachmentDesc Colours[4] = {};

	for (u32 Target = 0; Target < TargetCount; ++Target)
	{
		Wrapped[Target] = WrapNriSurface(Targets[Target]->GetSurface());
		nri::TextureViewDesc ViewDesc = {};
		ViewDesc.texture = Wrapped[Target];
		ViewDesc.type = nri::TextureView::COLOR_ATTACHMENT;
		ViewDesc.mipNum = 1;
		ViewDesc.layerNum = 1;
		ViewDesc.layerOffset = nri::Dim_t(IsFace ? *FaceSlice : 0);
		Api.CreateTextureView(ViewDesc, TargetViews[Target]);
		Colours[Target].descriptor = TargetViews[Target];
		Colours[Target].loadOp = nri::LoadOp::LOAD;
		Colours[Target].storeOp = nri::StoreOp::STORE;
	}

	// A view of overdraw counts layers into a colour target it has cleared; the shaded one counts what passes the
	// depth test in the order of the draws, so it draws the depth again from nothing
	const u32 Overdraw = IsColour && !IsFace && !(Parts & 32) && (ps_r_debug_view == DebugView_OverdrawRaster || ps_r_debug_view == DebugView_OverdrawShaded) ? ps_r_debug_view : 0;
	const bool IsOverdrawFirst = Overdraw && OverdrawFrame != RFrame.dwFrame;
	const bool IsCoarse = IsCoarseAlpha();
	// The passes drawn with the depth pass's shaders, which write the coverage of a coarse block
	const bool IsCutPass = !IsColour || Overdraw == DebugView_OverdrawShaded;

	if (IsOverdrawFirst)
	{
		OverdrawFrame = RFrame.dwFrame;
		Colours[0].loadOp = nri::LoadOp::CLEAR;
	}

	IRHISurface* DepthSurface = Depth->GetSurface();
	Wrapped[4] = WrapNriSurface(DepthSurface, NriDepthFormat(DepthSurface->GetFormat()));
	TargetViews[4] = MakeSliceView(Wrapped[4], 0);

	// Shaders are made before the draw is recorded: the recording holds the device's context, and a loader thread that
	// holds the resource manager waits for that context to adopt a texture
	if (Parts & 256)
	{
		CreateIdShaders();
	}

	// The terrain keeps its surfaces in its depth pass as the models do, in place of its ids: its layers were half of
	// what the resolve cost, and drawn they cost the depth pass nothing to speak of. r_rendering_path 1: the whole
	// level does
	const auto IsLevelKept = [](const SceneBin& Bin) { return RenderingPath() == 1 || Bin.Order == 1; };

	for (u32 Index = !IsColour ? 0 : LevelBinCount; Index < ((Parts & 1024) && IsBinned ? Bins.size() : 0); ++Index)
	{
		SceneBin& Bin = Bins[Index];
		ref_ps& Stage = IsColour ? Bin.HudPixel : Bin.SurfacePixel;

		if (!Stage && Bin.Order <= 2 && Bin.Depth && Bin.Depth->ps && Bin.Colour->ps && !Bin.IsEmissive && !(Index < LevelBinCount && (Bin.IsBound || !IsLevelKept(Bin))))
		{
			Stage = DEV->CreatePSWith(*(IsColour ? Bin.Colour : Bin.Depth)->ps, IsColour ? "SURFACE_HUD_ONLY" : "SURFACE_WRITES_SURFACE");
		}
	}

	if (GrassDrawn && (Parts & 1024) && GrassDrawn->ps && !GrassPass.SurfacePixel)
	{
		// The surface alone, into the one target the frame gives the grass
		const ref_ps Kept = DEV->CreatePSWith(*GrassDrawn->ps, "SURFACE_WRITES_SURFACE");
		GrassPass.SurfacePixel = Kept ? DEV->CreatePSWith(*Kept, "SURFACE_KEEPS_ONLY") : Kept;
	}

	nri::CommandBuffer& Commands = *BeginNriCommands(Reads, ReadCount, Targets, TargetCount, Depth);
	PROF_EVENT("ForwardRecord");
	Move(Commands, Counters, nri::AccessBits::ARGUMENT_BUFFER, nri::StageBits::INDIRECT);
	Move(Commands, Visible, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::VERTEX_SHADER);
	Move(Commands, Models, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::VERTEX_SHADER);

	if (Parts & 32)
	{
		UploadSorted(Commands);
	}

	if ((Parts & 24) && CardArgs.Buffer)
	{
		Move(Commands, CardArgs, nri::AccessBits::ARGUMENT_BUFFER, nri::StageBits::INDIRECT);
	}

	// What earlier frames asked for; the copy of a frame still on the GPU reads as an older one
	if (ForwardStreamFrame != RFrame.dwFrame)
	{
		PROF_EVENT("ForwardStreaming");
		ForwardStreamFrame = RFrame.dwFrame;
		u32 Seen[SCENE_TEXTURE_STREAMS];

		// Reading back what the GPU wrote: a device that has not finished the frame makes the map wait for it
		{
			PROF_EVENT("FeedbackReadback");
			nri::Buffer& Oldest = *FeedbackCopies[(RFrame.dwFrame + 1) % FeedbackFrames];
			std::memcpy(Seen, Api.MapBuffer(Oldest, 0, FeedbackBytes), FeedbackBytes);
			Api.UnmapBuffer(Oldest);
		}

		// The grass and the sorted list have no clusters in the cull to ask for them
		for (u32 Stream : SortedStreams)
		{
			Seen[Stream] = (ForwardStreamFrame << 8) | 255;
		}

		if (HasForwardGrass())
		{
			for (u32 Stream : GrassStreams)
			{
				Seen[Stream] = (ForwardStreamFrame << 8) | 255;
			}
		}

		Textures.Update(Seen, ForwardStreamFrame, Commands, *Streamer);
		TextureDecode::Upload(Commands, *Streamer);

		const xr_vector<u32>& Heads = Textures.GetHeads();
		nri::Buffer& HeadBuffer = *ForwardRecordBuffers[ForwardHeadRecords][ForwardFrame % FrameCount];
		u32* HeadWords = static_cast<u32*>(Api.MapBuffer(HeadBuffer, 0, ForwardRecordBytes[ForwardHeadRecords]));
		std::fill_n(HeadWords, SCENE_TEXTURE_STREAMS, SCENE_TEXTURE_ABSENT);
		std::copy_n(Heads.data(), std::min<size_t>(Heads.size(), SCENE_TEXTURE_STREAMS), HeadWords);
		Api.UnmapBuffer(HeadBuffer);
	}


	ForwardHeads HeadBlock = {};
	HeadBlock.Frame[0] = RFrame.dwFrame;

	Api.CmdSetDescriptorPool(Commands, *FramePools[(ForwardSetCount - 1) / ForwardPoolSets]);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::GRAPHICS, *ForwardLayout);

	// What every tree of the frame shares; its own place and light are in the tables
	{
		Fvector4 Wave, Wind;
		float SwayScale;
		GetTreeWave(Wave, Wind, SwayScale);
		CBPass& Trees = Cmd.Constants.Write<CBPass>();
		Trees.consts.set(SwayScale, SwayScale, 0.0f, 0.0f);
		Trees.wave = Wave;
		Trees.wind = Wind;

		const Fvector3& Ambient = RFrame.Env().ambient;
		const float Words[8] = { Ambient.x, Ambient.y, Ambient.z, 0.0f, ps_r__Detail_l_aniso, ps_r__Detail_l_ambient, 0.0f, 0.0f };
		std::memcpy(ForwardBinBlock.data() + 16, Words, sizeof(Words));
	}

	// The grass block takes the register of CBUIIcon, which an icon's passes read in its place
	static_assert(sizeof(Hlsl::SceneGrassView) >= sizeof(CBUIIcon));
	Hlsl::SceneGrassView IconBlock = {};

	if (Parts & 128)
	{
		std::memcpy(&IconBlock, &Cmd.Constants.Read<CBUIIcon>(), sizeof(CBUIIcon));
	}

	u32 Block = 0;
	const auto StreamBlock = [&](u32, const void* Data, u32 Size)
	{
		nri::SetRootDescriptorDesc Root = {};
		Root.rootDescriptorIndex = Block;
		Root.descriptor = ForwardBlockViews[Block];
		Root.offset = Block == ForwardHeadsRoot ? StreamApi.StreamConstantData(*Streamer, &HeadBlock, sizeof(HeadBlock))
			: Block == ForwardGrassRoot ? StreamApi.StreamConstantData(*Streamer, Parts & 128 ? &IconBlock : &GrassForwardBlock, sizeof(GrassForwardBlock)) : StreamApi.StreamConstantData(*Streamer, Data, Size);
		Api.CmdSetRootDescriptor(Commands, Root);
		++Block;
	};

	{
		PROF_EVENT("ForwardBlocks");
		Cmd.Constants.ForEachBlock(StreamBlock);
	}

	u32 DrawnWidth = DepthSurface->GetWidth();
	u32 DrawnHeight = DepthSurface->GetHeight();
	ViewSize(DrawnWidth, DrawnHeight);
	const nri::Viewport Viewport = { 0.0f, 0.0f, float(DrawnWidth), float(DrawnHeight), 0.0f, 1.0f };
	nri::Rect Scissor = { 0, 0, nri::Dim_t(DrawnWidth), nri::Dim_t(DrawnHeight) };

	// An icon is clipped to its window as the interface clips it
	if (const Irect* Clip = Parts & 128 ? Cmd.GetScissor() : nullptr)
	{
		const s32 Left = std::clamp(Clip->x1, 0, s32(DrawnWidth));
		const s32 Top = std::clamp(Clip->y1, 0, s32(DrawnHeight));
		Scissor = { s16(Left), s16(Top), nri::Dim_t(std::clamp(Clip->x2, Left, s32(DrawnWidth)) - Left), nri::Dim_t(std::clamp(Clip->y2, Top, s32(DrawnHeight)) - Top) };
	}

	Api.CmdSetViewports(Commands, &Viewport, 1);
	Api.CmdSetScissors(Commands, &Scissor, 1);

	nri::RenderingDesc Rendering = {};
	Rendering.colors = Colours;
	Rendering.colorNum = TargetCount;
	Rendering.depth.descriptor = TargetViews[4];
	Rendering.depth.loadOp = IsOverdrawFirst && Overdraw == DebugView_OverdrawShaded ? nri::LoadOp::CLEAR : nri::LoadOp::LOAD;
	Rendering.depth.clearValue.depthStencil.depth = 1.0f;
	Rendering.depth.storeOp = nri::StoreOp::STORE;
	Api.CmdBeginRendering(Commands, Rendering);

	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = ForwardSet;
	Api.CmdSetDescriptorSet(Commands, SetDesc);

	// Every bin's pipeline for this pass is made by the first frame that draws the pass: a bin whose first cluster
	// comes into view later finds it made
	u32 Made = 0;
	CTimer MakeTimer;
	MakeTimer.Start();

	for (u32 Index = 0; Index < (IsBinned && (Parts & 3) && !(Parts & (256 | 1024)) ? Bins.size() : 0); ++Index)
	{
		SceneBin& Bin = Bins[Index];
		const bool IsCut = Overdraw == DebugView_OverdrawShaded && Bin.Depth && Bin.Depth->ps;
		const SPass* Pass = IsFace ? Bin.Face._get() : IsCut || !IsColour ? Bin.Depth._get() : Bin.Colour._get();
		nri::Pipeline*& Pipeline = IsFace ? Bin.FacePipeline : Overdraw ? Bin.OverdrawPipelines[Overdraw - 1] : IsColour ? Bin.ColourPipeline : Bin.DepthPipelines[TargetCount];

		if (Pass && !Pipeline && !Bin.Count)
		{
			Pipeline = CreateForwardPipeline(*Pass, !IsColour, !IsFace && !Overdraw && IsColour && Bin.Depth, Targets, TargetCount, Depth, Overdraw);
			Made += Pipeline != nullptr;
		}
	}

	if (Made)
	{
		Msg("* [scene] forward pipelines made ahead of their first cluster: %u in %u ms (%s, %u targets)", Made, MakeTimer.GetElapsed_ms(), IsColour ? "colour" : "depth", TargetCount);
	}

	const bool IsMarked = GRHI->GPUStatsEnable || DevicePtr->GetRenderDocAPI();
	PROF_EVENT("ForwardBins");

	// The blended bins draw both parts of the list in a call of their own (Parts 64), after everything opaque
	const bool IsBlended = (Parts & 64) != 0;
	const u32 Lists = IsBlended ? 3u : Parts & 3u;

	for (u32 Step = IsBlended ? 4u : 0u; Step < (IsBlended ? 5u : 4u); ++Step)
	{
		for (u32 Index = 0; Index < (IsBinned && Lists ? Bins.size() : 0); ++Index)
		{
			SceneBin& Bin = Bins[Index];
			const bool IsIds = (Parts & 256) != 0;
			const SPass* Pass = IsFace ? Bin.Face._get() : IsColour || (IsIds && !Bin.Depth) ? Bin.Colour._get() : Bin.Depth._get();
			// What the resolve cannot shade stays a forward draw: a material with a map bound outside the buckets, or
			// one that gives off light
			const bool IsDeferred = Index < LevelBinCount && Bin.Order <= 2 && !Bin.IsBound && !Bin.IsEmissive;
			const bool IsShadedById = IsDeferred && !IsLevelKept(Bin);
			// A model's opaque parts keep their surfaces in their depth pass for the same frame to light (Parts 1024)
			const bool IsLevelSurfaced = IsDeferred && IsLevelKept(Bin) && Bin.Depth && Bin.Depth->ps && Bin.Colour->ps;
			const bool IsSurfaced = IsLevelSurfaced || (Index >= LevelBinCount && Bin.Order <= 2 && !Bin.IsEmissive && Bin.Depth && Bin.Depth->ps && Bin.Colour->ps);
			const bool IsSurfaces = (Parts & 1024) != 0;

			// The colour pass of Parts 1024 is of the HUD's parts: the level has none
			if (!Bin.Count || !Pass || Bin.Order != Step || (IsIds ? !IsShadedById : (Parts & 512) && IsShadedById) || (IsSurfaces ? !IsSurfaced || (IsColour && IsLevelSurfaced) : (Parts & 2048) && IsSurfaced))
			{
				continue;
			}

			// The shaded count is of what the depth pass keeps: its shader is the one that cuts a surface by its texture
			if (Overdraw == DebugView_OverdrawShaded && Bin.Depth && Bin.Depth->ps)
			{
				Pass = Bin.Depth._get();
			}

			nri::Pipeline*& Pipeline = IsIds ? IdPipelines[Bin.Order == 2][u32(Pass->state->state->Raster.CullMode) & 3u]
				: IsSurfaces ? (IsColour ? Bin.HudPipeline : Bin.SurfacePipeline) : IsFace ? Bin.FacePipeline : Overdraw ? Bin.OverdrawPipelines[Overdraw - 1] : IsColour ? Bin.ColourPipeline : Bin.DepthPipelines[TargetCount];

			if (!Pipeline)
			{
				Pipeline = IsIds ? CreateIdPipeline(*Pass, Bin.Order == 2, Targets, Depth)
					: IsSurfaces ? CreateForwardPipeline(*Pass, !IsColour, IsColour, Targets, TargetCount, Depth, 0, false, false, nullptr, (IsColour ? Bin.HudPixel : Bin.SurfacePixel)._get())
					: CreateForwardPipeline(*Pass, !IsColour, !IsFace && !Overdraw && IsColour && Bin.Depth, Targets, TargetCount, Depth, Overdraw);
			}

			if (!Pipeline)
			{
				continue;
			}

			// A bin is named for whoever reads the frame: a mark costs more than the bin's draws
			const std::optional<CRHIGPUMark> BinMark = IsMarked ? std::optional<CRHIGPUMark>(std::in_place, *Pass->ps->cName, L"SceneBin") : std::nullopt;
			Api.CmdSetPipeline(Commands, *Pipeline);

			if (IsCoarse)
			{
				const nri::ShadingRateDesc Rate = { Bin.Order == 2 && IsCutPass ? nri::ShadingRate::FRAGMENT_SIZE_2X2 : nri::ShadingRate::FRAGMENT_SIZE_1X1, nri::ShadingRateCombiner::KEEP, nri::ShadingRateCombiner::KEEP };
				Api.CmdSetShadingRate(Commands, Rate);
			}
			// The reference of the dynamic frame, as the grass takes it: a tree's blender asks for nearly twice as much,
			// which leaves a far crown half empty
			Cmd.SetAlphaRef(Pass->state->state->AlphaRef ? u32(ps_r2_def_aref_quality) : 0u);
			Cmd.Constants.Write<CBMaterial>().dt_params.w = 1.0f / r__dtex_range;
			nri::SetRootDescriptorDesc Material = {};
			Material.rootDescriptorIndex = ForwardMaterialRoot;
			Material.descriptor = ForwardBlockViews[ForwardMaterialRoot];
			Material.offset = StreamApi.StreamConstantData(*Streamer, &Cmd.Constants.Read<CBMaterial>(), sizeof(CBMaterial));
			Api.CmdSetRootDescriptor(Commands, Material);

			for (u32 Part = 0; Part < 2; ++Part)
			{
				// A model bin has entries in the main view's first part alone
				if (!(Lists & (1u << Part)) || (Part && Index >= LevelBinCount))
				{
					continue;
				}

				const u32 ListPart = IsFace ? 2 + *FaceSlice : Part;
				*reinterpret_cast<u32*>(ForwardBinBlock.data()) = Bin.Start + ListPart * (ClusterCount + ModelClusters) + (IsFace ? SCENE_DYNAMIC_CLUSTERS : 0);
				// The class the id pass names a bin's pixels by: terrain apart from everything else
				reinterpret_cast<u32*>(ForwardBinBlock.data())[2] = Bin.Order == 1 ? 1u : 0u;
				nri::SetRootDescriptorDesc Root = {};
				Root.rootDescriptorIndex = ForwardBinRoot;
				Root.descriptor = ForwardBlockViews[ForwardBinRoot];
				Root.offset = StreamApi.StreamConstantData(*Streamer, ForwardBinBlock.data(), u32(ForwardBinBlock.size()));
				Api.CmdSetRootDescriptor(Commands, Root);
				Api.CmdDrawIndirect(Commands, *Counters.Buffer, (SCENE_COUNTER_WORDS + (Index + ListPart * Bins.size()) * 4) * sizeof(u32), 1, sizeof(nri::DrawDesc), nullptr, 0);
			}
		}

		// The frame's grass: every entry of its cull in one draw
		if (GrassDrawn && Step == 3)
		{
			// Its depth pass keeps its surfaces for the frame that lights them later (Parts 1024)
			const bool IsKept = (Parts & 1024) != 0 && GrassDrawn->ps;
			nri::Pipeline*& Pipeline = IsKept ? GrassPass.SurfacePipeline : Overdraw ? GrassPass.OverdrawPipelines[Overdraw - 1] : IsColour ? GrassPass.ColourPipeline : GrassPass.DepthPipelines[TargetCount];

			if (!Pipeline)
			{
				const bool IsCut = Overdraw == DebugView_OverdrawShaded && GrassPass.Depth && GrassPass.Depth->ps;
				Pipeline = IsKept ? CreateForwardPipeline(*GrassDrawn, true, false, Targets, TargetCount, Depth, 0, true, false, nullptr, GrassPass.SurfacePixel._get())
					: CreateForwardPipeline(IsCut ? *GrassPass.Depth : *GrassDrawn, !IsColour, !Overdraw && IsColour && GrassPass.Depth, Targets, TargetCount, Depth, Overdraw, true);
			}

			if (Pipeline)
			{
				GPU_EVENT(SceneGrassDraw);
				Api.CmdSetPipeline(Commands, *Pipeline);

				if (IsCoarse)
				{
					const nri::ShadingRateDesc Rate = { IsCutPass && GrassPass.Depth ? nri::ShadingRate::FRAGMENT_SIZE_2X2 : nri::ShadingRate::FRAGMENT_SIZE_1X1, nri::ShadingRateCombiner::KEEP, nri::ShadingRateCombiner::KEEP };
					Api.CmdSetShadingRate(Commands, Rate);
				}

				// The reference of the dynamic frame's grass: the blender's own leaves half as much of a blade
				Cmd.SetAlphaRef(u32(ps_r2_def_aref_quality));
				nri::SetRootDescriptorDesc Material = {};
				Material.rootDescriptorIndex = ForwardMaterialRoot;
				Material.descriptor = ForwardBlockViews[ForwardMaterialRoot];
				Material.offset = StreamApi.StreamConstantData(*Streamer, &Cmd.Constants.Read<CBMaterial>(), sizeof(CBMaterial));
			Api.CmdSetRootDescriptor(Commands, Material);
				Api.CmdSetIndexBuffer(Commands, *GrassIndices, 0, nri::IndexType::UINT16);

				// A draw a model and mesh: the whole one, then the far one
				for (u32 Bin = 0; Bin < GrassForwardBlock.counts.y; ++Bin)
				{
					for (u32 Far = 0; Far < (GrassForwardBlock.bins[Bin].far_clusters ? 2u : 1u); ++Far)
					{
						const u32 Words[2] = { Far ? GrassForwardBlock.counts.x + GrassForwardBlock.bins[Bin].first * SCENE_GRASS_COPIES : GrassForwardBlock.bins[Bin].first, Bin };
						std::memcpy(ForwardBinBlock.data(), Words, sizeof(Words));
						nri::SetRootDescriptorDesc Root = {};
						Root.rootDescriptorIndex = ForwardBinRoot;
						Root.descriptor = ForwardBlockViews[ForwardBinRoot];
						Root.offset = StreamApi.StreamConstantData(*Streamer, ForwardBinBlock.data(), u32(ForwardBinBlock.size()));
						Api.CmdSetRootDescriptor(Commands, Root);
						Api.CmdDrawIndexedIndirect(Commands, *Grass.Args.Buffer, GrassArgBytes + (Bin * 2 + Far) * sizeof(nri::DrawIndexedDesc), 1, sizeof(nri::DrawIndexedDesc), nullptr, 0);
					}
				}
		}
		}
	}

	// The cards the cards' cull kept, every one in both draws (Parts 8 and 16): cut out where the graph drew its cards,
	// then blended over them where they fade in
	for (u32 Kind = 0; Kind < 2; ++Kind)
	{
		const SPass* Pass = CardPasses[Kind]._get();
		nri::Pipeline*& Pipeline = CardPipelines[Kind];

		if (!(Parts & (8u << Kind)) || !IsColour || IsFace || !Pass || CardFrame != RFrame.dwFrame)
		{
			continue;
		}

		Pipeline = Pipeline ? Pipeline : CreateForwardPipeline(*Pass, false, false, Targets, TargetCount, Depth);

		if (!Pipeline)
		{
			continue;
		}

		GPU_EVENT(SceneCards);
		Api.CmdSetPipeline(Commands, *Pipeline);

		if (IsCoarse)
		{
			const nri::ShadingRateDesc Rate = { nri::ShadingRate::FRAGMENT_SIZE_1X1, nri::ShadingRateCombiner::KEEP, nri::ShadingRateCombiner::KEEP };
			Api.CmdSetShadingRate(Commands, Rate);
		}

		Cmd.SetAlphaRef(u32(Pass->state->state->AlphaRef));
		nri::SetRootDescriptorDesc Material = {};
		Material.rootDescriptorIndex = ForwardMaterialRoot;
		Material.descriptor = ForwardBlockViews[ForwardMaterialRoot];
		Material.offset = StreamApi.StreamConstantData(*Streamer, &Cmd.Constants.Read<CBMaterial>(), sizeof(CBMaterial));
		Api.CmdSetRootDescriptor(Commands, Material);

		*reinterpret_cast<u32*>(ForwardBinBlock.data()) = CardStart;
		nri::SetRootDescriptorDesc Root = {};
		Root.rootDescriptorIndex = ForwardBinRoot;
		Root.descriptor = ForwardBlockViews[ForwardBinRoot];
		Root.offset = StreamApi.StreamConstantData(*Streamer, ForwardBinBlock.data(), u32(ForwardBinBlock.size()));
		Api.CmdSetRootDescriptor(Commands, Root);
		Api.CmdDrawIndirect(Commands, *CardArgs.Buffer, 0, 1, sizeof(nri::DrawDesc), nullptr, 0);
	}

	// The sorted list's segments, in its order: neighbours of one pass, an instance a cluster
	for (u32 Index = 0; Index < ((Parts & 32) ? SortedSegments.size() : 0); ++Index)
	{
		const SortedSegment& Segment = SortedSegments[Index];
		nri::Pipeline*& Pipeline = SortedPipeline(Segment.Pass, Samples);
		Pipeline = Pipeline ? Pipeline : CreateForwardPipeline(*Segment.Pass, false, false, Targets, TargetCount, Depth, 0, false, true);

		if (!Pipeline)
		{
			static bool IsLogged = false;

			if (!IsLogged)
			{
				IsLogged = true;
				Msg("! [scene] no pipeline for a sorted surface (ps %s): it is not drawn", Segment.Pass->ps ? Segment.Pass->ps->cName.c_str() : "none");
			}

			continue;
		}

		Api.CmdSetPipeline(Commands, *Pipeline);
		Cmd.SetAlphaRef(u32(Segment.Pass->state->state->AlphaRef));
		Cmd.Constants.Write<CBMaterial>().dt_params.w = 1.0f / r__dtex_range;
		nri::SetRootDescriptorDesc Material = {};
		Material.rootDescriptorIndex = ForwardMaterialRoot;
		Material.descriptor = ForwardBlockViews[ForwardMaterialRoot];
		Material.offset = StreamApi.StreamConstantData(*Streamer, &Cmd.Constants.Read<CBMaterial>(), sizeof(CBMaterial));
		Api.CmdSetRootDescriptor(Commands, Material);

		*reinterpret_cast<u32*>(ForwardBinBlock.data()) = SortedStart + Segment.Start;
		nri::SetRootDescriptorDesc Root = {};
		Root.rootDescriptorIndex = ForwardBinRoot;
		Root.descriptor = ForwardBlockViews[ForwardBinRoot];
		Root.offset = StreamApi.StreamConstantData(*Streamer, ForwardBinBlock.data(), u32(ForwardBinBlock.size()));
		Api.CmdSetRootDescriptor(Commands, Root);
		Api.CmdDraw(Commands, { SCENE_CLUSTER_TRIANGLES * 3, Segment.Count, 0, 0 });
	}

	// The old backend records into the same list: the rate does not stay with it
	if (IsCoarse)
	{
		const nri::ShadingRateDesc Rate = { nri::ShadingRate::FRAGMENT_SIZE_1X1, nri::ShadingRateCombiner::KEEP, nri::ShadingRateCombiner::KEEP };
		Api.CmdSetShadingRate(Commands, Rate);
	}

	Api.CmdEndRendering(Commands);

	if (IsColour && !IsFace && (Parts & 7) && !(Parts & 1024))
	{
		Move(Commands, Feedback, nri::AccessBits::COPY_SOURCE, nri::StageBits::COPY);
		Api.CmdCopyBuffer(Commands, *FeedbackCopies[RFrame.dwFrame % FeedbackFrames], 0, *Feedback.Buffer, 0, FeedbackBytes);
	}

	// The counts of the bins the frame copied: a bin a model joined with since moved the rest
	CullStatsBins = CullStatsStep == 1 ? u32(Bins.size()) : CullStatsBins;
	const u64 CullStatsBytes = (SCENE_COUNTER_WORDS + CullStatsBins * 8) * sizeof(u32);

	if (IsColour && !IsFace && IsBinned && CullStatsStep == 1)
	{
		if (!CullStatsCopy)
		{
			nri::BufferDesc CopyDesc = {};
			CopyDesc.size = (SCENE_COUNTER_WORDS + BinLimit * 8) * sizeof(u32) + GrassDrawBytes;
			Api.CreateCommittedBuffer(*GRHI->Nri->Device, nri::MemoryLocation::HOST_READBACK, 0.0f, CopyDesc, CullStatsCopy);
		}

		Move(Commands, Counters, nri::AccessBits::COPY_SOURCE, nri::StageBits::COPY);
		Api.CmdCopyBuffer(Commands, *CullStatsCopy, 0, *Counters.Buffer, 0, CullStatsBytes);
		CullStatsBlades = Grass.Args.Buffer ? GrassForwardBlock.counts.x : 0;
		const bool IsTaken = ModelFrame == RFrame.dwFrame;
		CullStatsModels[0] = IsTaken ? ModelOpaque : 0;
		CullStatsModels[1] = IsTaken ? ModelRuns : 0;
		CullStatsModels[2] = 0;
		CullStatsMainPackets = MainPackets;
		CullStatsShaders = IsTaken ? ShadersLeft : decltype(ShadersLeft)();
		CopySortedCounts();

		for (u32 Hud = 0; Hud < 2; ++Hud)
		{
			for (u32 Reason = 0; Reason < u32(ModelLeft::Count); ++Reason)
			{
				CullStatsLeft[Hud][Reason] = IsTaken ? ModelsLeft[Hud][Reason] : 0;
				CullStatsModels[2] += Hud || !Reason ? 0 : CullStatsLeft[Hud][Reason];
			}
		}

		for (u32 Slot = 0; Slot < SCENE_CASTER_VIEWS; ++Slot)
		{
			CullStatsCasters[Slot] = CascadeCounts[Slot];
		}

		// The lamps, the rain map and the spot shadow cache can come later in the frame: the whole frame before is taken
		for (u32 Kind = 0; Kind < u32(ShadowKind::Count); ++Kind)
		{
			const KindCounts& Counts = KindsLeft[Kind];
			CullStatsKinds[Kind] = Counts.Frame == RFrame.dwFrame ? Counts.Last : Counts.Frame + 1 == RFrame.dwFrame ? Counts.Now : CasterCounts();
		}

		if (CullStatsBlades)
		{
			Move(Commands, Grass.Args, nri::AccessBits::COPY_SOURCE, nri::StageBits::COPY);
			Api.CmdCopyBuffer(Commands, *CullStatsCopy, CullStatsBytes, *Grass.Args.Buffer, 0, GrassDrawBytes);
		}

		CullStatsFrame = RFrame.dwFrame;
		CullStatsStep = 2;
	}
	else if (CullStatsStep == 2 && RFrame.dwFrame - CullStatsFrame > 3)
	{
		// By the order of a bin (opaque, terrain, cut out, blended) and the part of the frame that drew it: what the frame
		// before showed, and what the depth of that did not hide
		PROF_EVENT("CullStatsReadback");
		u32 Drawn[4][2] = {};
		const u32* Words = static_cast<const u32*>(Api.MapBuffer(*CullStatsCopy, 0, CullStatsBytes + GrassDrawBytes));
		const nri::DrawIndexedDesc* Draws = reinterpret_cast<const nri::DrawIndexedDesc*>(reinterpret_cast<const u8*>(Words) + CullStatsBytes + GrassArgBytes);
		u32 Blades = 0;

		for (u32 Draw = 0; CullStatsBlades && Draw < SCENE_GRASS_BINS * 2; ++Draw)
		{
			Blades += Draws[Draw].instanceNum;
		}

		u32 ModelRunsDrawn = 0;

		for (u32 Part = 0; Part < 2; ++Part)
		{
			for (u32 Index = 0; Index < CullStatsBins; ++Index)
			{
				const u32 Count = Words[SCENE_COUNTER_WORDS + (Index + Part * CullStatsBins) * 4 + SCENE_COUNTER_CLUSTERS];
				(Index < LevelBinCount ? Drawn[Bins[Index].Order == 4 ? 3u : std::min(Bins[Index].Order, 2u)][Part] : ModelRunsDrawn) += Count;
			}
		}

		Api.UnmapBuffer(*CullStatsCopy);
		Msg("* [scene] forward cull: clusters drawn of %u: opaque %u + %u, terrain %u + %u, cut out %u + %u, blended %u + %u (seen the frame before + new); grass blades %u of %u; models taken %u, runs drawn %u of %u, mesh parts left in the graph %u",
			ClusterCount, Drawn[0][0], Drawn[0][1], Drawn[1][0], Drawn[1][1], Drawn[2][0], Drawn[2][1], Drawn[3][0], Drawn[3][1], Blades, CullStatsBlades, CullStatsModels[0], ModelRunsDrawn, CullStatsModels[1], CullStatsModels[2]);

		string512 Left[2] = {};

		for (u32 Hud = 0; Hud < 2; ++Hud)
		{
			for (u32 Reason = 1; Reason < u32(ModelLeft::Count); ++Reason)
			{
				string64 Part;
				xr_sprintf(Part, "%s%s %u", Reason > 1 ? ", " : "", ModelLeftNames[Reason], CullStatsLeft[Hud][Reason]);
				xr_strcat(Left[Hud], Part);
			}
		}

		string1024 Shaders = {};

		for (size_t Index = 0; Index < std::min<size_t>(CullStatsShaders.size(), 8); ++Index)
		{
			string128 Part;
			xr_sprintf(Part, "%s %s %u", Index ? "," : "", CullStatsShaders[Index].first.size() ? *CullStatsShaders[Index].first : "none", CullStatsShaders[Index].second);
			xr_strcat(Shaders, Part);
		}

		Msg("* [scene] main view left by reason: the graph holds %u static packets; level visuals %s", CullStatsMainPackets, MainLeftLine);
		Msg("* [scene] mesh parts left in the graph by reason: %s; %u model shaders joined, %u could not; no element for a moving object by vertex shader:%s", Left[0], u32(ModelBinIds.size()), u32(ModelRejects.size()), Shaders);
		Msg("* [scene] HUD parts taken %u, left in the graph by reason: %s", CullStatsLeft[1][u32(ModelLeft::Taken)], Left[1]);
		LogSortedCounts();
		// Why the moving casters of some views stayed, summed
		const auto Reasons = [](const CasterCounts* Views, u32 Count, string512& Line)
		{
			Line[0] = 0;

			for (u32 Reason = 1; Reason < u32(CasterLeft::Count); ++Reason)
			{
				u32 Sum = 0;

				for (u32 View = 0; View < Count; ++View)
				{
					Sum += Views[View].Left[Reason];
				}

				string64 Part;
				xr_sprintf(Part, "%s%s %u", Reason > 1 ? ", " : "", CasterLeftNames[Reason], Sum);
				xr_strcat(Line, Part);
			}
		};

		string512 Casters;
		Reasons(CullStatsCasters, SCENE_CASTER_VIEWS, Casters);
		const CasterCounts* Sun = CullStatsCasters;
		Msg("* [scene] sun cascade casters taken %u / %u / %u, left in the graph by reason: %s; the cascades' graphs drew static %u / %u / %u and moving %u / %u / %u packets themselves",
			Sun[0].Left[0], Sun[1].Left[0], Sun[2].Left[0], Casters, Sun[0].Drawn[0], Sun[1].Drawn[0], Sun[2].Drawn[0], Sun[0].Drawn[1], Sun[1].Drawn[1], Sun[2].Drawn[1]);

		const char* const KindNames[u32(ShadowKind::Count)] = { "lamp shadow views", "rain map", "spot shadow cache" };

		for (u32 Kind = 0; Kind < u32(ShadowKind::Count); ++Kind)
		{
			const CasterCounts& Counts = CullStatsKinds[Kind];
			Reasons(&Counts, 1, Casters);
			string512 Entries = "";

			if (Kind == u32(ShadowKind::SpotCache))
			{
				RImplementation.Target->DescribeSpotShadowCache(Entries);
			}

			Msg("* [scene] %s in the frame before: moving casters taken %u, left in the graph by reason: %s; the graphs drew static %u and moving %u packets of the first pass and %u of the translucent pass themselves%s",
				KindNames[Kind], Counts.Left[0], Casters, Counts.Drawn[0], Counts.Drawn[1], Counts.Drawn[2], Entries);
		}

		CullStatsStep = 0;
	}

	EndNriCommands(&Commands);

	for (u32 Target = 0; Target < 5; ++Target)
	{
		if (TargetViews[Target])
		{
			Api.DestroyDescriptor(TargetViews[Target]);
			Api.DestroyTexture(Wrapped[Target]);
		}
	}

	Cmd.RestoreInput();
	return true;
}

void GpuScene::EndFrame()
{
	if (Streamer)
	{
		StreamApi.EndStreamerFrame(*Streamer);
	}

	if (ClusterCount)
	{
		WrapPages();
	}
}

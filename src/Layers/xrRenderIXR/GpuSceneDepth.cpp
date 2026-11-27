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
// The shadow views cast their moving objects as records of the frame's models
bool TakesCasters()
{
	return ModelCapacity && ps_r_scene_models && !DevicePtr->IsEditorMode();
}

CasterCounts& CountsOf(ShadowKind Kind)
{
	KindCounts& Counts = KindsLeft[u32(Kind)];

	if (Counts.Frame != RFrame.dwFrame)
	{
		Counts.Last = Counts.Frame + 1 == RFrame.dwFrame ? Counts.Now : CasterCounts();
		Counts.Now = {};
		Counts.Frame = RFrame.dwFrame;
	}

	return Counts.Now;
}

CasterLeft CasterPass(const SPass& Pass, bool IsPart, u32& Alpha)
{
	const u32 BaseSlot = ShaderBindings::Texture("s_base");
	// A level shader on a rigid mesh draws its vertices as the model shader does
	const bool IsShader = DepthState && Pass.vs && Pass.ps && Pass.state && !(Pass.gs && Pass.gs->gs) && !(Pass.hs && Pass.hs->sh) && !(Pass.ds && Pass.ds->sh) &&
		IsNamed(Pass.ps->cName, "shadow_base") && (IsNamed(Pass.vs->cName, "shadow_model") || (!IsPart && IsNamed(Pass.vs->cName, "shadow_base")));
	Alpha = SCENE_TEXTURE_ABSENT;

	// Cut out by its texture: tested by the alpha the level's casters of that texture are tested by
	if (IsShader && Pass.ps->Texture2DSlots == (1ull << BaseSlot) && Pass.T && HasAlpha)
	{
		for (const auto& [Stage, Texture] : *Pass.T)
		{
			const auto Found = Stage == CTexture::rstPixel + BaseSlot ? AlphaSlices.find(Texture._get()) : AlphaSlices.end();
			Alpha = Found != AlphaSlices.end() ? Found->second : Alpha;
		}
	}

	const GfxPipelineState* State = Pass.state ? Pass.state->state : nullptr;
	return !IsShader ? CasterLeft::Shader
		: !IsBatchState(Pass) || (Pass.ps->Texture2DSlots && (State->Blend.RenderTarget[0].BlendEnable || State->Depth.DepthWriteMask != RHI_DEPTH_WRITE_MASK_ALL)) ? CasterLeft::State
		: Pass.ps->Texture2DSlots && Alpha == SCENE_TEXTURE_ABSENT ? CasterLeft::Alpha : CasterLeft::Taken;
}

CasterLeft MakeCaster(const SPass& Pass, dxRender_Visual* Visual, const Fmatrix& Matrix, bool IsDetailed, bool HasRoom, u32& Runs, Hlsl::SceneModel& Record, CSkeletonX*& Skin)
{
	const IRender_Mesh* Mesh = dynamic_cast<const IRender_Mesh*>(Visual);
	const u32 Type = Visual->Type;
	const bool IsPart = Type == MT_SKELETON_GEOMDEF_ST || Type == MT_SKELETON_GEOMDEF_PM;
	CSkeletonX* Owner = IsPart ? static_cast<CSkeletonX*>(Visual) : nullptr;
	u32 Alpha = SCENE_TEXTURE_ABSENT;
	const CasterLeft Shaded = !Mesh || (!IsPart && Type != MT_NORMAL && Type != MT_PROGRESSIVE) ? CasterLeft::Kind : CasterPass(Pass, IsPart, Alpha);

	// A part draws the detail the main view picked last
	const FProgressive* Progressive = Owner && Owner->progressive_mesh ? static_cast<const FProgressive*>(Owner) : Type == MT_PROGRESSIVE ? dynamic_cast<const FProgressive*>(Visual) : nullptr;
	const FSlideWindow* Window = !Progressive ? nullptr : Owner || !IsDetailed ? Progressive->GetLastWindow() : Progressive->GetWindow();
	const u32 Triangles = Window ? Window->num_tris : Mesh ? Mesh->dwPrimitives : 0;
	const u32 Count = (Triangles + SCENE_CLUSTER_TRIANGLES - 1) / SCENE_CLUSTER_TRIANGLES;
	const u32 Page = Mesh ? Mesh->Alloc.Page : PageSlots;

	const CasterLeft Why = Shaded != CasterLeft::Taken ? Shaded
		: Page >= PageSlots || !IndexPageBuffers[Page] ? CasterLeft::Page
		: Owner && (!Owner->Parent || !Owner->visible) ? CasterLeft::Hidden
		: !Triangles || Count > SCENE_DYNAMIC_RUNS ? CasterLeft::Triangles
		: !HasRoom || Count > Runs ? CasterLeft::Caps : CasterLeft::Taken;

	if (Why != CasterLeft::Taken)
	{
		return Why;
	}

	Fmatrix World = Matrix;
	Skin = Owner && Owner->IsSkinned() ? Owner : nullptr;

	if (Skin)
	{
		RImplementation.Skinning.Request(*Owner, Matrix);
	}
	else if (Owner)
	{
		Fmatrix Old;
		RImplementation.Skinning.Rigid(*Owner->Parent, Matrix, u16(Owner->RMS_boneid), World, Old);
	}

	Record.world = World;
	Record.index_start = Mesh->iBase + (Window ? Window->offset : 0);
	Record.triangle_count = Triangles;
	Record.vertices = Mesh->vBase;
	Record.source = Page;
	Record.alpha = Alpha;
	Runs -= Count;
	return CasterLeft::Taken;
}

namespace
{
// The lamp parts queued for one target: where each goes in it, its view, sectors and object
Hlsl::SceneView TileBlock = {};
IRHIDepthStencilView* TileTarget = nullptr;
u32 TileSlice = 0;

nri::Pipeline* CreateDepthPipeline(nri::Format Format, bool IsAlpha, bool IsSpot, bool IsTiled = false, bool IsMesh = false)
{
	static RHIBlob* MeshCode = nullptr;

	if (IsMesh && !MeshCode)
	{
		MeshCode = RImplementation.CompileStage("common\\scene_depth", ".ms.hlsl", "ms_5_0");
	}

	if (IsMesh && !MeshCode)
	{
		return nullptr;
	}

	static const nri::CullMode Culls[] = { nri::CullMode::NONE, nri::CullMode::NONE, nri::CullMode::FRONT, nri::CullMode::BACK };
	const RHIRasterizerDesc& Raster = DepthState->state->Raster;

	nri::ShaderDesc Shaders[2] = {};
	ShaderExternalMap Defines;

	if (IsAlpha)
	{
		Defines["USE_AREF"] = "1";
		AlphaPixels = DEV->_CreatePS("common\\scene_depth");
		Shaders[1] = ShaderCode(AlphaPixels->ps, nri::StageBits::FRAGMENT_SHADER);
	}

	if (IsTiled)
	{
		Defines["USE_TILES"] = "1";
	}

	if (IsMesh)
	{
		Shaders[0].stage = nri::StageBits::MESH_SHADER;
		Shaders[0].bytecode = MeshCode->GetBufferPointer();
		Shaders[0].size = MeshCode->GetBufferSize();
	}
	else
	{
		ref_vs& Vertex = DepthShaders[u32(IsAlpha) | u32(IsTiled) << 1];
		Vertex = DEV->_CreateVS("common\\scene_depth", Defines);
		Shaders[0] = ShaderCode(Vertex->vs, nri::StageBits::VERTEX_SHADER);
	}

	nri::GraphicsPipelineDesc Desc = {};
	Desc.pipelineLayout = IsMesh ? MeshLayout : DrawLayout;
	Desc.inputAssembly.topology = nri::Topology::TRIANGLE_LIST;
	// A lamp's map is pushed back by its slope: a wall the lamp grazes shadowed itself in stripes a texel of the map
	// apart, which no lift of the receiver short of tens of centimetres closes
	Desc.rasterization.depthBias = IsSpot ? nri::DepthBiasDesc{ 0.0f, 0.0f, 2.0f } : nri::DepthBiasDesc{ float(Raster.DepthBias), Raster.DepthBiasClamp, Raster.SlopeScaledDepthBias };
	// A spot shadow map overrides these two (SetupSpotShadowTarget)
	Desc.rasterization.cullMode = IsSpot ? nri::CullMode::BACK : Culls[u32(Raster.CullMode)];
	Desc.rasterization.frontCounterClockwise = Raster.FrontCounterClockwise != 0;
	Desc.rasterization.depthClamp = !Raster.DepthClipEnable;
	Desc.outputMerger.depthStencilFormat = Format;
	Desc.outputMerger.depth.compareOp = IsSpot ? nri::CompareOp::LESS_EQUAL : Compare[u32(DepthState->state->Depth.DepthFunc)];
	Desc.outputMerger.depth.write = true;
	Desc.shaders = Shaders;
	Desc.shaderNum = IsAlpha ? 2 : 1;

	nri::Pipeline* Pipeline = nullptr;
	PROF_EVENT("ScenePipelineCreate");
	Api.CreateGraphicsPipeline(*GRHI->Nri->Device, Desc, Pipeline);
	return Pipeline;
}

DepthPipelines& FindPipelines(nri::Format Format, bool IsSpot)
{
	auto Found = std::find_if(Pipelines.begin(), Pipelines.end(), [Format, IsSpot](const DepthPipelines& Item) { return Item.Format == Format && Item.IsSpot == IsSpot; });

	if (Found == Pipelines.end())
	{
		Pipelines.push_back({ Format, IsSpot, CreateDepthPipeline(Format, false, IsSpot), HasAlpha ? CreateDepthPipeline(Format, true, IsSpot) : nullptr });
		Found = Pipelines.end() - 1;
		Found->Mesh = MeshLayout ? CreateDepthPipeline(Format, false, IsSpot, false, true) : nullptr;
	}

	return *Found;
}

// What a shadow view's graph draws itself
void CountDrawn(CasterCounts& Counts, const R_dsgraph_structure& Graph, bool IsTranslucent = true)
{
	Counts.Drawn[0] += u32(Graph.StaticPasses[0].size());
	Counts.Drawn[1] += u32(Graph.DynamicPasses[0].size());
	Counts.Drawn[2] += IsTranslucent ? u32(Graph.StaticPasses[1].size() + Graph.DynamicPasses[1].size() + Graph.mapSorted.size()) : 0u;
}

// The geometry pages belong to the old backend, which has to hand them over readable
void ReadSceneBuffers()
{
	for (u32 Page = 0; Page < RImplementation.Geometry.PageCount(); ++Page)
	{
		if (IRHIBuffer* Source = RImplementation.Geometry.VertexBuffer(Page))
		{
			GRHI->DevicePtr->ReadExternalBuffer(Source);
			GRHI->DevicePtr->ReadExternalBuffer(RImplementation.Geometry.IndexBuffer(Page));
		}
	}
}

RHIViewport DrawnArea(IRHISurface* Surface, const RHIViewport* Area)
{
	if (Area)
	{
		return *Area;
	}

	u32 Width = Surface->GetWidth();
	u32 Height = Surface->GetHeight();
	ViewSize(Width, Height);
	return { 0.0f, 0.0f, float(Width), float(Height), 0.0f, 1.0f };
}

DepthBatch& KeepBatch(u32 View, IRHIDepthStencilView* Target, u32 Slice, const RHIViewport* Area, nri::Pipeline* Opaque, nri::Pipeline* Alpha)
{
	DepthBatch& Batch = OwnViews[View].Batches.emplace_back();
	Batch.Target = Target;
	Batch.Slice = Slice;
	Batch.Drawn = DrawnArea(Target->GetSurface(), Area);
	Batch.Opaque = Opaque;
	Batch.Alpha = Alpha;
	return Batch;
}
} // namespace

CullBlocks StreamCull(const Hlsl::SceneView& Block)
{
	CullBlocks Blocks;
	Blocks.Range = StreamApi.StreamConstantData(*Streamer, &Block, sizeof(Block));
	Blocks.BandCount = (Block.counts.y & SCENE_VIEW_BINS) ? 1 + SCENE_BANDS : 1;

	for (u32 Band = 0; Band < Blocks.BandCount; ++Band)
	{
		Hlsl::SceneView Banded = Block;
		Banded.thresholds.w = float(Band) - 1.0f;
		Blocks.Bands[Band] = StreamApi.StreamConstantData(*Streamer, &Banded, sizeof(Banded));
	}

	return Blocks;
}

// The two levels of the cull into the view's lists. Touches only those lists' states
void RecordCull(nri::CommandBuffer& Commands, const ViewLists& Lists, const Hlsl::SceneView& Block, const CullBlocks& Blocks)
{
	// The second part of the main view leaves the first one's draw arguments as they are: the colour pass reads both
	const bool IsSecond = (Block.counts.y & SCENE_VIEW_UNSEEN) != 0;
	const u64 Half = (SCENE_COUNTER_WORDS + Bins.size() * 4) * sizeof(u32);
	Move(Commands, Lists.Counters, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);

	// A frame with no bins (the advanced one) has the counters alone: a zero of no bytes past them is read as "all
	// that is left" and runs off the buffer's end, which closes the command list with an error on D3D12
	if (Bins.empty())
	{
		Api.CmdZeroBuffer(Commands, *Lists.Counters.Buffer, 0, SCENE_COUNTER_WORDS * sizeof(u32));
	}
	// A face of the cube has its draw arguments after the main view's
	else if (Block.second.x >= Bins.size() * 2)
	{
		Api.CmdZeroBuffer(Commands, *Lists.Counters.Buffer, 0, SCENE_COUNTER_WORDS * sizeof(u32));
		Api.CmdZeroBuffer(Commands, *Lists.Counters.Buffer, (SCENE_COUNTER_WORDS + u64(Block.second.x) * 4) * sizeof(u32), Bins.size() * 4 * sizeof(u32));
	}
	else
	{
		Api.CmdZeroBuffer(Commands, *Lists.Counters.Buffer, IsSecond ? Half : 0, IsSecond ? Bins.size() * 4 * sizeof(u32) : Half);
	}

	Move(Commands, Lists.CullGroups, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
	Api.CmdZeroBuffer(Commands, *Lists.CullGroups.Buffer, 0, 8 * sizeof(u32));
	Move(Commands, Lists.CullGroups, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
	// The counts of its two lists
	Move(Commands, Lists.Chunks, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
	Api.CmdZeroBuffer(Commands, *Lists.Chunks.Buffer, 0, 2 * sizeof(u32));
	Move(Commands, Lists.Chunks, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
	Move(Commands, Lists.Counters, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
	Move(Commands, Lists.Visible, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
	Move(Commands, Lists.VisibleAlpha, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);

	// The runs of clusters the view may see, then only their clusters: most of a level is outside any one view
	Api.CmdSetDescriptorPool(Commands, *Lists.Pool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *RangeLayout);
	Api.CmdSetPipeline(Commands, *RangePipeline);
	SetBlocks(Commands, Blocks.Range);

	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = Lists.RangeSet;
	Api.CmdSetDescriptorSet(Commands, SetDesc);
	Api.CmdDispatch(Commands, { (RangeCount + 63) / 64, 1, 1 });

	Move(Commands, Lists.CullGroups, nri::AccessBits::ARGUMENT_BUFFER, nri::StageBits::INDIRECT);
	Move(Commands, Lists.Chunks, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::COMPUTE_SHADER);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *CullLayout);
	Api.CmdSetPipeline(Commands, *CullPipeline);
	SetDesc.descriptorSet = Lists.CullSet;
	Api.CmdSetDescriptorSet(Commands, SetDesc);

	for (u32 Band = 0; Band < Blocks.BandCount; ++Band)
	{
		SetBlocks(Commands, Blocks.Bands[Band]);
		Api.CmdDispatchIndirect(Commands, *Lists.CullGroups.Buffer, Band ? 4 * sizeof(u32) : 0);
	}
}

namespace
{
// The view's two indirect draws, of what is solid and of what its texture cuts out, into the slice of its target
void RecordRaster(nri::CommandBuffer& Commands, const ViewLists& Lists, nri::Descriptor* SliceView, const RHIViewport& Drawn, u32 View, nri::Pipeline* Opaque, nri::Pipeline* Alpha)
{
	Move(Commands, Lists.Counters, nri::AccessBits::ARGUMENT_BUFFER, nri::StageBits::INDIRECT);
	Move(Commands, Lists.Visible, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::VERTEX_SHADER);
	Move(Commands, Lists.VisibleAlpha, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::VERTEX_SHADER);

	const bool IsMesh = std::any_of(Pipelines.begin(), Pipelines.end(), [Opaque](const DepthPipelines& Item) { return Item.Mesh == Opaque; });
	Api.CmdSetDescriptorPool(Commands, *Lists.Pool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::GRAPHICS, IsMesh ? *MeshLayout : *DrawLayout);
	Api.CmdSetPipeline(Commands, *Opaque);
	SetBlocks(Commands, View);

	const nri::Viewport Viewport = { Drawn.TopLeftX, Drawn.TopLeftY, Drawn.Width, Drawn.Height, Drawn.MinDepth, Drawn.MaxDepth };
	const nri::Rect Scissor = { s16(Drawn.TopLeftX), s16(Drawn.TopLeftY), nri::Dim_t(Drawn.Width), nri::Dim_t(Drawn.Height) };
	Api.CmdSetViewports(Commands, &Viewport, 1);
	Api.CmdSetScissors(Commands, &Scissor, 1);

	nri::RenderingDesc Rendering = {};
	Rendering.depth.descriptor = SliceView;
	Rendering.depth.loadOp = nri::LoadOp::LOAD;
	Rendering.depth.storeOp = nri::StoreOp::STORE;
	Api.CmdBeginRendering(Commands, Rendering);

	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = Lists.DrawSet;
	Api.CmdSetDescriptorSet(Commands, SetDesc);

	// A group an entry of the list: the three words from the list's count (scene_cull)
	if (IsMesh)
	{
		MeshApi.CmdDrawMeshTasksIndirect(Commands, *Lists.Counters.Buffer, SCENE_COUNTER_CLUSTERS * sizeof(u32), 1, sizeof(nri::DrawMeshTasksDesc), nullptr, 0);
	}
	else
	{
		Api.CmdDrawIndirect(Commands, *Lists.Counters.Buffer, 0, 1, sizeof(nri::DrawDesc), nullptr, 0);
	}

	if (Alpha)
	{
		if (IsMesh)
		{
			Api.CmdSetPipelineLayout(Commands, nri::BindPoint::GRAPHICS, *DrawLayout);
			SetBlocks(Commands, View);
		}

		Api.CmdSetPipeline(Commands, *Alpha);
		SetDesc.descriptorSet = Lists.AlphaSet;
		Api.CmdSetDescriptorSet(Commands, SetDesc);
		Api.CmdDrawIndirect(Commands, *Lists.Counters.Buffer, SCENE_COUNTER_ALPHA * sizeof(u32), 1, sizeof(nri::DrawDesc), nullptr, 0);
	}

	Api.CmdEndRendering(Commands);
}
} // namespace
} // namespace GpuSceneInternal

void GpuScene::CountGraph(ShadowKind Kind, const R_dsgraph_structure& Graph, bool IsTranslucent)
{
	CountDrawn(CountsOf(Kind), Graph, IsTranslucent);
}

bool GpuScene::IsShadowBinned(const R_dsgraph_structure& Graph) const
{
	return ClusterCount && !ShadowLeftovers[!Graph.pmask[1] && !Graph.pmask[2]];
}

bool GpuScene::DrawDepth(GfxCmdList& Cmd, R_dsgraph_structure& Graph, const DepthDraw& Draw)
{
	const u32 Format = NriDepthFormat(Draw.Target->GetSurface()->GetFormat());
	if (!ClusterCount || !Format)
	{
		return false;
	}

	const DepthPipelines& Found = FindPipelines(nri::nriConvertDXGIFormatToNRI(Format), Draw.IsSpot);

	if (KeptView < OwnViewCount)
	{
		MakeCullBlock(Graph, Draw, 0, 0, KeepBatch(KeptView, Draw.Target, Draw.Slice, Draw.Area, Found.Solid(), Found.Alpha).Block);
		return true;
	}

	GPU_EVENT(SceneDepth);
	Cull(Graph, Draw, 0);
	Raster(Cmd, Draw.Target, Draw.Slice, Draw.Area, Found.Solid(), Found.Alpha);
	return true;
}

bool GpuScene::PrepareView(GfxCmdList& Cmd, R_dsgraph_structure& Graph, const DepthDraw& Draw, u32 View)
{
	const u32 Format = NriDepthFormat(Draw.Target->GetSurface()->GetFormat());

	if (!ClusterCount || !Format || !OwnViews[View].CullSet)
	{
		return false;
	}

	// Its moving casters are the whole of the frame's table, which its cull tests by their flags
	CountDrawn(CascadeCounts[View], Graph);
	const DepthPipelines& Found = FindPipelines(nri::nriConvertDXGIFormatToNRI(Format), Draw.IsSpot);
	MakeCullBlock(Graph, Draw, TakesCasters() && ModelFrame == RFrame.dwFrame ? u32(ModelRecords.size()) : 0u, 0, KeepBatch(View, Draw.Target, Draw.Slice, Draw.Area, Found.Solid(), Found.Alpha).Block);
	return true;
}

void GpuScene::KeepDepth(u32 View)
{
	KeptView = View < OwnViewCount && OwnViews[View].CullSet ? View : OwnViewCount;
}

void GpuScene::PrepareViews(GfxCmdList& Cmd)
{
	KeptView = OwnViewCount;

	if (std::all_of(std::begin(OwnViews), std::end(OwnViews), [](const OwnView& Own) { return Own.Batches.empty(); }))
	{
		return;
	}

	PROF_EVENT("SceneViewsPrepare");
	RImplementation.Skinning.Flush(Cmd);
	WrapSkin();
	ReadSceneBuffers();

	// Each target is handed over writable and the shared buffers the culls read readable, after everything the primary
	// thread drew before and before any view is recorded
	for (OwnView& Own : OwnViews)
	{
		const DepthBatch* Before = nullptr;
		nri::CommandBuffer* Commands = nullptr;

		for (DepthBatch& Batch : Own.Batches)
		{
			const bool IsNew = !Before || Before->Target != Batch.Target || Before->Slice != Batch.Slice;
			IRHISurface* Surface = Batch.Target->GetSurface();
			Batch.Blocks = StreamCull(Batch.Block);
			Batch.Texture = IsNew ? WrapNriSurface(Surface, NriDepthFormat(Surface->GetFormat())) : nullptr;
			Batch.SliceView = IsNew ? MakeSliceView(Batch.Texture, Batch.Slice) : Before->SliceView;

			if (IsNew && Commands)
			{
				EndNriCommands(Commands);
			}

			Commands = IsNew ? BeginNriCommands(nullptr, 0, nullptr, 0, Batch.Target) : Commands;
			PrepareCull(*Commands, Batch.Block);
			Before = &Batch;
		}

		if (Commands)
		{
			EndNriCommands(Commands);
		}
	}

	Cmd.RestoreInput();
}

u32 GpuScene::PreparedViews(u32 (&Views)[OwnViewCount]) const
{
	u32 Count = 0;

	for (u32 View = 0; View < OwnViewCount; ++View)
	{
		Views[Count] = View;
		Count += !OwnViews[View].Batches.empty();
	}

	return Count;
}

void GpuScene::RecordView(nri::CommandBuffer& Commands, u32 View)
{
	OwnView& Own = OwnViews[View];
	const ViewLists Lists = Own.Lists();

	for (const DepthBatch& Batch : Own.Batches)
	{
		RecordCull(Commands, Lists, Batch.Block, Batch.Blocks);
		RecordRaster(Commands, Lists, Batch.SliceView, Batch.Drawn, Batch.Blocks.Bands[Batch.Blocks.BandCount - 1], Batch.Opaque, Batch.Alpha);
	}
}

// The cull and the draws of a batch in two passes: on D3D11 a pass that writes a list and then reads it loses the output
void GpuScene::RecordViews(GfxCmdList& Cmd)
{
	// A view of one batch has lists nothing else writes: the culls of all such views go first, in one pass, and their
	// draws follow each other with no cull between them to wait for
	const auto IsAlone = [](const OwnView& Own) { return Own.Batches.size() == 1; };

	if (std::any_of(std::begin(OwnViews), std::end(OwnViews), IsAlone))
	{
		GPU_EVENT(SceneCull);
		nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, nullptr);

		for (OwnView& Own : OwnViews)
		{
			if (IsAlone(Own))
			{
				RecordCull(Commands, Own.Lists(), Own.Batches[0].Block, Own.Batches[0].Blocks);
			}
		}

		EndNriCommands(&Commands);
	}

	for (OwnView& Own : OwnViews)
	{
		const ViewLists Lists = Own.Lists();

		for (const DepthBatch& Batch : Own.Batches)
		{
			const auto Record = [&]
			{
				if (!IsAlone(Own))
				{
					GPU_EVENT(SceneCull);
					nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, nullptr);
					RecordCull(Commands, Lists, Batch.Block, Batch.Blocks);
					EndNriCommands(&Commands);
				}

				nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, Batch.Target);
				RecordRaster(Commands, Lists, Batch.SliceView, Batch.Drawn, Batch.Blocks.Bands[Batch.Blocks.BandCount - 1], Batch.Opaque, Batch.Alpha);
				EndNriCommands(&Commands);
			};

			if (Batch.Block.counts.y & SCENE_VIEW_TILED)
			{
				GPU_EVENT(SceneTiles);
				Record();
			}
			else
			{
				GPU_EVENT(SceneDepth);
				Record();
			}
		}
	}

	Cmd.RestoreInput();
}

void GpuScene::FinishViews()
{
	for (OwnView& Own : OwnViews)
	{
		for (const DepthBatch& Batch : Own.Batches)
		{
			if (Batch.Texture)
			{
				Api.DestroyDescriptor(Batch.SliceView);
				Api.DestroyTexture(Batch.Texture);
			}
		}

		Own.Batches.clear();
	}
}

bool GpuScene::QueueTile(GfxCmdList& Cmd, IRHIDepthStencilView* Target, u32 Slice, const RHIViewport& Area, const GpuTileView& View)
{
	IRHISurface* Surface = Target->GetSurface();

	if (!ClusterCount || !NriDepthFormat(Surface->GetFormat()) || !TakesCasters() || ModelFrame != RFrame.dwFrame || ModelRecords.empty())
	{
		return false;
	}

	if (TileCount && (TileTarget != Target || TileSlice != Slice || TileCount == SCENE_VIEW_TILES))
	{
		DrawTiles(Cmd);
	}

	TileTarget = Target;
	TileSlice = Slice;
	const float Width = float(Surface->GetWidth());
	const float Height = float(Surface->GetHeight());
	TileBlock.tile_views[TileCount] = View.Transform;
	TileBlock.tile_rects[TileCount].set(Area.Width / Width, Area.Height / Height, (2.0f * Area.TopLeftX + Area.Width) / Width - 1.0f, 1.0f - (2.0f * Area.TopLeftY + Area.Height) / Height);
	TileBlock.tile_objects[TileCount] = { View.Object, u32(View.IsOnly), 0, 0 };
	static_assert(sizeof(View.Sectors) == 2 * sizeof(Hlsl::uint4));
	std::memcpy(&TileBlock.tile_sectors[TileCount * 2], View.Sectors, sizeof(View.Sectors));
	++TileCount;
	return true;
}

// A page batch: the view block of up to SCENE_VIEW_TILES lamp parts of one target (their views, rects, sectors and
// objects), its cull over the whole table and its two indirect draws, recorded now or kept for its view (KeepDepth).
// Nothing but NRI: no game call, no graph, no old-backend draw
void GpuScene::DrawTiles(GfxCmdList& Cmd)
{
	if (!TileCount)
	{
		return;
	}

	const nri::Format Typed = nri::nriConvertDXGIFormatToNRI(NriDepthFormat(TileTarget->GetSurface()->GetFormat()));
	DepthPipelines& Found = FindPipelines(Typed, true);

	if (!Found.Tiled)
	{
		Found.Tiled = CreateDepthPipeline(Typed, false, true, true);
		Found.TiledAlpha = HasAlpha ? CreateDepthPipeline(Typed, true, true, true) : nullptr;
	}

	// No clusters: a thread of the cull a record of the table, tested against every tile
	const u32 Translucent = ps_r2_ls_flags_ext.test(R4FLAG_TSHDOWS) ? SCENE_VIEW_TRANSLUCENT : 0u;
	TileBlock.counts = { 0, SCENE_VIEW_TILED | Translucent | (u32(ModelRecords.size()) << SCENE_VIEW_DYNAMIC_SHIFT), 0, 0 };
	TileBlock.second = { 0, TileCount, 0, SCENE_OBJECT_NONE };
	TileBlock.material.x = ps_r2_def_aref_quality / 255.0f;
	TileCount = 0;

	if (KeptView < OwnViewCount)
	{
		KeepBatch(KeptView, TileTarget, TileSlice, nullptr, Found.Tiled, Found.TiledAlpha).Block = TileBlock;
		return;
	}

	GPU_EVENT(SceneTiles);
	Dispatch(TileBlock);
	Raster(Cmd, TileTarget, TileSlice, nullptr, Found.Tiled, Found.TiledAlpha);
}

void GpuScene::Raster(GfxCmdList& Cmd, IRHIDepthStencilView* Target, u32 Slice, const RHIViewport* Area, nri::Pipeline* Opaque, nri::Pipeline* Alpha)
{
	IRHISurface* Surface = Target->GetSurface();
	const u32 Format = NriDepthFormat(Surface->GetFormat());
	WrapSkin();

	// The wrapper of the old backend's surface lives for this pass: targets are bound when recorded
	nri::Texture* Texture = WrapNriSurface(Surface, Format);
	nri::Descriptor* SliceView = MakeSliceView(Texture, Slice);

	ReadSceneBuffers();
	nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, Target);
	RecordRaster(Commands, SharedLists(), SliceView, DrawnArea(Surface, Area), ViewOffset, Opaque, Alpha);
	EndNriCommands(&Commands);

	Api.DestroyDescriptor(SliceView);
	Api.DestroyTexture(Texture);
	Cmd.RestoreInput();
}

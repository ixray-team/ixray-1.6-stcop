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
constexpr u32 GrassItemSize = 48;
constexpr u32 GrassItemPosition = 16;
constexpr u64 GrassTileBytes = SCENE_GRASS_TILES * sizeof(u32);
constexpr u32 GrassTileSize = 32;
// Entries of the visible list per instance the buffers hold: a near instance takes one per cluster of its mesh
constexpr u32 GrassEntries = 2;
// A frame sends its runs once: its shadow views cull the same ones
const u32* GrassRunWords = nullptr;
} // namespace

nri::Descriptor* MakeSliceView(nri::Texture* Texture, u32 Slice)
{
	nri::TextureViewDesc Desc = {};
	Desc.texture = Texture;
	Desc.type = nri::TextureView::DEPTH_STENCIL_ATTACHMENT;
	Desc.mipNum = 1;
	Desc.layerOffset = nri::Dim_t(Slice);
	Desc.layerNum = 1;

	nri::Descriptor* View = nullptr;
	Api.CreateTextureView(Desc, View);
	return View;
}

void ReleaseGrass(GrassSet& Set)
{
	if (!Set.Source)
	{
		return;
	}

	for (nri::Descriptor* View : Set.Views)
	{
		Api.DestroyDescriptor(View);
	}

	Api.DestroyBuffer(Set.Items);
	Api.DestroyBuffer(Set.Args.Buffer);
	Api.DestroyBuffer(Set.Visible.Buffer);
	_RELEASE(Set.Source);
	Set = {};
}

namespace
{
void ReserveGrass(GrassSet& Set, u32 Count)
{
	const u32 Capacity = std::max(Count, std::max(Set.Capacity * 2, 16384u));
	ReleaseGrass(Set);
	Set.Capacity = Capacity;

	RHIBufferDesc Desc = {};
	Desc.Size = Capacity * GrassItemSize;
	Desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
	Desc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
	Desc.StructureByteStride = GrassItemSize;
	Set.Source = GRHI->CreateBuffer(Desc);
	Set.Items = WrapBuffer(Set.Source, GrassItemSize);
	Borrowed.pop_back();

	const nri::BufferUsageBits Storage = nri::BufferUsageBits::SHADER_RESOURCE_STORAGE;
	Set.Args.Buffer = CreateBuffer(GrassDrawBytes + GrassTileBytes, 0, Storage | nri::BufferUsageBits::ARGUMENT, "Scene grass arguments");
	Set.Visible.Buffer = CreateBuffer(u64(Capacity) * GrassEntries * 5 * sizeof(u32), 0, Storage | nri::BufferUsageBits::SHADER_RESOURCE, "Scene visible grass");

	nri::BufferViewDesc View = {};
	View.buffer = Set.Items;
	View.type = nri::BufferView::STRUCTURED_BUFFER;
	View.structureStride = GrassItemSize;
	Api.CreateBufferView(View, Set.Views[0]);
	View = {};
	View.type = nri::BufferView::STORAGE_BUFFER;
	View.format = nri::Format::R32_UINT;
	View.buffer = Set.Args.Buffer;
	Api.CreateBufferView(View, Set.Views[1]);
	View.buffer = Set.Visible.Buffer;
	Api.CreateBufferView(View, Set.Views[2]);
	View.type = nri::BufferView::BUFFER;
	Api.CreateBufferView(View, Set.Views[3]);

	Api.AllocateDescriptorSets(*GrassPool, *GrassLayout, 0, Set.Sets, FrameCount, 0);

	// scene_depth: the instances where the moving objects' vertices are, the grass list for the visible one;
	// the heap's index pages are not read
	nri::Descriptor* Tables[5 + PageSlots];
	nri::Descriptor* Lists[2 + PageSlots];
	std::copy(SceneTableViews, SceneTableViews + 4, Tables);
	std::copy(VertexPageViews, VertexPageViews + PageSlots, Tables + 4);
	Tables[4 + PageSlots] = Set.Views[0];
	std::fill(Lists, Lists + 2 + PageSlots, ClusterIndexView);
	Lists[1] = Set.Views[3];

	Api.AllocateDescriptorSets(*GrassDrawPool, *DrawLayout, 0, &Set.DrawSet, 1, 0);
	const nri::UpdateDescriptorRangeDesc Updates[4] = { { Set.DrawSet, 0, 0, Tables, 5 + PageSlots }, { Set.DrawSet, 1, 0, Lists, 2 + PageSlots }, { Set.DrawSet, 2, 0, &AlphaView, 1 }, { Set.DrawSet, 3, 0, &ModelView, 1 } };
	Api.UpdateDescriptorRanges(Updates, 4);

	for (nri::DescriptorSet* Cull : Set.Sets)
	{
		const nri::UpdateDescriptorRangeDesc CullUpdates[3] = { { Cull, 0, 0, &Set.Views[0], 1 }, { Cull, 1, 0, &Set.Views[1], 2 }, { Cull, 3, 0, &ClusterIndexView, 1 } };
		Api.UpdateDescriptorRanges(CullUpdates, 3);
	}
}
nri::Pipeline* FindGrassPipeline(nri::Format Format, bool IsSpot)
{
	for (const GrassPipeline& Item : GrassPipelines)
	{
		if (Item.Format == Format && Item.IsSpot == IsSpot)
		{
			return Item.Pipeline;
		}
	}

	ShaderExternalMap Defines;
	Defines["USE_AREF"] = "1";
	Defines["USE_GRASS"] = "1";

	GrassVertex = DEV->_CreateVS("common\\scene_depth", Defines);
	GrassPixels = DEV->_CreatePS("common\\scene_depth", Defines);
	const nri::ShaderDesc Shaders[2] = { ShaderCode(GrassVertex->vs, nri::StageBits::VERTEX_SHADER), ShaderCode(GrassPixels->ps, nri::StageBits::FRAGMENT_SHADER) };
	const RHIRasterizerDesc& Raster = DepthState->state->Raster;

	nri::GraphicsPipelineDesc Desc = {};
	Desc.pipelineLayout = DrawLayout;
	Desc.inputAssembly.topology = nri::Topology::TRIANGLE_LIST;
	// Grass is drawn from both sides
	Desc.rasterization.cullMode = nri::CullMode::NONE;
	Desc.rasterization.depthBias = { float(Raster.DepthBias), Raster.DepthBiasClamp, Raster.SlopeScaledDepthBias };
	Desc.rasterization.depthClamp = !Raster.DepthClipEnable;
	Desc.outputMerger.depthStencilFormat = Format;
	Desc.outputMerger.depth.compareOp = IsSpot ? nri::CompareOp::LESS_EQUAL : Compare[u32(DepthState->state->Depth.DepthFunc)];
	Desc.outputMerger.depth.write = true;
	Desc.shaders = Shaders;
	Desc.shaderNum = 2;

	nri::Pipeline* Pipeline = nullptr;
	PROF_EVENT("ScenePipelineCreate");
	Api.CreateGraphicsPipeline(*GRHI->Nri->Device, Desc, Pipeline);
	GrassPipelines.push_back({ Format, IsSpot, Pipeline });
	return Pipeline;
}
} // namespace

// A caster outside a side of the main view shadows nothing the eye sees when the light carries its shadow
// further out: the sides the light leaves by, and the plane behind the eye, cull the casters as well.
// returns the count of the planes with these
u32 AddCasterPlanes(Hlsl::float4* Planes, u32 Count, const Fvector& Look)
{
	if (Look.square_magnitude() <= 0.0f)
	{
		return Count;
	}

	const CFrustum& Main = RImplementation.ViewBase;
	Fplane Behind;
	Behind.n.invert(RFrame.vCameraDirection);
	Behind.d = -Behind.n.dotproduct(RFrame.vCameraPosition);

	for (int Side = 0; Side <= Main.p_count && Count < SCENE_VIEW_PLANES; ++Side)
	{
		Fplane Source = Behind;

		if (Side < Main.p_count)
		{
			Source = Main.planes[Side];
		}

		if (Source.n.dotproduct(Look) >= 0.0f)
		{
			Planes[Count++].set(Source.n.x, Source.n.y, Source.n.z, Source.d);
		}
	}

	return Count;
}
} // namespace GpuSceneInternal

bool GpuScene::DrawGrass(GfxCmdList& Cmd, u32 Frame, const GrassBin* Bins, u32 BinCount, const CFrustum& Frustum, const DepthDraw& Draw, const GrassRuns* Runs)
{
	IRHISurface* Surface = Draw.IsForward ? nullptr : Draw.Target->GetSurface();
	const u32 Format = Surface ? NriDepthFormat(Surface->GetFormat()) : 0;
	if (!ClusterCount || !AlphaView || !DepthState || !BinCount || BinCount > SCENE_GRASS_BINS || (!Draw.IsForward && !Format))
	{
		return false;
	}

	Hlsl::SceneGrassView Block = {};
	u32 Total = 0;
	u32 Most = 0;

	for (u32 Bin = 0; Bin < BinCount; ++Bin)
	{
		const GrassBin& Item = Bins[Bin];
		if (Item.Cluster == u32(-1))
		{
			return false;
		}

		// An entry is as many corners as the largest mesh a view draws: every view draws the whole one of a near blade
		Most = std::max(Most, std::min(std::max(Item.Triangles, Item.FarTriangles), SCENE_CLUSTER_TRIANGLES));

		Block.bins[Bin] = { Total, Item.Count, Item.Radius, Item.Cluster, (Item.FarTriangles + SCENE_CLUSTER_TRIANGLES - 1) / SCENE_CLUSTER_TRIANGLES, (Item.Triangles + SCENE_CLUSTER_TRIANGLES - 1) / SCENE_CLUSTER_TRIANGLES, Item.Variant, Item.FarTriangles | (Item.Triangles << 16) };
		Block.bins[Bin].own_radius = Item.OwnRadius;

		if (const auto Found = GrassDraws.find(Item.Cluster); Found != GrassDraws.end())
		{
			Block.bins[Bin].index_start = Found->second.IndexStart;
			Block.bins[Bin].vertices = Found->second.Vertices;
			Block.bins[Bin].material = Found->second.Material;
		}

		if (Item.Variant)
		{
			std::copy(Item.Wave, Item.Wave + 4, Block.waves + (Item.Variant - 1) * 4);
		}

		Total += Item.Count;
	}

	if (Total > SCENE_GRASS_ITEMS)
	{
		return false;
	}

	// A blade's model is a few triangles: an entry is not drawn with the corners no cluster of the frame has
	// The forward frame culls its grass after the depth pyramid of its first part is built: a blade is tested on it
	const bool IsHidden = Draw.IsForward && HizRead && IsForwardBinned();
	// Outside the editor the lists come thinned for the main view, for every view that draws them
	Block.limits = { Most * 3, Runs ? 2u : Draw.IsForward || !DevicePtr->IsEditorMode() ? 1u : 0u, Draw.IsForward ? 1u : 0u, IsHidden ? 1u : 0u };
	Block.budget.set(0.0f, 0.0f, 0.0f, 0.0f);

	if (IsHidden)
	{
		// r_culling_quality: the layers of grass a tile of the screen keeps
		static constexpr float Layers[] = { 4.0f, 8.0f, 0.0f };
		u32 Size = GrassTileSize;

		while (((HizWidth * 4 + Size - 1) / Size) * ((HizHeight * 4 + Size - 1) / Size) > SCENE_GRASS_TILES)
		{
			Size *= 2;
		}

		Block.budget.set(Layers[std::min<u32>(ps_r_culling_quality, 2)], 0.0f, float((HizWidth * 4 + Size - 1) / Size), float((HizHeight * 4 + Size - 1) / Size));
	}


	GrassSet& Set = Grass;
	if (!Runs && Set.CellFrame != Frame)
	{
		Set.CellFrame = Frame;
		const auto Position = [&](u32 Bin, u32 Index)
		{
			Fvector Value;
			std::memcpy(&Value, static_cast<const u8*>(Bins[Bin].Items) + size_t(Index) * GrassItemSize + GrassItemPosition, sizeof(Value));
			return Value;
		};

		Fbox Area;
		Area.invalidate();
		float Reserve = 0.0f;

		for (u32 Bin = 0; Bin < BinCount; ++Bin)
		{
			Reserve = std::max(Reserve, Bins[Bin].Radius * 2.0f);

			for (u32 Index = 0; Index < Bins[Bin].Count; ++Index)
			{
				Area.modify(Position(Bin, Index));
			}
		}

		for (u32 Cell = 0; Cell < GrassCells * GrassCells; ++Cell)
		{
			Set.Cells[Cell].invalidate();
			Set.CellBins[Cell].reset();
		}

		const float ScaleX = float(GrassCells) / std::max(Area.max.x - Area.min.x, 1.0f);
		const float ScaleZ = float(GrassCells) / std::max(Area.max.z - Area.min.z, 1.0f);

		for (u32 Bin = 0; Bin < BinCount; ++Bin)
		{
			for (u32 Index = 0; Index < Bins[Bin].Count; ++Index)
			{
				const Fvector Value = Position(Bin, Index);
				const u32 X = std::min(u32((Value.x - Area.min.x) * ScaleX), GrassCells - 1);
				const u32 Z = std::min(u32((Value.z - Area.min.z) * ScaleZ), GrassCells - 1);
				Set.Cells[Z * GrassCells + X].modify(Value);
				Set.CellBins[Z * GrassCells + X].set(Bin);
			}
		}

		for (u32 Cell = 0; Cell < GrassCells * GrassCells; ++Cell)
		{
			Set.Cells[Cell].grow(Reserve);
		}
	}

	std::bitset<SCENE_GRASS_BINS> Reached;

	for (u32 Cell = 0; Cell < GrassCells * GrassCells && !Runs; ++Cell)
	{
		u32 Planes = Frustum.getMask();

		if ((Set.CellBins[Cell] & ~Reached).any() && Frustum.testAABB(Set.Cells[Cell].data(), Planes) != fcvNone)
		{
			Reached |= Set.CellBins[Cell];
		}
	}

	if (Runs ? !Runs->Count : Reached.none())
	{
		return true;
	}

	GPU_EVENT(SceneGrass);

	if (!GrassLayout)
	{
		const nri::StageBits Compute = nri::StageBits::COMPUTE_SHADER;
		const nri::DescriptorRangeDesc Ranges[4] = { { 0, 1, nri::DescriptorType::STRUCTURED_BUFFER, Compute }, { 0, 2, nri::DescriptorType::STORAGE_BUFFER, Compute }, { 1, 1, nri::DescriptorType::TEXTURE, Compute, nri::DescriptorRangeBits::PARTIALLY_BOUND }, { 2, 1, nri::DescriptorType::BUFFER, Compute, nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET } };

		GrassLayout = CreateLayout(Compute, Ranges, 4);
		GrassCullShader = DEV->_CreateCS("common\\scene_grass_cull");
		nri::ComputePipelineDesc Desc = {};
		Desc.pipelineLayout = GrassLayout;
		Desc.shader = ShaderCode(GrassCullShader->sh, Compute);
		Api.CreateComputePipeline(*GRHI->Nri->Device, Desc, GrassCullPipeline);

		nri::BufferViewDesc View = {};
		View.buffer = StreamApi.GetStreamerConstantBuffer(*Streamer);
		View.type = nri::BufferView::CONSTANT_BUFFER;
		View.size = (sizeof(Hlsl::SceneGrassView) + 255) & ~255u;
		Api.CreateBufferView(View, GrassBlock);

		nri::DescriptorPoolDesc PoolDesc = {};
		PoolDesc.descriptorSetMaxNum = 32;
		PoolDesc.structuredBufferMaxNum = 32;
		PoolDesc.storageBufferMaxNum = 64;
		PoolDesc.textureMaxNum = 32;
		PoolDesc.bufferMaxNum = 32;
		Api.CreateDescriptorPool(*GRHI->Nri->Device, PoolDesc, GrassPool);

		// The sets of the draw: the scene's own tables with the frame's instances and their visible list
		PoolDesc = {};
		PoolDesc.descriptorSetMaxNum = 32;
		PoolDesc.structuredBufferMaxNum = 32 * (6 + PageSlots);
		PoolDesc.bufferMaxNum = 32 * (2 + PageSlots);
		PoolDesc.textureMaxNum = 32;
		Api.CreateDescriptorPool(*GRHI->Nri->Device, PoolDesc, GrassDrawPool);
	}

	if (std::max(Total, Runs ? GrassSlotStore::Capacity : 0u) > Set.Capacity)
	{
		// The old backend owns the queue: its flush covers the passes that read the old buffers
		PROF_EVENT("GrassReserve");
		GRHI->DevicePtr->Flush();
		ReserveGrass(Set, std::max(Total, Runs ? GrassSlotStore::Capacity : 0u));
	}

	if (Runs)
	{
		// The slots' blades that came or changed since the last frame; the buffer keeps the rest
		{
			PROF_EVENT("GrassBladesUpload");
			Runs->Store->Drain([&](u32 First, const CDetail::SlotItem* Blades, u32 Count)
			{
				Set.Source->UpdateSubresource(Blades, Count * GrassItemSize, First * GrassItemSize);
			});
		}

		if (Runs->Count > GrassRunCapacity)
		{
			GRHI->DevicePtr->Flush();
			GrassRunCapacity = std::max(Runs->Count, std::max(GrassRunCapacity * 2, 65536u));
			GrassRunFrame = u32(-1);

			for (u32 Slot = 0; Slot < FrameCount; ++Slot)
			{
				if (GrassRunBuffers[Slot])
				{
					Api.DestroyDescriptor(GrassRunViews[Slot]);
					Api.DestroyBuffer(GrassRunBuffers[Slot]);
				}

				nri::BufferDesc Desc = {};
				Desc.size = u64(GrassRunCapacity) * sizeof(u32);
				Desc.usage = nri::BufferUsageBits::SHADER_RESOURCE;
				Api.CreateCommittedBuffer(*GRHI->Nri->Device, nri::MemoryLocation::HOST_UPLOAD, 0.0f, Desc, GrassRunBuffers[Slot]);
				nri::BufferViewDesc View = {};
				View.buffer = GrassRunBuffers[Slot];
				View.type = nri::BufferView::BUFFER;
				View.format = nri::Format::R32_UINT;
				Api.CreateBufferView(View, GrassRunViews[Slot]);
			}
		}

		if (GrassRunFrame != Frame || GrassRunWords != Runs->Words)
		{
			GrassRunFrame = Frame;
			GrassRunWords = Runs->Words;
			const u64 Bytes = u64(Runs->Count) * sizeof(u32);
			std::memcpy(Api.MapBuffer(*GrassRunBuffers[Frame % FrameCount], 0, Bytes), Runs->Words, Bytes);
			Api.UnmapBuffer(*GrassRunBuffers[Frame % FrameCount]);
		}

		const nri::UpdateDescriptorRangeDesc Update = { Set.Sets[Frame % FrameCount], 3, 0, &GrassRunViews[Frame % FrameCount], 1 };
		Api.UpdateDescriptorRanges(&Update, 1);
	}

	if (!Runs && (Set.Frame != Frame || Total > Set.Uploaded))
	{
		Set.Frame = Frame;
		Set.Uploaded = Total;

		for (u32 Bin = 0; Bin < BinCount; ++Bin)
		{
			Set.Source->UpdateSubresource(Bins[Bin].Items, Bins[Bin].Count * GrassItemSize, Block.bins[Bin].first * GrassItemSize);
		}
	}

	u32 Planes = std::min(u32(Frustum.p_count), SCENE_VIEW_PLANES);
	for (u32 Plane = 0; Plane < Planes; ++Plane)
	{
		const Fplane& Source = Frustum.planes[Plane];
		Block.planes[Plane].set(Source.n.x, Source.n.y, Source.n.z, Source.d);
	}

	Planes = AddCasterPlanes(Block.planes, Planes, Draw.Look);
	Block.view_projection = Cmd.Constants.Read<CBView>().m_VP;
	Block.counts = { Total, BinCount, Planes, Set.Capacity * GrassEntries };
	// Every view thins by the main view's eye, so a shadow belongs to a blade that is drawn
	Block.thin.set(RFrame.vCameraPosition.x, RFrame.vCameraPosition.y, RFrame.vCameraPosition.z, GrassThinStart());
	Block.lod.set(GrassFarStart(), ps_r2_def_aref_quality / 255.0f, 0.0f, 0.0f);
	std::memcpy(&Block.wind, &Cmd.Constants.Read<CBDetail>(), sizeof(Block.wind));

	const nri::Format Typed = nri::nriConvertDXGIFormatToNRI(Format);
	nri::Texture* Texture = Surface ? WrapNriSurface(Surface, Format) : nullptr;
	nri::Descriptor* SliceView = Surface ? MakeSliceView(Texture, Draw.Slice) : nullptr;

	for (u32 Page = 0; Page < RImplementation.Geometry.PageCount(); ++Page)
	{
		if (IRHIBuffer* Source = RImplementation.Geometry.VertexBuffer(Page))
		{
			GRHI->DevicePtr->ReadExternalBuffer(Source);
			GRHI->DevicePtr->ReadExternalBuffer(RImplementation.Geometry.IndexBuffer(Page));
		}
	}

	GRHI->DevicePtr->ReadExternalBuffer(Set.Source);

	// One pass: the instances are culled into entries of a visible list, one per cluster, and one draw takes them
	nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, Draw.IsForward ? nullptr : Draw.Target);
	Move(Commands, Set.Args, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
	Api.CmdZeroBuffer(Commands, *Set.Args.Buffer, 0, GrassDrawBytes + GrassTileBytes);
	Move(Commands, Set.Args, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
	Move(Commands, Set.Visible, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);

	Api.CmdSetDescriptorPool(Commands, *GrassPool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *GrassLayout);
	Api.CmdSetPipeline(Commands, *GrassCullPipeline);

	nri::SetRootDescriptorDesc Root = {};
	Root.descriptor = GrassBlock;
	Root.offset = StreamApi.StreamConstantData(*Streamer, &Block, sizeof(Block));
	Api.CmdSetRootDescriptor(Commands, Root);

	if (IsHidden)
	{
		const nri::UpdateDescriptorRangeDesc Pyramid = { Set.Sets[Frame % FrameCount], 2, 0, &HizRead, 1 };
		Api.UpdateDescriptorRanges(&Pyramid, 1);
		nri::SetRootDescriptorDesc Main = {};
		Main.rootDescriptorIndex = 1;
		Main.descriptor = ViewBlock;
		Main.offset = ForwardViewOffset;
		Api.CmdSetRootDescriptor(Commands, Main);
	}

	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = Set.Sets[Frame % FrameCount];
	Api.CmdSetDescriptorSet(Commands, SetDesc);
	const nri::DispatchDesc Groups = { ((Runs ? Runs->Count : Total) + 63) / 64, 1, 1 };

	if (Block.budget.x != 0.0f)
	{
		// The blades of every tile of the screen are counted first: the pass after keeps a tile's share of them
		Hlsl::SceneGrassView Counting = Block;
		Counting.budget.y = 1.0f;
		nri::SetRootDescriptorDesc Count = Root;
		Count.offset = StreamApi.StreamConstantData(*Streamer, &Counting, sizeof(Counting));
		Api.CmdSetRootDescriptor(Commands, Count);
		Api.CmdDispatch(Commands, Groups);
		Move(Commands, Set.Args, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
		Api.CmdSetRootDescriptor(Commands, Root);
	}

	Api.CmdDispatch(Commands, Groups);

	Move(Commands, Set.Args, nri::AccessBits::ARGUMENT_BUFFER, nri::StageBits::INDIRECT);
	Move(Commands, Set.Visible, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::VERTEX_SHADER);

	// The forward frame draws the entries itself, with its own shaders
	if (Draw.IsForward)
	{
		GrassForwardBlock = Block;
		GrassForwardFrame = Frame;
		EndNriCommands(&Commands);
		Cmd.RestoreInput();
		return true;
	}

	Api.CmdSetDescriptorPool(Commands, *GrassDrawPool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::GRAPHICS, *DrawLayout);
	Api.CmdSetPipeline(Commands, *FindGrassPipeline(Typed, Draw.IsSpot));
	// The grass block is all the draw reads: it stands in both places
	Api.CmdSetRootDescriptor(Commands, Root);
	Root.rootDescriptorIndex = 1;
	Api.CmdSetRootDescriptor(Commands, Root);

	u32 WholeWidth = Surface->GetWidth();
	u32 WholeHeight = Surface->GetHeight();
	ViewSize(WholeWidth, WholeHeight);
	const RHIViewport Whole = { 0.0f, 0.0f, float(WholeWidth), float(WholeHeight), 0.0f, 1.0f };
	const RHIViewport& Drawn = Draw.Area ? *Draw.Area : Whole;
	const nri::Viewport Viewport = { Drawn.TopLeftX, Drawn.TopLeftY, Drawn.Width, Drawn.Height, Drawn.MinDepth, Drawn.MaxDepth };
	const nri::Rect Scissor = { s16(Drawn.TopLeftX), s16(Drawn.TopLeftY), nri::Dim_t(Drawn.Width), nri::Dim_t(Drawn.Height) };
	Api.CmdSetViewports(Commands, &Viewport, 1);
	Api.CmdSetScissors(Commands, &Scissor, 1);

	nri::RenderingDesc Rendering = {};
	Rendering.depth.descriptor = SliceView;
	Rendering.depth.loadOp = nri::LoadOp::LOAD;
	Rendering.depth.storeOp = nri::StoreOp::STORE;
	Api.CmdBeginRendering(Commands, Rendering);

	SetDesc.descriptorSet = Set.DrawSet;
	Api.CmdSetDescriptorSet(Commands, SetDesc);
	Api.CmdDrawIndirect(Commands, *Set.Args.Buffer, 0, 1, sizeof(nri::DrawDesc), nullptr, 0);
	Api.CmdEndRendering(Commands);
	EndNriCommands(&Commands);

	if (Texture)
	{
		Api.DestroyDescriptor(SliceView);
		Api.DestroyTexture(Texture);
	}

	Cmd.RestoreInput();
	return true;
}

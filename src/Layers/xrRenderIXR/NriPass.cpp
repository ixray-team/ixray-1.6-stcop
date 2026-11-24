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
#include "NriPass.h"
#include "GpuTextures.h"
#include "dxRenderDeviceRender.h"

#include "StateManager/dx10SamplerStateCache.h"

// The engine defines Device as a macro; NRI and NriDevice use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

#include <NRI.h>
#include <Extensions/NRIHelper.h>
#include <Extensions/NRIStreamer.h>
#include <Extensions/NRIWrapperD3D11.h>
#include <Extensions/NRIWrapperD3D12.h>

namespace
{
nri::Device* Gpu = nullptr;
nri::CoreInterface Api;
nri::HelperInterface Help;
nri::StreamerInterface StreamApi;
nri::WrapperD3D11Interface Wrapper11;
nri::WrapperD3D12Interface Wrapper12;
// Without NVAPI, NRI's D3D11 command buffer ignores the context it is given: it stores the commands and replays them
// on the immediate context when submitted, so one buffer serves every pass
nri::CommandBuffer* Replay = nullptr;
bool IsReplaying = false;
// A run of dynamic draws recorded into Replay, its rendering scope still open; RunEpoch tells a new run
bool IsRunOpen = false;
bool IsRunRendering = false;
u32 RunEpoch = 0;
// D3D12: the pass a row of dynamic batches with the same targets shares (SubmitDynamic of a range)
nri::CommandBuffer* Group = nullptr;

const char* const Declines[] = { "", "no vertex shader", "geometry or tessellation shaders", "a view bound by hand that NRI has no view of",
	"a depth target it cannot write", "a target it cannot write", "targets of mixed sample counts", "a texture it cannot read",
	"an empty read of a kind with no zero view", "a blend factor", "no view of a texture", "more reads than its set holds", "no pipeline",
	"no descriptor set", "a vertex shader of input-assembler vertices", "a triangle fan", "more vertices than a chunk holds" };
static_assert(std::size(Declines) == size_t(ENriDecline::Count));

// What a caller asked for that NRI cannot do, told once a reason: the clear, copy or pass is skipped
void TellOnce(const char* Why, const char* Name)
{
	static const char* Told[32] = {};

	for (const char*& Item : Told)
	{
		if (Item == Why)
		{
			return;
		}

		if (!Item)
		{
			Item = Why;
			Msg("! NRI pass: %s (%s), skipped", Why, Name);
			return;
		}
	}
}

nri::AttachmentDesc Loaded(nri::Descriptor* View)
{
	nri::AttachmentDesc Desc = {};
	Desc.descriptor = View;
	Desc.loadOp = nri::LoadOp::LOAD;
	Desc.storeOp = nri::StoreOp::STORE;
	return Desc;
}

// ERHI_COMPARISON and ERHI_STENCIL_OP keep the D3D numbering, which starts at 1
const nri::CompareOp Compare[] = { nri::CompareOp::NONE, nri::CompareOp::NEVER, nri::CompareOp::LESS, nri::CompareOp::EQUAL, nri::CompareOp::LESS_EQUAL, nri::CompareOp::GREATER, nri::CompareOp::NOT_EQUAL, nri::CompareOp::GREATER_EQUAL, nri::CompareOp::ALWAYS };
const nri::StencilOp StencilOps[] = { nri::StencilOp::KEEP, nri::StencilOp::KEEP, nri::StencilOp::ZERO, nri::StencilOp::REPLACE, nri::StencilOp::INCREMENT_AND_CLAMP, nri::StencilOp::DECREMENT_AND_CLAMP, nri::StencilOp::INVERT, nri::StencilOp::INCREMENT_AND_WRAP, nri::StencilOp::DECREMENT_AND_WRAP };

nri::SamplerDesc MakeSampler(const RHISampleDesc& Source)
{
	static const nri::AddressMode Address[] = { nri::AddressMode::REPEAT, nri::AddressMode::REPEAT, nri::AddressMode::MIRRORED_REPEAT, nri::AddressMode::CLAMP_TO_EDGE, nri::AddressMode::CLAMP_TO_BORDER, nri::AddressMode::MIRROR_CLAMP_TO_EDGE };

	// ERHI_FILTER keeps the D3D bit layout: mip 0x1, mag 0x4, min 0x10, anisotropic 0x40, comparison 0x80
	const u32 Filter = u32(Source.Filter);
	nri::SamplerDesc Desc = {};
	Desc.filters.min = Filter & 0x10 ? nri::Filter::LINEAR : nri::Filter::NEAREST;
	Desc.filters.mag = Filter & 0x4 ? nri::Filter::LINEAR : nri::Filter::NEAREST;
	Desc.filters.mip = Filter & 0x1 ? nri::Filter::LINEAR : nri::Filter::NEAREST;
	Desc.anisotropy = Filter & 0x40 ? u8(Source.MaxAnisotropy) : u8(1);
	Desc.mipBias = Source.MipLODBias;
	Desc.mipMax = 16.0f;
	Desc.addressModes = { Address[u32(Source.AddressU)], Address[u32(Source.AddressV)], Address[u32(Source.AddressW)] };
	Desc.compareOp = Filter & 0x80 ? Compare[u32(Source.ComparisonFunc)] : nri::CompareOp::NONE;
	return Desc;
}

void LoadApi()
{
	if (Gpu)
	{
		return;
	}

	Gpu = GRHI->Nri->Device;
	nri::nriGetInterface(*Gpu, NRI_INTERFACE(nri::CoreInterface), &Api);
	nri::nriGetInterface(*Gpu, NRI_INTERFACE(nri::HelperInterface), &Help);
	nri::nriGetInterface(*Gpu, NRI_INTERFACE(nri::StreamerInterface), &StreamApi);

	if (GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		nri::nriGetInterface(*Gpu, NRI_INTERFACE(nri::WrapperD3D12Interface), &Wrapper12);
	}
	else
	{
		nri::nriGetInterface(*Gpu, NRI_INTERFACE(nri::WrapperD3D11Interface), &Wrapper11);
	}
}
} // namespace

// On D3D12 every pass records into the NRI command buffer of the old backend's open list, between its draws. A D3D11
// pass runs when it ends: the old backend draws on the same context at once, so its commands cannot wait for more
nri::CommandBuffer* BeginNriCommands(IRHISurface* const* Reads, u32 ReadCount, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth)
{
	PROF_EVENT("NriBegin");
	FlushNriRun();
	LoadApi();

	if (nri::CommandBuffer* Commands = GRHI->BeginExternalPass(Reads, ReadCount, Targets, TargetCount, Depth))
	{
		return Commands;
	}

	VERIFY(!IsReplaying);
	IsReplaying = true;

	if (!Replay)
	{
		nri::CommandBufferD3D11Desc Desc = {};
		Desc.d3d11DeviceContext = static_cast<ID3D11DeviceContext*>(GRHI->GetContext());
		Wrapper11.CreateCommandBufferD3D11(*Gpu, Desc, Replay);
	}

	Api.BeginCommandBuffer(*Replay, nullptr);
	return Replay;
}

namespace
{
// A run keeps the command list's record of its shaders: the draw that replays it binds them again
void SubmitReplay(nri::CommandBuffer* Commands, bool IsRun)
{
	Api.EndCommandBuffer(*Commands);

	// The commands run now, on the context the old backend draws with. Its input layout is still bound there and
	// NRI's vertex shaders take other inputs: the layer counts every such draw as an error. The old backend
	// sets its own again at its next draw
	GRHI->SetInputLayout(nullptr);
	RImplementation.Backend.RestoreInput();

	nri::QueueSubmitDesc Submit = {};
	Submit.commandBuffers = &Commands;
	Submit.commandBufferNum = 1;
	Api.QueueSubmit(*GRHI->Nri->Queue, Submit);
	IsReplaying = false;
	++RImplementation.NriPasses.Counting.Submits;
	GRHI->EndExternalPass();

	// NRI set its own shaders on the context the old backend caches by pointer: taken as still bound, a compute
	// shader of NRI ran the old backend's next dispatch
	RImplementation.Imm().UnbindShaders(!IsRun);
}
} // namespace

void EndNriCommands(nri::CommandBuffer* Commands)
{
	PROF_EVENT("NriEnd");
	if (GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		GRHI->EndExternalPass();
		return;
	}

	SubmitReplay(Commands, false);
}

bool FlushNriRun()
{
	if (!IsRunOpen)
	{
		return false;
	}

	PROF_EVENT("NriFlushRun");
	IsRunOpen = false;

	if (IsRunRendering)
	{
		Api.CmdEndRendering(*Replay);
		IsRunRendering = false;
	}

	RImplementation.Dynamic.Upload();
	SubmitReplay(Replay, true);
	return true;
}

void ResolveNri(IRHIRenderTargetView* const* Targets, IRHISurface* const* Sources, u32 Count)
{
	constexpr u32 Most = 4;
	R_ASSERT(Count <= Most);
	// The old backend hands its sources over as read by shaders and its targets as drawn into: they go back so
	nri::CommandBuffer& Commands = *BeginNriCommands(Sources, Count, Targets, Count, nullptr);
	nri::Texture* Wrapped[Most * 2];
	nri::TextureBarrierDesc Barriers[Most * 2] = {};

	for (u32 Index = 0; Index < Count; ++Index)
	{
		Wrapped[Index * 2] = WrapNriSurface(Targets[Index]->GetSurface());
		Wrapped[Index * 2 + 1] = WrapNriSurface(Sources[Index]);

		nri::TextureBarrierDesc& To = Barriers[Index * 2];
		To.texture = Wrapped[Index * 2];
		To.before = { nri::AccessBits::COLOR_ATTACHMENT, nri::Layout::COLOR_ATTACHMENT, nri::StageBits::COLOR_ATTACHMENT };
		To.after = { nri::AccessBits::RESOLVE_DESTINATION, nri::Layout::RESOLVE_DESTINATION, nri::StageBits::RESOLVE };
		To.mipNum = nri::REMAINING;
		To.layerNum = nri::REMAINING;

		nri::TextureBarrierDesc& From = Barriers[Index * 2 + 1];
		From.texture = Wrapped[Index * 2 + 1];
		From.before = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::FRAGMENT_SHADER };
		From.after = { nri::AccessBits::RESOLVE_SOURCE, nri::Layout::RESOLVE_SOURCE, nri::StageBits::RESOLVE };
		From.mipNum = nri::REMAINING;
		From.layerNum = nri::REMAINING;
	}

	nri::BarrierDesc Desc = {};
	Desc.textures = Barriers;
	Desc.textureNum = Count * 2;
	Api.CmdBarrier(Commands, Desc);

	for (u32 Index = 0; Index < Count; ++Index)
	{
		Api.CmdResolveTexture(Commands, *Wrapped[Index * 2], nullptr, *Wrapped[Index * 2 + 1], nullptr, nri::ResolveOp::AVERAGE);
	}

	for (u32 Index = 0; Index < Count * 2; ++Index)
	{
		std::swap(Barriers[Index].before, Barriers[Index].after);
	}

	Api.CmdBarrier(Commands, Desc);
	EndNriCommands(&Commands);

	for (u32 Index = 0; Index < Count * 2; ++Index)
	{
		Api.DestroyTexture(Wrapped[Index]);
	}
}

void NriPass::Create(const CShaderConstants& Constants)
{
	LoadApi();
	IsCreated = true;

	nri::StreamerDesc StreamerDesc = {};
	StreamerDesc.constantBufferMemoryLocation = nri::MemoryLocation::HOST_UPLOAD;
	// A ring nothing fences: it holds every queued frame's blocks, the dynamic draws' included
	StreamerDesc.constantBufferSize = 16 << 20;
	StreamerDesc.dynamicBufferMemoryLocation = nri::MemoryLocation::HOST_UPLOAD;
	StreamerDesc.dynamicBufferDesc.usage = nri::BufferUsageBits::VERTEX;
	StreamerDesc.queuedFrameNum = FrameCount - 1;
	StreamApi.CreateStreamer(*Gpu, StreamerDesc, Streamer);

	nri::Buffer* ConstantBuffer = StreamApi.GetStreamerConstantBuffer(*Streamer);
	Constants.ForEachBlock([&](u32 Slot, const void*, u32 Size)
	{
		R_ASSERT(BlockCount < BlockSlots);
		BlockRegisters[BlockCount] = Slot;
		LastBlockOffsets[BlockCount] = u32(LastBlocks.size());
		LastBlocks.resize(LastBlocks.size() + Size);

		nri::BufferViewDesc View = {};
		View.buffer = ConstantBuffer;
		View.type = nri::BufferView::CONSTANT_BUFFER;
		View.size = (Size + 255) & ~255u;
		Api.CreateBufferView(View, BlockViews[BlockCount++]);
	});

	nri::DescriptorRangeDesc Textures = {};
	Textures.descriptorNum = TextureSlots;
	Textures.descriptorType = nri::DescriptorType::TEXTURE;
	Textures.shaderStages = nri::StageBits::FRAGMENT_SHADER;
	// A pass draws into what an earlier one on the same list read while that one's set is still bound
	Textures.flags = nri::DescriptorRangeBits::PARTIALLY_BOUND | nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET;
	Layout = CreateLayout(&Textures, 1, u32(nri::StageBits::VERTEX_SHADER | nri::StageBits::FRAGMENT_SHADER));
	CreateZeros();
}

void NriPass::CreateZeros()
{
	constexpr u32 Layers = 6;
	const u32 Texels[Layers] = {};
	const nri::TextureSubresourceUploadDesc Content = { Texels, 1, sizeof(u32), sizeof(u32) };
	const nri::TextureSubresourceUploadDesc Contents[Layers] = { Content, Content, Content, Content, Content, Content };
	const nri::AccessLayoutStage Read = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::ALL };
	const nri::TextureType Types[3] = { nri::TextureType::TEXTURE_2D, nri::TextureType::TEXTURE_2D, nri::TextureType::TEXTURE_3D };
	nri::TextureUploadDesc Uploads[4] = {};

	for (u32 Index = 0; Index < 4; ++Index)
	{
		nri::TextureDesc Desc = {};
		Desc.type = Index < 3 ? Types[Index] : nri::TextureType::TEXTURE_2D;
		Desc.usage = Index < 3 ? nri::TextureUsageBits::SHADER_RESOURCE : nri::TextureUsageBits::SHADER_RESOURCE_STORAGE;
		Desc.format = Index < 3 ? nri::Format::RGBA8_UNORM : nri::Format::R32_SFLOAT;
		Desc.width = 1;
		Desc.height = 1;
		Desc.depth = 1;
		Desc.mipNum = 1;
		Desc.layerNum = Index == 1 ? nri::Dim_t(Layers) : nri::Dim_t(1);
		Desc.sampleNum = 1;
		nri::Texture*& Made = Index < 3 ? Zeros[Index] : Dropped;
		Api.CreateCommittedTexture(*Gpu, nri::MemoryLocation::DEVICE, 0.0f, Desc, Made);
		Api.SetDebugName(Made, Index < 3 ? "NRI pass zero texture" : "NRI pass dropped writes");

		Uploads[Index].subresources = Contents;
		Uploads[Index].texture = Made;
		Uploads[Index].after = Index < 3 ? Read : nri::AccessLayoutStage{ nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::Layout::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER };
		Uploads[Index].planes = nri::PlaneBits::ALL;
	}

	const xr_vector<u8> Bytes(4096);
	nri::BufferDesc BufferDesc = {};
	BufferDesc.size = Bytes.size();
	BufferDesc.structureStride = sizeof(u32);
	BufferDesc.usage = nri::BufferUsageBits::SHADER_RESOURCE;
	Api.CreateCommittedBuffer(*Gpu, nri::MemoryLocation::DEVICE, 0.0f, BufferDesc, ZeroBuffer);
	Api.SetDebugName(ZeroBuffer, "NRI pass zero buffer");
	const nri::BufferUploadDesc Buffer = { Bytes.data(), ZeroBuffer, { nri::AccessBits::SHADER_RESOURCE, nri::StageBits::ALL } };
	Help.UploadData(*GRHI->Nri->Queue, Uploads, 4, &Buffer, 1);

	// By ESlotKind
	static const nri::TextureView Views[] = { nri::TextureView::TEXTURE, nri::TextureView::TEXTURE_ARRAY, nri::TextureView::TEXTURE_CUBE, nri::TextureView::TEXTURE_CUBE_ARRAY, nri::TextureView::TEXTURE };
	const u32 Sources[] = { 0, 1, 1, 1, 2 };
	static_assert(std::size(Views) == size_t(ESlotKind::Structured));

	for (u32 Kind = 0; Kind < std::size(Views); ++Kind)
	{
		nri::TextureViewDesc View = {};
		View.texture = Zeros[Sources[Kind]];
		View.type = Views[Kind];
		View.mipNum = 1;
		View.layerNum = nri::REMAINING;
		Api.CreateTextureView(View, ZeroViews[Kind]);
	}

	nri::BufferViewDesc Structured = {};
	Structured.buffer = ZeroBuffer;
	Structured.type = nri::BufferView::STRUCTURED_BUFFER;
	Structured.size = Bytes.size();
	Structured.structureStride = sizeof(u32);
	Api.CreateBufferView(Structured, ZeroViews[u32(ESlotKind::Structured)]);

	nri::TextureViewDesc Storage = {};
	Storage.texture = Dropped;
	Storage.type = nri::TextureView::STORAGE_TEXTURE;
	Storage.mipNum = 1;
	Storage.layerNum = 1;
	Api.CreateTextureView(Storage, DroppedView);
}

nri::PipelineLayout* NriPass::CreateLayout(const nri::DescriptorRangeDesc* Ranges, u32 RangeCount, u32 Stages) const
{
	nri::RootDescriptorDesc Roots[BlockSlots] = {};

	for (u32 Block = 0; Block < BlockCount; ++Block)
	{
		Roots[Block].registerIndex = BlockRegisters[Block];
		Roots[Block].descriptorType = nri::DescriptorType::CONSTANT_BUFFER;
	}

	// The sampler table is fixed; anisotropy and mip bias are taken as they are now
	nri::RootSamplerDesc Samplers[ShaderBindings::SamplerCount] = {};
	u32 SamplerCount = 0;

	for (u32 Register = 0; Register < ShaderBindings::SamplerCount; ++Register)
	{
		if (RHIObject* State = SSManager.GetState(Register))
		{
			RHISampleDesc Source = {};
			State->GetDesc(&Source);
			Samplers[SamplerCount].registerIndex = Register;
			Samplers[SamplerCount++].desc = MakeSampler(Source);
		}
	}

	nri::DescriptorSetDesc Set = {};
	Set.ranges = Ranges;
	Set.rangeNum = RangeCount;

	nri::PipelineLayoutDesc LayoutDesc = {};
	LayoutDesc.rootDescriptors = Roots;
	LayoutDesc.rootDescriptorNum = BlockCount;
	LayoutDesc.rootSamplers = Samplers;
	LayoutDesc.rootSamplerNum = SamplerCount;
	LayoutDesc.descriptorSets = &Set;
	LayoutDesc.descriptorSetNum = 1;
	LayoutDesc.shaderStages = nri::StageBits(Stages);
	nri::PipelineLayout* Created = nullptr;
	Api.CreatePipelineLayout(*Gpu, LayoutDesc, Created);
	return Created;
}

void NriPass::ReleaseFrame(Frame& Item)
{
	for (const auto& [Key, Texture] : Item.Textures)
	{
		Api.DestroyDescriptor(Texture.View);
		Api.DestroyTexture(Texture.Texture);
	}

	Item.Textures.clear();

	for (const BufferWrap& Borrow : Item.Borrowed)
	{
		Api.DestroyDescriptor(Borrow.View);
		Api.DestroyBuffer(Borrow.Buffer);
	}

	Item.Borrowed.clear();
	Item.BufferCount = 0;

	for (Pools* Kind : { &Item.Passes, &Item.Dynamic })
	{
		for (u32 Pool = 0; Pool < Kind->Count; ++Pool)
		{
			Api.ResetDescriptorPool(*Kind->Items[Pool]);
		}

		Kind->Count = 0;
	}

	Item.Sets.clear();
}

// A set a pass: every restored spot shadow, every decal layer filled and every compute pass is one. A UI frame's
// dynamic draws read hundreds of textures
nri::DescriptorPool* NriPass::AllocateSets(Pools& From, bool IsDynamic, const nri::PipelineLayout& SetLayout, nri::DescriptorSet** Sets, u32 Count)
{
	const u32 Most = IsDynamic ? DynamicSets : SetSlots;
	nri::DescriptorPoolDesc Desc = {};
	Desc.flags = nri::DescriptorPoolBits::ALLOW_UPDATE_AFTER_SET;
	Desc.descriptorSetMaxNum = Most;
	Desc.textureMaxNum = Most * TextureSlots;
	Desc.storageTextureMaxNum = IsDynamic ? 0 : Most * StorageSlots;
	Desc.storageStructuredBufferMaxNum = IsDynamic ? 0 : Most;
	Desc.structuredBufferMaxNum = Most * (IsDynamic ? TextureSlots : 8);

	for (u32 Try = 0; Try < 2; ++Try)
	{
		if (From.Count && Api.AllocateDescriptorSets(*From.Items[From.Count - 1], SetLayout, 0, Sets, Count, 0) == nri::Result::SUCCESS)
		{
			return From.Items[From.Count - 1];
		}

		if (From.Count == From.Items.size())
		{
			nri::DescriptorPool* Pool = nullptr;

			if (Api.CreateDescriptorPool(*Gpu, Desc, Pool) != nri::Result::SUCCESS)
			{
				return nullptr;
			}

			From.Items.push_back(Pool);
		}

		++From.Count;
	}

	return nullptr;
}

void NriPass::Forget(IRHISurface* Surface)
{
	if (GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		return;
	}

	// D3D11 keeps what it was given until the work is done, once it is submitted
	FlushNriRun();
	void* Native = Surface->GetRawTexture();

	for (Frame& Item : Frames)
	{
		std::erase_if(Item.Textures, [&](const auto& Entry)
		{
			if (Entry.first.Native != Native)
			{
				return false;
			}

			Api.DestroyDescriptor(Entry.second.View);
			Api.DestroyTexture(Entry.second.Texture);
			return true;
		});
	}

	// A kept set or a recorded batch may hold one of them
	Frames[FrameIndex].Sets.clear();
	DropBatches();
}

void NriPass::DropBatches()
{
	for (DynamicBatch& Item : Batches)
	{
		Item.Pipeline = nullptr;
	}
}

void NriPass::EndFrame()
{
	FlushNriRun();
	Shown = std::exchange(Counting, NriStats{});
	HasDynamicBlocks = false;
	Batches.clear();
	BatchReads.clear();

	if (!IsCreated)
	{
		return;
	}

	StreamApi.EndStreamerFrame(*Streamer);
	FrameIndex = (FrameIndex + 1) % FrameCount;
	ReleaseFrame(Frames[FrameIndex]);
}

void NriPass::ReleaseTextures()
{
	if (!IsCreated)
	{
		return;
	}

	FlushNriRun();
	GRHI->DevicePtr->Flush();
	DropBatches();

	for (Frame& Item : Frames)
	{
		ReleaseFrame(Item);
	}
}

void NriPass::Destroy()
{
	FlushNriRun();

	if (Replay)
	{
		Api.DestroyCommandBuffer(Replay);
		Replay = nullptr;
	}

	if (!IsCreated)
	{
		return;
	}

	// The old backend owns the queue: its flush covers the passes recorded into its lists
	GRHI->DevicePtr->Flush();

	for (Frame& Item : Frames)
	{
		ReleaseFrame(Item);

		for (Pools* Kind : { &Item.Passes, &Item.Dynamic })
		{
			for (nri::DescriptorPool* Pool : Kind->Items)
			{
				Api.DestroyDescriptorPool(Pool);
			}

			Kind->Items.clear();
		}

		for (const Records& Buffer : Item.Buffers)
		{
			Api.DestroyDescriptor(Buffer.View);
			Api.DestroyBuffer(Buffer.Buffer);
		}

		Item.Buffers.clear();
	}

	for (const auto& [Key, Pipeline] : Pipelines)
	{
		Api.DestroyPipeline(Pipeline);
	}
	Pipelines.clear();
	LastPipeline = nullptr;

	for (const ComputePipeline& Item : ComputePipelines)
	{
		Api.DestroyPipeline(Item.Pipeline);
		Api.DestroyPipelineLayout(Item.Layout);
	}
	ComputePipelines.clear();

	for (const auto& [Format, Pipeline] : MipPipelines)
	{
		Api.DestroyPipeline(Pipeline);
	}
	MipPipelines.clear();

	for (u32 Block = 0; Block < BlockCount; ++Block)
	{
		Api.DestroyDescriptor(BlockViews[Block]);
	}

	BlockCount = 0;
	LastBlocks.clear();
	HasDynamicBlocks = false;
	Run = {};
	Batches.clear();
	BatchReads.clear();
	Api.DestroyPipelineLayout(DynamicLayout);
	DynamicLayout = nullptr;
	Api.DestroyBuffer(Iota);
	Iota = nullptr;
	for (nri::Descriptor*& View : ZeroViews)
	{
		Api.DestroyDescriptor(View);
		View = nullptr;
	}

	for (nri::Texture*& Texture : Zeros)
	{
		Api.DestroyTexture(Texture);
		Texture = nullptr;
	}

	Api.DestroyBuffer(ZeroBuffer);
	ZeroBuffer = nullptr;
	Api.DestroyDescriptor(DroppedView);
	Api.DestroyTexture(Dropped);
	DroppedView = nullptr;
	Dropped = nullptr;
	Api.DestroyPipelineLayout(MipLayout);
	MipLayout = nullptr;
	MipVertex = nullptr;
	MipPixel = nullptr;
	Api.DestroyPipelineLayout(Layout);
	StreamApi.DestroyStreamer(Streamer);
	Gpu = nullptr;
	IsCreated = false;
}

nri::Descriptor* NriPass::Wrap(IRHISurface* Surface, EView Kind, u32 TypedFormat, u32 Slice, u32 Mip)
{
	const s32 Format = s32(TypedFormat ? TypedFormat : u32(Surface->GetFormat()));
	const WrapKey Key = { Surface->GetRawTexture(), u32(Format), u32(Kind), Slice, Mip };
	const auto [Found, IsNew] = Frames[FrameIndex].Textures.try_emplace(Key, Wrapped{});
	Wrapped& Item = Found->second;

	if (!IsNew)
	{
		return Item.View;
	}

	if (GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		nri::TextureD3D12Desc Desc = {};
		Desc.d3d12Resource = static_cast<ID3D12Resource*>(Surface->GetRawTexture());
		Desc.format = Format;
		Wrapper12.CreateTextureD3D12(*Gpu, Desc, Item.Texture);
	}
	else
	{
		nri::TextureD3D11Desc Desc = {};
		Desc.d3d11Resource = static_cast<ID3D11Resource*>(Surface->GetRawTexture());
		Desc.format = Format;
		Wrapper11.CreateTextureD3D11(*Gpu, Desc, Item.Texture);
	}

	nri::TextureViewDesc View = {};
	View.texture = Item.Texture;
	static const nri::TextureView Types[] = { nri::TextureView::TEXTURE, nri::TextureView::COLOR_ATTACHMENT, nri::TextureView::DEPTH_STENCIL_ATTACHMENT, nri::TextureView::STORAGE_TEXTURE };
	View.type = Types[u32(Kind)];

	if (Kind == EView::Read)
	{
		// The same choice CTexture makes for the view the old backend binds
		const u32 Layers = Surface->GetArraySize();

		if ((ERHI_RESOURCE_MISC_FLAG(Surface->GetMiscFlags()) & ERHI_RESOURCE_MISC_FLAG::TEXTURECUBE) != ERHI_RESOURCE_MISC_FLAG(0))
		{
			View.type = Layers > 6 ? nri::TextureView::TEXTURE_CUBE_ARRAY : nri::TextureView::TEXTURE_CUBE;
		}
		else if (Layers > 1)
		{
			View.type = nri::TextureView::TEXTURE_ARRAY;
		}

		if (TypedFormat)
		{
			View.planes = nri::PlaneBits::DEPTH;
		}
	}

	View.mipOffset = nri::Dim_t(Mip);
	View.mipNum = Kind == EView::Read ? nri::REMAINING : nri::Dim_t(1);
	View.layerOffset = nri::Dim_t(Slice);
	View.layerNum = Kind == EView::Read ? nri::REMAINING : nri::Dim_t(1);
	Api.CreateTextureView(View, Item.View);
	return Item.View;
}

nri::Pipeline* NriPass::FindPipeline(const PipelineKey& Key)
{
	if (LastPipeline && !std::memcmp(&LastKey, &Key, sizeof(Key)))
	{
		return LastPipeline;
	}

	const auto [Found, IsNew] = Pipelines.try_emplace(Key, nullptr);

	if (IsNew)
	{
		Found->second = CreatePipeline(Key);
	}

	LastKey = Key;
	LastPipeline = Found->second;
	return LastPipeline;
}

nri::Pipeline* NriPass::CreatePipeline(const PipelineKey& Key)
{
	// ERHI_BLEND and ERHI_BLEND_OP keep the D3D numbering
	static const nri::BlendFactor Factors[] = {
		nri::BlendFactor::ZERO, nri::BlendFactor::ZERO, nri::BlendFactor::ONE, nri::BlendFactor::SRC_COLOR, nri::BlendFactor::ONE_MINUS_SRC_COLOR,
		nri::BlendFactor::SRC_ALPHA, nri::BlendFactor::ONE_MINUS_SRC_ALPHA, nri::BlendFactor::DST_ALPHA, nri::BlendFactor::ONE_MINUS_DST_ALPHA,
		nri::BlendFactor::DST_COLOR, nri::BlendFactor::ONE_MINUS_DST_COLOR, nri::BlendFactor::SRC_ALPHA_SATURATE, nri::BlendFactor::ZERO, nri::BlendFactor::ZERO,
		nri::BlendFactor::CONSTANT_COLOR, nri::BlendFactor::ONE_MINUS_CONSTANT_COLOR, nri::BlendFactor::SRC1_COLOR, nri::BlendFactor::ONE_MINUS_SRC1_COLOR,
		nri::BlendFactor::SRC1_ALPHA, nri::BlendFactor::ONE_MINUS_SRC1_ALPHA };
	static const nri::BlendOp Ops[] = { nri::BlendOp::ADD, nri::BlendOp::ADD, nri::BlendOp::SUBTRACT, nri::BlendOp::REVERSE_SUBTRACT, nri::BlendOp::MIN, nri::BlendOp::MAX };
	static const nri::CullMode Culls[] = { nri::CullMode::NONE, nri::CullMode::NONE, nri::CullMode::FRONT, nri::CullMode::BACK };

	const void* Code[2] = {};
	size_t Size[2] = {};

	// A pass with no pixel shader writes depth alone
	if (!GRHI->DevicePtr->GetShaderCode(Key.Vertex, Code[0], Size[0]) || (Key.Pixel && !GRHI->DevicePtr->GetShaderCode(Key.Pixel, Code[1], Size[1])))
	{
		return nullptr;
	}

	const nri::ShaderDesc Shaders[2] = { { nri::StageBits::VERTEX_SHADER, Code[0], Size[0] }, { nri::StageBits::FRAGMENT_SHADER, Code[1], Size[1] } };

	nri::ColorAttachmentDesc Colors[TargetSlots] = {};

	for (u32 Target = 0; Target < Key.TargetCount; ++Target)
	{
		const u8* Blend = Key.Blend[Target];
		Colors[Target].format = nri::Format(Key.Formats[Target]);
		Colors[Target].blendEnabled = Blend[0] != 0;
		Colors[Target].colorBlend = { Factors[Blend[1]], Factors[Blend[2]], Ops[Blend[3]] };
		Colors[Target].alphaBlend = { Factors[Blend[4]], Factors[Blend[5]], Ops[Blend[6]] };
		Colors[Target].colorWriteMask = nri::ColorWriteBits(Blend[7]);
	}

	nri::MultisampleDesc Multisample = {};
	Multisample.sampleNum = nri::Sample_t(std::max<u32>(Key.Samples, 1));
	Multisample.alphaToCoverage = Key.AlphaToCoverage != 0;

	// A dynamic draw's vertex shader pulls its records; its DRAWID input is the instance's number
	const nri::VertexAttributeDesc DrawId = { { "DRAWID", 0 }, { 0 }, 0, nri::Format::R32_UINT, 0 };
	const nri::VertexStreamDesc Stream = { 0, nri::VertexStreamStepRate::PER_INSTANCE, sizeof(u32) };
	const nri::VertexInputDesc Input = { &DrawId, 1, &Stream, 1 };

	nri::GraphicsPipelineDesc Desc = {};
	Desc.pipelineLayout = Key.Topology ? DynamicLayout : Layout;
	Desc.vertexInput = Key.Topology ? &Input : nullptr;
	// ERHI_PRIMITIVE_TOPOLOGY counts from an undefined one
	Desc.inputAssembly.topology = Key.Topology ? nri::Topology(Key.Topology - 1) : nri::Topology::TRIANGLE_LIST;
	Desc.rasterization.fillMode = Key.Wireframe ? nri::FillMode::WIREFRAME : nri::FillMode::SOLID;
	Desc.rasterization.cullMode = Culls[Key.Cull];
	Desc.rasterization.frontCounterClockwise = Key.FrontCounterClockwise != 0;
	Desc.multisample = Key.AlphaToCoverage || Key.Samples > 1 ? &Multisample : nullptr;
	Desc.outputMerger.colors = Colors;
	Desc.outputMerger.colorNum = Key.TargetCount;

	if (Key.Formats[TargetSlots])
	{
		const u8* Depth = Key.Depth;
		Desc.outputMerger.depthStencilFormat = nri::Format(Key.Formats[TargetSlots]);
		Desc.outputMerger.depth.compareOp = Compare[Depth[0]];
		Desc.outputMerger.depth.write = Depth[1] != 0;

		if (Depth[2])
		{
			Desc.outputMerger.stencil.front = { Compare[Depth[8]], StencilOps[Depth[5]], StencilOps[Depth[7]], StencilOps[Depth[6]], Depth[4], Depth[3] };
			Desc.outputMerger.stencil.back = { Compare[Depth[12]], StencilOps[Depth[9]], StencilOps[Depth[11]], StencilOps[Depth[10]], Depth[4], Depth[3] };
		}
	}

	Desc.shaders = Shaders;
	Desc.shaderNum = Key.Pixel ? 2 : 1;

	// A shader that reads what the layout lacks fails on D3D12: its draws are not drawn
	nri::Pipeline* Pipeline = nullptr;
	return Api.CreateGraphicsPipeline(*Gpu, Desc, Pipeline) == nri::Result::SUCCESS ? Pipeline : nullptr;
}

namespace
{
// Depth and typeless formats (DXGI numbering): reading them needs a plane and a typed view
bool IsPlainColor(ERHI_FORMAT Format)
{
	const u32 Dxgi = u32(Format);
	const bool Depth = (Dxgi >= 19 && Dxgi <= 22) || Dxgi == 39 || Dxgi == 40 || (Dxgi >= 44 && Dxgi <= 47) || Dxgi == 53 || Dxgi == 55;
	return !Depth && nri::nriConvertDXGIFormatToNRI(Dxgi) != nri::Format::UNKNOWN;
}

u32 TypelessDepthFormat(ERHI_FORMAT Format)
{
	const u32 Dxgi = u32(Format);
	return Dxgi == 19 ? 20u : Dxgi == 39 ? 40u : Dxgi == 44 ? 45u : Dxgi == 53 ? 55u : 0u;
}

u32 DepthTargetFormat(ERHI_FORMAT Format)
{
	const u32 Dxgi = u32(Format);
	return Dxgi >= 19 && Dxgi <= 22 ? 20u : Dxgi >= 39 && Dxgi <= 41 ? 40u : Dxgi >= 44 && Dxgi <= 47 ? 45u : Dxgi >= 53 && Dxgi <= 56 ? 55u : 0u;
}

// The old backend's view of one slice of an array does not tell the slice: it is found among the renderer's views of
// each slice, Count when it is not one of them
template <class TView, size_t Count>
u32 SliceOf(TView* View, TView* const (&Views)[Count])
{
	return View->GetSurface()->GetArraySize() > 1 ? u32(std::find(Views, Views + Count, View) - Views) : 0u;
}

// The slice a view of an array target writes: -1 where it is none of the renderer's slice views. Of the colour arrays,
// passes draw into a layer of the decals and a face of the reflection cubes
u32 TargetSlice(IRHIRenderTargetView* View)
{
	if (View->GetSurface()->GetArraySize() == 1)
	{
		return 0;
	}

	if (!RImplementation.Target)
	{
		return u32(-1);
	}

	const CRenderTarget& Target = *RImplementation.Target;

	if (const u32 Layer = SliceOf(View, RImplementation.DecalLayerTarget); Layer < CRender::DecalLayers)
	{
		return Layer;
	}

	for (const CRTC* Cube : { Target.rt_Reflection._get(), Target.rt_Reflection_temp._get() })
	{
		if (const u32 Face = Cube ? SliceOf(View, Cube->pRT) : 6u; Face < 6)
		{
			return Face;
		}
	}

	return u32(-1);
}

u32 TargetSlice(IRHIDepthStencilView* View)
{
	if (View->GetSurface()->GetArraySize() == 1)
	{
		return 0;
	}

	if (!RImplementation.Target)
	{
		return u32(-1);
	}

	const CRenderTarget& Target = *RImplementation.Target;
	const u32 Page = SliceOf(View, Target.rt_smap_depth_dsv);
	const u32 Cascade = SliceOf(View, Target.rt_smap_depth_sun_dsv);
	return Page < CRenderTarget::SmapPages ? Page : Cascade < std::size(Target.rt_smap_depth_sun_dsv) ? Cascade : u32(-1);
}

// The format a whole surface is copied through: 0 for a typeless colour
u32 CopyFormat(ERHI_FORMAT Format)
{
	return IsPlainColor(Format) ? u32(Format) : DepthTargetFormat(Format);
}

// One target of the old backend, cleared in a rendering scope of its own
template <class TView>
void ClearAttachment(TView* View, u32 Format, const nri::ClearAttachmentDesc& Clear)
{
	constexpr bool IsDepth = std::is_same_v<TView, IRHIDepthStencilView>;
	const u32 Slice = TargetSlice(View);

	if (!Format || Slice == u32(-1))
	{
		TellOnce(!Format ? "a clear of a target of a typeless or unknown format" : "a clear of a slice of an array the renderer does not know", IsDepth ? "depth" : "colour");
		return;
	}

	LoadApi();
	nri::Texture* Texture = WrapNriSurface(View->GetSurface(), Format);
	nri::TextureViewDesc ViewDesc = {};
	ViewDesc.texture = Texture;
	ViewDesc.type = IsDepth ? nri::TextureView::DEPTH_STENCIL_ATTACHMENT : nri::TextureView::COLOR_ATTACHMENT;
	ViewDesc.mipNum = 1;
	ViewDesc.layerOffset = nri::Dim_t(Slice);
	ViewDesc.layerNum = 1;
	nri::Descriptor* Attachment = nullptr;
	Api.CreateTextureView(ViewDesc, Attachment);

	nri::AttachmentDesc Target = {};
	Target.descriptor = Attachment;
	Target.loadOp = nri::LoadOp::LOAD;
	Target.storeOp = nri::StoreOp::STORE;
	nri::RenderingDesc Rendering = {};
	nri::CommandBuffer* Begun = nullptr;

	if constexpr (IsDepth)
	{
		Rendering.depth = Target;
		Begun = BeginNriCommands(nullptr, 0, nullptr, 0, View);
	}
	else
	{
		Rendering.colors = &Target;
		Rendering.colorNum = 1;
		Begun = BeginNriCommands(nullptr, 0, &View, 1, nullptr);
	}

	nri::CommandBuffer& Commands = *Begun;
	Api.CmdBeginRendering(Commands, Rendering);
	Api.CmdClearAttachments(Commands, &Clear, 1, nullptr, 0);
	Api.CmdEndRendering(Commands);
	EndNriCommands(&Commands);

	// The clear and the targets it binds take the descriptor when recorded
	Api.DestroyDescriptor(Attachment);
	Api.DestroyTexture(Texture);
}
} // namespace

void ClearNri(IRHIRenderTargetView* Target, const float* Color)
{
	nri::ClearAttachmentDesc Clear = {};
	Clear.value.color.f = { Color[0], Color[1], Color[2], Color[3] };
	Clear.planes = nri::PlaneBits::COLOR;
	ClearAttachment(Target, IsPlainColor(Target->GetSurface()->GetFormat()) ? u32(Target->GetSurface()->GetFormat()) : 0u, Clear);
}

void ClearNri(IRHIDepthStencilView* Target, ERHI_CLEAR_TARGET Flags, float Depth, u8 Stencil)
{
	// ERHI_CLEAR_TARGET is the D3D numbering: depth 1, stencil 2
	nri::ClearAttachmentDesc Clear = {};
	Clear.value.depthStencil = { Depth, Stencil };
	Clear.planes = nri::PlaneBits(u32(Flags) << 1);
	ClearAttachment(Target, DepthTargetFormat(Target->GetSurface()->GetFormat()), Clear);
}

void CopyNri(IRHISurface* Dest, IRHISurface* Source)
{
	IRHISurface* const Surfaces[2] = { Dest, Source };
	const u32 Formats[2] = { CopyFormat(Dest->GetFormat()), CopyFormat(Source->GetFormat()) };
	const bool IsResolve = Source->GetSampleDescCount() > 1 && Dest->GetSampleDescCount() == 1;

	// A depth surface is not resolved; neither API copies a multisampled one into a plain one
	if (!Formats[0] || !Formats[1] || (IsResolve && !IsPlainColor(Source->GetFormat())))
	{
		TellOnce(IsResolve ? "a copy of a multisampled depth into a plain surface" : "a copy of a typeless colour surface", "copy");
		return;
	}

	LoadApi();
	static const nri::AccessLayoutStage Ends[2][2] = {
		{ { nri::AccessBits::COPY_DESTINATION, nri::Layout::COPY_DESTINATION, nri::StageBits::COPY }, { nri::AccessBits::COPY_SOURCE, nri::Layout::COPY_SOURCE, nri::StageBits::COPY } },
		{ { nri::AccessBits::RESOLVE_DESTINATION, nri::Layout::RESOLVE_DESTINATION, nri::StageBits::RESOLVE }, { nri::AccessBits::RESOLVE_SOURCE, nri::Layout::RESOLVE_SOURCE, nri::StageBits::RESOLVE } } };
	nri::Texture* Wrapped[2];
	nri::TextureBarrierDesc Barriers[2] = {};

	for (u32 Index = 0; Index < 2; ++Index)
	{
		Wrapped[Index] = WrapNriSurface(Surfaces[Index], Formats[Index]);
		Barriers[Index].texture = Wrapped[Index];
		Barriers[Index].before = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::FRAGMENT_SHADER };
		Barriers[Index].after = Ends[IsResolve][Index];
		Barriers[Index].mipNum = nri::REMAINING;
		Barriers[Index].layerNum = nri::REMAINING;
	}

	// The old backend hands both over as read by shaders and takes them back so
	nri::CommandBuffer& Commands = *BeginNriCommands(Surfaces, 2, nullptr, 0, nullptr);
	nri::BarrierDesc Desc = {};
	Desc.textures = Barriers;
	Desc.textureNum = 2;
	Api.CmdBarrier(Commands, Desc);

	if (IsResolve)
	{
		Api.CmdResolveTexture(Commands, *Wrapped[0], nullptr, *Wrapped[1], nullptr, nri::ResolveOp::AVERAGE);
	}
	else
	{
		Api.CmdCopyTexture(Commands, *Wrapped[0], nullptr, *Wrapped[1], nullptr);
	}

	for (nri::TextureBarrierDesc& Barrier : Barriers)
	{
		std::swap(Barrier.before, Barrier.after);
	}

	Api.CmdBarrier(Commands, Desc);
	EndNriCommands(&Commands);
	Api.DestroyTexture(Wrapped[0]);
	Api.DestroyTexture(Wrapped[1]);
}

nri::SamplerDesc NriSampler(const RHISampleDesc& Source)
{
	return MakeSampler(Source);
}

u32 NriDepthFormat(ERHI_FORMAT Format)
{
	return DepthTargetFormat(Format);
}

nri::Descriptor* NriPass::WrapRead(IRHISurface* Surface)
{
	// A reset destroys this, and a level's frame may draw nothing through Draw to make it again: without its
	// wrappers the scene's draws had no shadow maps, light lists or sky after r_restart
	if (!IsCreated)
	{
		Create(RImplementation.Imm().Constants);
	}

	return Wrap(Surface, EView::Read, TypelessDepthFormat(Surface->GetFormat()));
}

nri::Descriptor* NriPass::WrapRead(IRHIBuffer* Buffer, u32 Stride)
{
	if (!IsCreated)
	{
		Create(RImplementation.Imm().Constants);
	}

	// Every time: the old backend may have copied into it since
	void* Native = GRHI->DevicePtr->ReadExternalBuffer(Buffer);
	xr_vector<BufferWrap>& Borrowed = Frames[FrameIndex].Borrowed;
	const auto Found = std::find_if(Borrowed.begin(), Borrowed.end(), [Native](const BufferWrap& Item) { return Item.Native == Native; });

	if (Found != Borrowed.end())
	{
		return Found->View;
	}

	BufferWrap& Item = Borrowed.emplace_back(BufferWrap{ Native, nullptr, nullptr });

	if (GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		nri::BufferD3D12Desc Desc = {};
		Desc.d3d12Resource = static_cast<ID3D12Resource*>(Native);
		Desc.structureStride = Stride;
		Wrapper12.CreateBufferD3D12(*Gpu, Desc, Item.Buffer);
	}
	else
	{
		nri::BufferD3D11Desc Desc = {};
		Desc.d3d11Resource = static_cast<ID3D11Resource*>(Native);
		Wrapper11.CreateBufferD3D11(*Gpu, Desc, Item.Buffer);
	}

	nri::BufferViewDesc View = {};
	View.buffer = Item.Buffer;
	View.type = nri::BufferView::STRUCTURED_BUFFER;
	View.structureStride = Stride;
	Api.CreateBufferView(View, Item.View);
	return Item.View;
}

ENriDecline NriPass::Collect(GfxCmdList& Cmd, Collected& Out, bool IsDynamic) const
{
	SPass* Pass = Cmd.Pass;
	Out.Declined = ~0u;

	if (!Pass || !Pass->vs || !Pass->ps || !Pass->vs->vs)
	{
		return ENriDecline::Pass;
	}

	// The engine's shaders have none of these stages: only an addon's pass names one
	if ((Pass->gs && Pass->gs->gs) || (Pass->hs && Pass->hs->sh) || (Pass->ds && Pass->ds->sh))
	{
		return ENriDecline::Stages;
	}

	// A view bound by hand stays bound for the frame: a pass reads there what NRI was given of it where its own textures
	// bind nothing over it. A dynamic draw reads its chunk where it pulls the ring; the skinned vertices and the pages
	// after the ring are not read for ring records. A fullscreen pass's vertex shader reads nothing
	const u64 HandReads = Cmd.RawViews[0] & Pass->ps->ReadSlots & ~Cmd.TextureMask[0];
	const u64 Pulled = ((1ull << VertexSlots) - 1) << ShaderBindings::Texture("g_RingVertices");
	const u64 VertexReads = Pass->vs->ReadSlots & ~(IsDynamic ? Pulled : 0ull);

	if (!IsDynamic && VertexReads)
	{
		Out.Declined = 0x100u | u32(std::countr_zero(VertexReads));
		return ENriDecline::Bindings;
	}

	Out.PixelGiven = 0;
	Out.VertexGiven = 0;
	Out.VertexEmpty = 0;

	for (u64 Left = VertexReads; Left; Left &= Left - 1)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		nri::Descriptor* View = Cmd.HandViews[1][Slot].View;

		if (!View && ((Cmd.RawViews[1] >> Slot) & 1))
		{
			Out.Declined = 0x100u | Slot;
			return ENriDecline::Bindings;
		}

		Out.Given[1][Slot] = View;
		Out.VertexGiven |= View ? 1ull << Slot : 0ull;
		Out.VertexEmpty |= View ? 0ull : 1ull << Slot;
	}

	IRHIDepthStencilView* DepthTarget = GRHI->GetDepthStencilView();
	Out.Depth = DepthTarget;
	Out.DepthFormat = DepthTarget ? DepthTargetFormat(DepthTarget->GetSurface()->GetFormat()) : 0;
	Out.DepthSlice = DepthTarget ? TargetSlice(DepthTarget) : 0;
	u32 Samples = DepthTarget ? DepthTarget->GetSurface()->GetSampleDescCount() : 0;

	if (DepthTarget && (!Out.DepthFormat || Out.DepthSlice == u32(-1)))
	{
		return ENriDecline::Depth;
	}

	Out.TargetCount = 0;

	for (u32 Slot = 0; Slot < TargetSlots; ++Slot)
	{
		IRHIRenderTargetView* Target = GRHI->GetRenderTargetView(Slot);

		if (!Target)
		{
			continue;
		}

		const u32 Slice = TargetSlice(Target);

		if (Slot != Out.TargetCount || Slice == u32(-1) || !IsPlainColor(Target->GetSurface()->GetFormat()))
		{
			return ENriDecline::Target;
		}

		const u32 Count = Target->GetSurface()->GetSampleDescCount();

		if (Samples && Count != Samples)
		{
			return ENriDecline::Samples;
		}

		Samples = Count;
		Out.TargetSlices[Out.TargetCount] = Slice;
		Out.Targets[Out.TargetCount++] = Target;
	}

	if (!Out.TargetCount && !DepthTarget)
	{
		return ENriDecline::Target;
	}

	Out.ReadCount = 0;
	Out.Slots = Pass->ps->ReadSlots;
	Out.Empty = Pass->ps->ReadSlots;

	const auto Read = [&](u32 Slot, IRHISurface* Surface)
	{
		Out.Empty &= ~(1ull << Slot);

		// Sampling the depth buffer a pass also tests against needs a read-only depth target
		const u32 Typed = TypelessDepthFormat(Surface->GetFormat());
		if ((!Typed && !IsPlainColor(Surface->GetFormat())) || (DepthTarget && Surface == DepthTarget->GetSurface()))
		{
			return false;
		}

		Out.ReadFormats[Out.ReadCount] = Typed;
		Out.ReadSlots[Out.ReadCount] = Slot;
		Out.Reads[Out.ReadCount++] = Surface;
		return true;
	};

	// A texture of the pass its pixel shader does not read is left out. A multisampled one is read through a view of
	// its samples, as the shader declares it. What NRI made is read through its own view, in the state it keeps
	for (u64 Used = Cmd.TextureMask[0] & Pass->ps->ReadSlots; Used; Used &= Used - 1)
	{
		const u32 Slot = u32(std::countr_zero(Used));
		CTexture* Texture = Cmd.Textures[0][Slot];

		if (nri::Descriptor* View = Texture ? Texture->BoundView() : nullptr)
		{
			Out.Empty &= ~(1ull << Slot);
			Out.Given[0][Slot] = View;
			Out.PixelGiven |= 1ull << Slot;
			continue;
		}

		IRHISurface* Surface = Texture ? Texture->BoundSurface() : nullptr;

		if (Surface && !Read(Slot, Surface))
		{
			return ENriDecline::Read;
		}
	}

	for (u64 Left = HandReads; Left; Left &= Left - 1)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		const GfxCmdList::HandView& Hand = Cmd.HandViews[0][Slot];

		if (Hand.Surface && !Read(Slot, Hand.Surface))
		{
			return ENriDecline::Read;
		}

		if (!Hand.Surface && !Hand.View)
		{
			Out.Declined = Slot;
			return ENriDecline::Bindings;
		}

		Out.Empty &= ~(1ull << Slot);
		Out.Given[0][Slot] = Hand.View;
		Out.PixelGiven |= Hand.View ? 1ull << Slot : 0ull;
	}

	Out.Kinds = Pass->ps->Kinds;
	const RHIBlendDesc& Blend = Cmd.Blend;
	PipelineKey& Key = Out.Key;
	std::memset(&Key, 0, sizeof(Key));
	Key.Vertex = Pass->vs->vs;
	Key.Pixel = Pass->ps->ps;
	Key.TargetCount = u8(Out.TargetCount);
	Key.Cull = u8(Cmd.Raster.CullMode);
	Key.Wireframe = Cmd.Raster.FillMode == RHI_FILL_WIREFRAME;
	Key.FrontCounterClockwise = Cmd.Raster.FrontCounterClockwise;
	Key.AlphaToCoverage = Blend.AlphaToCoverageEnable;
	Key.Samples = u8(Samples);

	if (DepthTarget)
	{
		const RHIDepthStencilDesc& Depth = Cmd.Depth;
		const u8 Fields[13] = {
			u8(Depth.DepthEnable ? u32(Depth.DepthFunc) : 0), u8(Depth.DepthEnable && Depth.DepthWriteMask != RHI_DEPTH_WRITE_MASK_ZERO), u8(Depth.StencilEnable), Depth.StencilReadMask, Depth.StencilWriteMask,
			u8(Depth.FrontFace.StencilFailOp), u8(Depth.FrontFace.StencilDepthFailOp), u8(Depth.FrontFace.StencilPassOp), u8(Depth.FrontFace.StencilFunc),
			u8(Depth.BackFace.StencilFailOp), u8(Depth.BackFace.StencilDepthFailOp), u8(Depth.BackFace.StencilPassOp), u8(Depth.BackFace.StencilFunc) };
		std::memcpy(Key.Depth, Fields, sizeof(Fields));
		Key.Formats[TargetSlots] = u32(nri::nriConvertDXGIFormatToNRI(Out.DepthFormat));
	}

	for (u32 Target = 0; Target < Out.TargetCount; ++Target)
	{
		const RHIRenderTargetBlendDesc& Source = Blend.RenderTarget[Blend.IndependentBlendEnable ? Target : 0];
		const u8 Fields[8] = { u8(Source.BlendEnable), u8(Source.SrcBlend), u8(Source.DestBlend), u8(Source.BlendOp), u8(Source.SrcBlendAlpha), u8(Source.DestBlendAlpha), u8(Source.BlendOpAlpha), Source.RenderTargetWriteMask };

		// No blender of the engine or XML blend names a blend factor: its constant is not passed on
		if (Source.BlendEnable && (Fields[1] == RHI_BLEND_BLEND_FACTOR || Fields[1] == RHI_BLEND_INV_BLEND_FACTOR || Fields[2] == RHI_BLEND_BLEND_FACTOR || Fields[2] == RHI_BLEND_INV_BLEND_FACTOR))
		{
			return ENriDecline::Blend;
		}

		std::memcpy(Key.Blend[Target], Fields, sizeof(Fields));
		Key.Formats[Target] = u32(nri::nriConvertDXGIFormatToNRI(u32(Out.Targets[Target]->GetSurface()->GetFormat())));
	}

	return ENriDecline::None;
}

ENriDecline NriPass::MakeViews(Collected& Bound, nri::Descriptor** Views)
{
	for (u32 Read = 0; Read < Bound.ReadCount; ++Read)
	{
		nri::Descriptor*& View = Views[Bound.ReadSlots[Read]];
		View = Wrap(Bound.Reads[Read], EView::Read, Bound.ReadFormats[Read]);

		if (!View)
		{
			Bound.Declined = Bound.ReadSlots[Read];
			return ENriDecline::Wrap;
		}
	}

	// The old backend read zero where it bound nothing: a multisampled, one-dimensional or typed-buffer read has no zero
	// view. A buffer goes into the range of textures, as both are shader resources to the device
	for (u64 Left = Bound.Empty; Left; Left &= Left - 1)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		Views[Slot] = ZeroViews[u32(Bound.Kinds[Slot])];

		if (!Views[Slot])
		{
			Bound.Declined = Slot;
			return ENriDecline::EmptyRead;
		}
	}

	for (u64 Left = Bound.PixelGiven; Left; Left &= Left - 1)
	{
		Views[std::countr_zero(Left)] = Bound.Given[0][std::countr_zero(Left)];
	}

	return ENriDecline::None;
}

void NriPass::Attach(const Collected& Bound, nri::AttachmentDesc* Colors, nri::AttachmentDesc& Depth)
{
	for (u32 Target = 0; Target < Bound.TargetCount; ++Target)
	{
		Colors[Target] = Loaded(Wrap(Bound.Targets[Target]->GetSurface(), EView::Color, 0, Bound.TargetSlices[Target]));
	}

	if (Bound.Depth)
	{
		Depth = Loaded(Wrap(Bound.Depth->GetSurface(), EView::Depth, Bound.DepthFormat, Bound.DepthSlice));
	}
}

void NriPass::Rects(const GfxCmdList& Cmd, const Collected& Bound, RHIViewport& Viewport, Irect& Scissor) const
{
	// The old backend's viewport stays as it was last set; without one the target's size is used
	Viewport = Cmd.LastViewport;

	if (Viewport.Width <= 0.0f || Viewport.Height <= 0.0f)
	{
		IRHISurface* Sized = Bound.TargetCount ? Bound.Targets[0]->GetSurface() : Bound.Depth->GetSurface();
		Viewport.Width = float(Sized->GetWidth());
		Viewport.Height = float(Sized->GetHeight());
	}

	Scissor = Cmd.LastScissor;

	if (!Cmd.Raster.ScissorEnable)
	{
		Scissor.set(s32(Viewport.TopLeftX), s32(Viewport.TopLeftY), s32(Viewport.TopLeftX + Viewport.Width), s32(Viewport.TopLeftY + Viewport.Height));
	}
}

namespace
{
void SetRects(nri::CommandBuffer& Commands, const RHIViewport& Area, const Irect& Clip)
{
	const nri::Viewport Viewport = { Area.TopLeftX, Area.TopLeftY, Area.Width, Area.Height, Area.MinDepth, Area.MaxDepth };
	const nri::Rect Scissor = { s16(Clip.x1), s16(Clip.y1), nri::Dim_t(std::max(Clip.x2 - Clip.x1, 0)), nri::Dim_t(std::max(Clip.y2 - Clip.y1, 0)) };
	Api.CmdSetViewports(Commands, &Viewport, 1);
	Api.CmdSetScissors(Commands, &Scissor, 1);
}
} // namespace

void NriPass::Drop(const GfxCmdList& Cmd, ENriDecline Reason, u32 Register)
{
	static bool IsTold[size_t(ENriDecline::Count)] = {};
	++Counting.Dropped;

	if (std::exchange(IsTold[size_t(Reason)], true))
	{
		return;
	}

	static const char* const Stages[] = { "pixel", "vertex" };
	string64 Where = {};

	if (Register >> 8 < std::size(Stages))
	{
		xr_sprintf(Where, ", %s register t%u", Stages[Register >> 8], Register & 0xFFu);
	}

	const SPass* Pass = Cmd.Pass;
	const char* Vertex = Pass && Pass->vs ? *Pass->vs->cName : "none";
	const char* Pixel = Pass && Pass->ps ? *Pass->ps->cName : "none";
	Msg("! NRI pass: a draw with %s%s (vertex shader %s, pixel shader %s) is not drawn", Declines[size_t(Reason)], Where, Vertex, Pixel);
}

void NriPass::Draw(GfxCmdList& Cmd, u32 VertexCount)
{
	if (!IsCreated)
	{
		Create(Cmd.Constants);
	}

	Collected Bound;
	nri::Descriptor* Views[TextureSlots];

	if (const ENriDecline Reason = Collect(Cmd, Bound, false); Reason != ENriDecline::None)
	{
		Drop(Cmd, Reason, Bound.Declined);
		return;
	}

	if (const ENriDecline Reason = MakeViews(Bound, Views); Reason != ENriDecline::None)
	{
		Drop(Cmd, Reason, Bound.Declined);
		return;
	}

	nri::Pipeline* Pipeline = FindPipeline(Bound.Key);
	nri::DescriptorSet* Set = nullptr;
	nri::DescriptorPool* Pool = Pipeline ? AllocateSets(Frames[FrameIndex].Passes, false, *Layout, &Set, 1) : nullptr;

	if (!Pool)
	{
		Drop(Cmd, Pipeline ? ENriDecline::Sets : ENriDecline::Pipeline);
		return;
	}

	nri::CommandBuffer& Commands = *BeginNriCommands(Bound.Reads, Bound.ReadCount, Bound.Targets, Bound.TargetCount, Bound.Depth);
	nri::UpdateDescriptorRangeDesc Updates[TextureSlots];
	u32 UpdateCount = 0;

	for (u64 Left = Bound.Slots; Left; Left &= Left - 1, ++UpdateCount)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		Updates[UpdateCount] = { Set, 0, Slot, &Views[Slot], 1 };
	}

	Api.UpdateDescriptorRanges(Updates, UpdateCount);

	Api.CmdSetDescriptorPool(Commands, *Pool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::GRAPHICS, *Layout);
	Api.CmdSetPipeline(Commands, *Pipeline);

	u32 Block = 0;
	Cmd.Constants.ForEachBlock([&](u32, const void* Data, u32 Size)
	{
		nri::SetRootDescriptorDesc Root = {};
		Root.rootDescriptorIndex = Block;
		Root.descriptor = BlockViews[Block++];
		Root.offset = StreamApi.StreamConstantData(*Streamer, Data, Size);
		Api.CmdSetRootDescriptor(Commands, Root);
	});

	RHIViewport Area;
	Irect Clip;
	Rects(Cmd, Bound, Area, Clip);
	SetRects(Commands, Area, Clip);

	if (Bound.Depth && Cmd.Depth.StencilEnable)
	{
		Api.CmdSetStencilReference(Commands, u8(Cmd.StencilRef), u8(Cmd.StencilRef));
	}

	nri::AttachmentDesc Colors[TargetSlots] = {};
	nri::RenderingDesc Rendering = {};
	Attach(Bound, Colors, Rendering.depth);
	Rendering.colors = Colors;
	Rendering.colorNum = Bound.TargetCount;
	Api.CmdBeginRendering(Commands, Rendering);

	// Textures are bound once the targets are: on D3D11 a texture still bound as a render target
	// cannot be bound as an input
	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = Set;
	Api.CmdSetDescriptorSet(Commands, SetDesc);
	const nri::DrawDesc Triangles = { VertexCount, 1, 0, 0 };
	Api.CmdDraw(Commands, Triangles);
	Api.CmdEndRendering(Commands);
	EndNriCommands(&Commands);
	Cmd.RestoreInput();
}

bool NriPass::BeginTiles(GfxCmdList& Cmd, IRHISurface* const* Reads, u32 Count)
{
	if (!IsCreated)
	{
		Create(Cmd.Constants);
	}

	Collected& Bound = Tiles.Bound;
	ENriDecline Reason = Collect(Cmd, Bound, false);
	Reason = Reason == ENriDecline::None ? MakeViews(Bound, Tiles.Views) : Reason;
	nri::Pipeline* Pipeline = Reason == ENriDecline::None ? FindPipeline(Bound.Key) : nullptr;

	if (!Pipeline)
	{
		Drop(Cmd, Reason == ENriDecline::None ? ENriDecline::Pipeline : Reason, Bound.Declined);
		return false;
	}

	Tiles.Pool = nullptr;
	Tiles.Commands = BeginNriCommands(Reads, Count, Bound.Targets, Bound.TargetCount, Bound.Depth);
	Api.CmdSetPipelineLayout(*Tiles.Commands, nri::BindPoint::GRAPHICS, *Layout);
	Api.CmdSetPipeline(*Tiles.Commands, *Pipeline);

	nri::AttachmentDesc Colors[TargetSlots] = {};
	nri::RenderingDesc Rendering = {};
	Attach(Bound, Colors, Rendering.depth);
	Rendering.colors = Colors;
	Rendering.colorNum = Bound.TargetCount;
	Api.CmdBeginRendering(*Tiles.Commands, Rendering);
	return true;
}

void NriPass::AddTile(GfxCmdList& Cmd, IRHISurface* Read)
{
	const Collected& Bound = Tiles.Bound;
	nri::CommandBuffer& Commands = *Tiles.Commands;
	nri::Descriptor* View = Wrap(Read, EView::Read, TypelessDepthFormat(Read->GetFormat()));
	nri::DescriptorSet* Set = nullptr;
	nri::DescriptorPool* Pool = View ? AllocateSets(Frames[FrameIndex].Passes, false, *Layout, &Set, 1) : nullptr;

	if (!Pool)
	{
		Drop(Cmd, View ? ENriDecline::Sets : ENriDecline::Wrap);
		return;
	}

	if (Bound.ReadCount)
	{
		Tiles.Views[Bound.ReadSlots[0]] = View;
	}

	nri::UpdateDescriptorRangeDesc Updates[TextureSlots];
	u32 UpdateCount = 0;

	for (u64 Left = Bound.Slots; Left; Left &= Left - 1, ++UpdateCount)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		Updates[UpdateCount] = { Set, 0, Slot, &Tiles.Views[Slot], 1 };
	}

	Api.UpdateDescriptorRanges(Updates, UpdateCount);

	if (Pool != Tiles.Pool)
	{
		Api.CmdSetDescriptorPool(Commands, *Pool);
		Tiles.Pool = Pool;
	}

	u32 Block = 0;
	Cmd.Constants.ForEachBlock([&](u32, const void* Data, u32 Size)
	{
		nri::SetRootDescriptorDesc Root = {};
		Root.rootDescriptorIndex = Block;
		Root.descriptor = BlockViews[Block++];
		Root.offset = StreamApi.StreamConstantData(*Streamer, Data, Size);
		Api.CmdSetRootDescriptor(Commands, Root);
	});

	RHIViewport Area;
	Irect Clip;
	Rects(Cmd, Bound, Area, Clip);
	SetRects(Commands, Area, Clip);

	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = Set;
	Api.CmdSetDescriptorSet(Commands, SetDesc);
	Api.CmdDraw(Commands, { 3, 1, 0, 0 });
}

void NriPass::EndTiles(GfxCmdList& Cmd)
{
	Api.CmdEndRendering(*Tiles.Commands);
	EndNriCommands(Tiles.Commands);
	Cmd.RestoreInput();
}

void NriPass::CreateDynamic()
{
	const nri::DescriptorRangeBits Flags =nri::DescriptorRangeBits::PARTIALLY_BOUND | nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET;
	const nri::DescriptorRangeDesc Ranges[2] = {
		{ 0, TextureSlots, nri::DescriptorType::TEXTURE, nri::StageBits::FRAGMENT_SHADER, Flags },
		{ 0, TextureSlots, nri::DescriptorType::STRUCTURED_BUFFER, nri::StageBits::VERTEX_SHADER, Flags } };
	DynamicLayout = CreateLayout(Ranges, 2, u32(nri::StageBits::VERTEX_SHADER | nri::StageBits::FRAGMENT_SHADER));

	xr_vector<u32> Numbers(GfxCmdList::IotaSize);
	std::iota(Numbers.begin(), Numbers.end(), 0u);
	nri::BufferDesc Desc = {};
	Desc.size = Numbers.size() * sizeof(u32);
	Desc.usage = nri::BufferUsageBits::VERTEX;
	Api.CreateCommittedBuffer(*Gpu, nri::MemoryLocation::DEVICE, 0.0f, Desc, Iota);
	Api.SetDebugName(Iota, "NRI pass draw numbers");

	const nri::BufferUploadDesc Upload = { Numbers.data(), Iota, { nri::AccessBits::VERTEX_BUFFER, nri::StageBits::VERTEX_SHADER } };
	Help.UploadData(*GRHI->Nri->Queue, nullptr, 0, &Upload, 1);
}

const NriPass::DynamicSet* NriPass::FindSet(const SetKey& Key, nri::Descriptor* const* PixelViews, nri::Descriptor* const* VertexViews)
{
	Frame& Current = Frames[FrameIndex];
	const auto [Found, IsNew] = Current.Sets.try_emplace(Key, DynamicSet{});
	DynamicSet& Item = Found->second;

	if (!IsNew)
	{
		return &Item;
	}

	Item.Pool = AllocateSets(Current.Dynamic, true, *DynamicLayout, &Item.Set, 1);

	if (!Item.Pool)
	{
		Current.Sets.erase(Found);
		return nullptr;
	}

	nri::UpdateDescriptorRangeDesc Updates[DynamicReads];
	u32 UpdateCount = 0;

	for (u64 Left = Key.PixelSlots; Left; Left &= Left - 1, ++UpdateCount)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		Updates[UpdateCount] = { Item.Set, 0, Slot, &PixelViews[Slot], 1 };
	}

	for (u64 Left = Key.VertexSlots; Left; Left &= Left - 1, ++UpdateCount)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		Updates[UpdateCount] = { Item.Set, 1, Slot, &VertexViews[Slot], 1 };
	}

	Api.UpdateDescriptorRanges(Updates, UpdateCount);
	return &Item;
}

bool NriPass::DrawDynamic(GfxCmdList& Cmd, const NriDynamicDraw& Item)
{
	Collected Bound;
	DynamicBatch Batch;

	if (!ResolveDynamic(Cmd, Item, Bound, Batch))
	{
		return false;
	}

	IssueDynamic(Cmd, Batch, Bound.Reads);
	return true;
}

u32 NriPass::RecordDynamic(GfxCmdList& Cmd, const NriDynamicDraw& Item)
{
	Collected Bound;
	DynamicBatch Batch;

	if (!ResolveDynamic(Cmd, Item, Bound, Batch))
	{
		return ~0u;
	}

	Batch.FirstRead = u32(BatchReads.size());
	BatchReads.insert(BatchReads.end(), Bound.Reads, Bound.Reads + Bound.ReadCount);
	Batches.push_back(Batch);
	return u32(Batches.size() - 1);
}

// A pass a batch was most of what a few hundred particle effects cost the frame: on D3D12 the batches of a row that
// draw into the same targets share one, which reads what any of them reads
void NriPass::SubmitDynamic(GfxCmdList& Cmd, u32 First, u32 Count)
{
	if (GRHI->APILevel != ERHI_API_LAYER::D3D12)
	{
		for (u32 Batch = First; Batch < First + Count; ++Batch)
		{
			SubmitDynamic(Cmd, Batch);
		}

		return;
	}

	for (u32 Batch = First; Batch < First + Count;)
	{
		const DynamicBatch& Lead = Batches[Batch];

		if (!Lead.Pipeline)
		{
			++Batch;
			continue;
		}

		IRHISurface* Reads[64];
		u32 ReadCount = 0;
		u32 End = Batch;

		for (; End < First + Count; ++End)
		{
			const DynamicBatch& Item = Batches[End];

			if (!Item.Pipeline)
			{
				continue;
			}

			if (Item.ColorCount != Lead.ColorCount || Item.DepthTarget != Lead.DepthTarget || !std::equal(Item.Targets, Item.Targets + Item.ColorCount, Lead.Targets))
			{
				break;
			}

			u32 Added = ReadCount;

			for (u32 Read = 0; Read < Item.ReadCount && Added <= std::size(Reads); ++Read)
			{
				IRHISurface* Surface = BatchReads[Item.FirstRead + Read];

				if (std::find(Reads, Reads + Added, Surface) == Reads + Added)
				{
					if (Added < std::size(Reads))
					{
						Reads[Added] = Surface;
					}

					++Added;
				}
			}

			if (Added > std::size(Reads))
			{
				break;
			}

			ReadCount = Added;
		}

		Group = BeginNriCommands(Reads, ReadCount, Lead.Targets, Lead.ColorCount, Lead.DepthTarget);
		++RunEpoch;

		for (; Batch < End; ++Batch)
		{
			if (Batches[Batch].Pipeline)
			{
				IssueDynamic(Cmd, Batches[Batch], nullptr);
			}
		}

		if (IsRunRendering)
		{
			Api.CmdEndRendering(*Group);
			IsRunRendering = false;
		}

		EndNriCommands(Group);
		Group = nullptr;
		Cmd.RestoreInput();
	}
}

void NriPass::SubmitDynamic(GfxCmdList& Cmd, u32 Batch)
{
	VERIFY(Batch < Batches.size());
	const DynamicBatch& Item = Batches[Batch];

	if (Item.Pipeline)
	{
		IssueDynamic(Cmd, Item, BatchReads.data() + Item.FirstRead);
	}
}

// Everything a dynamic draw binds is taken from Cmd and the targets now: views, set and pipeline live for the frame
// slot, the constants are streamed into a ring that holds every queued frame's
bool NriPass::ResolveDynamic(GfxCmdList& Cmd, const NriDynamicDraw& Item, Collected& Bound, DynamicBatch& Out)
{
	if (!IsCreated)
	{
		Create(Cmd.Constants);
	}

	if (!DynamicLayout)
	{
		CreateDynamic();
	}

	nri::Descriptor* Views[TextureSlots];
	const auto Fail = [&](ENriDecline Reason)
	{
		Drop(Cmd, Reason, Bound.Declined);
		return false;
	};

	if (const ENriDecline Reason = Collect(Cmd, Bound, true); Reason != ENriDecline::None)
	{
		return Fail(Reason);
	}

	if (const ENriDecline Reason = MakeViews(Bound, Views); Reason != ENriDecline::None)
	{
		return Fail(Reason);
	}

	for (u64 Left = Bound.VertexEmpty; Left; Left &= Left - 1)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		Bound.Given[1][Slot] = ZeroViews[u32(Cmd.Pass->vs->Kinds[Slot])];
		Bound.VertexGiven |= 1ull << Slot;

		if (!Bound.Given[1][Slot])
		{
			Bound.Declined = 0x100u | Slot;
			return Fail(ENriDecline::EmptyRead);
		}
	}

	const u32 Ring = ShaderBindings::Texture("g_RingVertices");
	const u64 VertexSlotsRead = Bound.VertexGiven | 1ull << Ring;
	Bound.Given[1][Ring] = Item.Chunk;

	if (u32(std::popcount(Bound.Slots) + std::popcount(VertexSlotsRead)) > DynamicReads)
	{
		return Fail(ENriDecline::Reads);
	}

	Bound.Key.Topology = u8(Item.Topology);
	nri::Pipeline* Pipeline = FindPipeline(Bound.Key);

	if (!Pipeline)
	{
		return Fail(ENriDecline::Pipeline);
	}

	SetKey Key = {};
	Key.PixelSlots = Bound.Slots;
	Key.VertexSlots = VertexSlotsRead;
	u32 ViewCount = 0;

	for (u64 Left = Bound.Slots; Left; Left &= Left - 1)
	{
		Key.Views[ViewCount++] = Views[std::countr_zero(Left)];
	}

	for (u64 Left = VertexSlotsRead; Left; Left &= Left - 1)
	{
		Key.Views[ViewCount++] = Bound.Given[1][std::countr_zero(Left)];
	}

	const DynamicSet* Set = FindSet(Key, Views, Bound.Given[1]);

	if (!Set)
	{
		return Fail(ENriDecline::Sets);
	}

	Out.Pipeline = Pipeline;
	Out.Pool = Set->Pool;
	Out.Set = Set->Set;
	Out.VertexCount = Item.VertexCount;
	Out.InstanceCount = Item.InstanceCount;
	Out.FirstRead = 0;
	Out.ReadCount = Bound.ReadCount;

	// The first dynamic draw of a frame streams every block
	const bool IsForced = !HasDynamicBlocks;
	const u32 Dirty = Cmd.Constants.TakeNriDirty() | (IsForced ? ~0u : 0u);
	HasDynamicBlocks = true;
	u32 Block = 0;

	Cmd.Constants.ForEachBlock([&](u32, const void* Data, u32 Size)
	{
		u8* Last = LastBlocks.data() + LastBlockOffsets[Block];

		if (((Dirty >> Block) & 1) && (IsForced || std::memcmp(Last, Data, Size)))
		{
			std::memcpy(Last, Data, Size);
			DynamicOffsets[Block] = StreamApi.StreamConstantData(*Streamer, Data, Size);
		}

		Out.Offsets[Block] = DynamicOffsets[Block];
		++Block;
	});

	Rects(Cmd, Bound, Out.Viewport, Out.Scissor);
	Out.HasStencil = Bound.Depth && Cmd.Depth.StencilEnable;
	Out.StencilRef = Cmd.StencilRef;

	nri::AttachmentDesc Colors[TargetSlots] = {};
	nri::AttachmentDesc Depth = {};
	Attach(Bound, Colors, Depth);

	for (u32 Target = 0; Target < TargetSlots; ++Target)
	{
		Out.Colors[Target] = Colors[Target].descriptor;
		Out.Targets[Target] = Target < Bound.TargetCount ? Bound.Targets[Target] : nullptr;
	}

	Out.ColorCount = Bound.TargetCount;
	Out.Depth = Depth.descriptor;
	Out.DepthTarget = Bound.Depth;
	return true;
}

// D3D12 records a dynamic draw in place, between the old backend's, and binds all it uses each time. D3D11 keeps a run
// open in Replay and binds only what changed since the run's last draw
void NriPass::IssueDynamic(GfxCmdList& Cmd, const DynamicBatch& Item, IRHISurface* const* Reads)
{
	const bool IsD3D12 = GRHI->APILevel == ERHI_API_LAYER::D3D12;
	nri::CommandBuffer* Commands = Replay;

	if (Group)
	{
		Commands = Group;
	}
	else if (IsD3D12)
	{
		Commands = BeginNriCommands(Reads, Item.ReadCount, Item.Targets, Item.ColorCount, Item.DepthTarget);
	}
	else if (!IsRunOpen)
	{
		Commands = BeginNriCommands(nullptr, 0, nullptr, 0, nullptr);
		IsRunOpen = true;
		++RunEpoch;
		++Counting.Runs;
	}

	const bool IsFresh = (IsD3D12 && !Group) || Run.Epoch != RunEpoch;

	if (IsFresh)
	{
		Run = {};
		Run.Epoch = RunEpoch;
		Api.CmdSetPipelineLayout(*Commands, nri::BindPoint::GRAPHICS, *DynamicLayout);
		const nri::VertexBufferDesc Stream = { Iota, 0, sizeof(u32) };
		Api.CmdSetVertexBuffers(*Commands, 0, &Stream, 1);
	}

	if (Item.Pool != Run.Pool)
	{
		Api.CmdSetDescriptorPool(*Commands, *Item.Pool);
		Run.Pool = Item.Pool;
	}

	if (Item.Pipeline != Run.Pipeline)
	{
		Api.CmdSetPipeline(*Commands, *Item.Pipeline);
		Run.Pipeline = Item.Pipeline;
	}

	for (u32 Block = 0; Block < BlockCount; ++Block)
	{
		if (IsFresh || Run.Offsets[Block] != Item.Offsets[Block])
		{
			nri::SetRootDescriptorDesc Root = {};
			Root.rootDescriptorIndex = Block;
			Root.descriptor = BlockViews[Block];
			Root.offset = Item.Offsets[Block];
			Api.CmdSetRootDescriptor(*Commands, Root);
			Run.Offsets[Block] = Item.Offsets[Block];
		}
	}

	if (IsFresh || std::memcmp(&Item.Viewport, &Run.Viewport, sizeof(Item.Viewport)) || std::memcmp(&Item.Scissor, &Run.Scissor, sizeof(Item.Scissor)))
	{
		SetRects(*Commands, Item.Viewport, Item.Scissor);
		Run.Viewport = Item.Viewport;
		Run.Scissor = Item.Scissor;
	}

	if (Item.HasStencil && (IsFresh || Run.StencilRef != Item.StencilRef))
	{
		Api.CmdSetStencilReference(*Commands, u8(Item.StencilRef), u8(Item.StencilRef));
		Run.StencilRef = Item.StencilRef;
	}

	bool IsRetargeted = IsFresh || Item.ColorCount != Run.ColorCount || Item.Depth != Run.Depth;

	for (u32 Target = 0; Target < Item.ColorCount; ++Target)
	{
		IsRetargeted |= Item.Colors[Target] != Run.Colors[Target];
		Run.Colors[Target] = Item.Colors[Target];
	}

	if (IsRetargeted)
	{
		if (IsRunRendering)
		{
			Api.CmdEndRendering(*Commands);
		}

		nri::AttachmentDesc Colors[TargetSlots] = {};
		nri::RenderingDesc Rendering = {};

		for (u32 Target = 0; Target < Item.ColorCount; ++Target)
		{
			Colors[Target] = Loaded(Item.Colors[Target]);
		}

		if (Item.Depth)
		{
			Rendering.depth = Loaded(Item.Depth);
		}

		Rendering.colors = Colors;
		Rendering.colorNum = Item.ColorCount;
		Api.CmdBeginRendering(*Commands, Rendering);
		IsRunRendering = !IsD3D12 || Group;
		Run.ColorCount = Item.ColorCount;
		Run.Depth = Item.Depth;
		// Bound once the targets are: on D3D11 binding a target drops it from the inputs
		Run.Set = nullptr;
	}

	if (Item.Set != Run.Set)
	{
		nri::SetDescriptorSetDesc SetDesc = {};
		SetDesc.descriptorSet = Item.Set;
		Api.CmdSetDescriptorSet(*Commands, SetDesc);
		Run.Set = Item.Set;
	}

	Api.CmdDraw(*Commands, { Item.VertexCount, Item.InstanceCount, 0, 0 });
	++Counting.Dynamic;

	if (IsD3D12 && !Group)
	{
		Api.CmdEndRendering(*Commands);
		EndNriCommands(Commands);
		Cmd.RestoreInput();
	}
}

namespace
{
// How many registers from the lowest of Left stand in a row and are of one kind: they make one range of the set, which
// an array of textures a shader indexes has to lie in
u32 RunLength(u64 Left, u64 Buffers)
{
	const u32 First = u32(std::countr_zero(Left));
	const u64 Kinds = Buffers >> First;
	return std::min(u32(std::countr_one(Left >> First)), u32(std::countr_zero(Kinds & 1 ? ~Kinds : Kinds)));
}

u64 WithoutRun(u64 Left, u32 Count)
{
	return Count < 64 ? Left & ~(((1ull << Count) - 1) << std::countr_zero(Left)) : 0;
}
} // namespace

const NriPass::ComputePipeline* NriPass::FindCompute(const SCS& Shader)
{
	for (const ComputePipeline& Item : ComputePipelines)
	{
		if (Item.Shader == Shader.sh)
		{
			return &Item;
		}
	}

	const void* Code = nullptr;
	size_t Size = 0;

	if (!GRHI->DevicePtr->GetShaderCode(Shader.sh, Code, Size))
	{
		return nullptr;
	}

	// A range a run of read registers, then one a written register, in the order Dispatch fills them: every one the
	// shader reads or writes is given a view
	nri::DescriptorRangeDesc Ranges[TextureSlots + StorageSlots] = {};
	u32 RangeCount = 0;

	for (u64 Left = Shader.ReadSlots; Left;)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		const u32 Count = RunLength(Left, Shader.BufferSlots);
		const nri::DescriptorType Type = (Shader.BufferSlots >> Slot) & 1 ? nri::DescriptorType::STRUCTURED_BUFFER : nri::DescriptorType::TEXTURE;
		Ranges[RangeCount++] = { Slot, Count, Type, nri::StageBits::COMPUTE_SHADER, nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET };
		Left = WithoutRun(Left, Count);
	}

	for (u32 Left = Shader.WriteSlots; Left; Left &= Left - 1)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		const nri::DescriptorType Type = (Shader.StorageSlots >> Slot) & 1 ? nri::DescriptorType::STORAGE_TEXTURE : nri::DescriptorType::STORAGE_STRUCTURED_BUFFER;
		Ranges[RangeCount++] = { Slot, 1, Type, nri::StageBits::COMPUTE_SHADER, nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET };
	}

	ComputePipeline Item = { Shader.sh, CreateLayout(Ranges, RangeCount, u32(nri::StageBits::COMPUTE_SHADER)), nullptr };
	nri::ComputePipelineDesc Desc = {};
	Desc.pipelineLayout = Item.Layout;
	Desc.shader = { nri::StageBits::COMPUTE_SHADER, Code, Size };

	if (Item.Layout)
	{
		Api.CreateComputePipeline(*Gpu, Desc, Item.Pipeline);
	}

	ComputePipelines.push_back(Item);
	return &ComputePipelines.back();
}

void NriPass::Dispatch(GfxCmdList& Cmd, SCS* Shader, const NriBinding* Reads, u32 ReadCount, const NriBinding* Writes, u32 WriteCount, u32 GroupsX, u32 GroupsY, u32 GroupsZ, nri::Buffer* Arguments, u64 ArgumentsOffset)
{
	constexpr u32 Compute = CTexture::rstCompute / CTexture::rstVertex;
	const auto Fail = [&Cmd, Shader](const char* Why)
	{
		TellOnce(Why, Shader ? *Shader->cName : "no shader");
		Cmd.UnbindComputeTextures();
	};

	if (!Shader || !Shader->sh || (Shader->WriteSlots >> StorageSlots) || WriteCount > StorageSlots)
	{
		return Fail("a dispatch that writes past the eighth register or more than eight views");
	}

	if (!IsCreated)
	{
		Create(Cmd.Constants);
	}

	// By register: the reads, then the writes
	nri::Descriptor* Views[TextureSlots + StorageSlots] = {};
	IRHISurface* Surfaces[TextureSlots + StorageSlots];
	IRHISurface* Written[StorageSlots];
	nri::BufferBarrierDesc BufferBarriers[StorageSlots + 1] = {};
	u32 SurfaceCount = 0;
	u32 WrittenCount = 0;
	u32 BufferCount = 0;
	u64 Bound = 0;
	const nri::AccessStage Readable = { nri::AccessBits::SHADER_RESOURCE, nri::StageBits::ALL };
	const nri::AccessStage Writable = { nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER };

	for (u32 Index = 0; Index < WriteCount; ++Index)
	{
		const NriBinding& Write = Writes[Index];
		IRHISurface* Surface = Write.Surface;

		if (Write.Slot >= StorageSlots || (((Shader->WriteSlots & ~Shader->StorageSlots) >> Write.Slot) & 1) != u32(Write.Buffer != nullptr) || (Write.Buffer && !Write.View))
		{
			return Fail("a dispatch that writes a buffer where its shader writes a texture or none, or the reverse");
		}

		if (Write.Buffer)
		{
			Views[TextureSlots + Write.Slot] = Write.View;
			BufferBarriers[BufferCount].buffer = Write.Buffer;
			BufferBarriers[BufferCount].before = Write.IsNew ? nri::AccessStage{} : Readable;
			BufferBarriers[BufferCount++].after = Writable;
			continue;
		}

		if (Surface && (Surface->GetSampleDescCount() > 1 || !IsPlainColor(Surface->GetFormat())))
		{
			return Fail("a dispatch that writes a multisampled or typeless texture");
		}

		Views[TextureSlots + Write.Slot] = Surface ? Wrap(Surface, EView::Storage, 0, 0, Write.Mip) : DroppedView;

		if (!Views[TextureSlots + Write.Slot])
		{
			return Fail("a dispatch that writes a texture NRI has no view of");
		}

		if (Surface && std::find(Written, Written + WrittenCount, Surface) == Written + WrittenCount)
		{
			Written[WrittenCount++] = Surface;
			Surfaces[SurfaceCount++] = Surface;
		}
	}

	// A surface is handed over in one state: what a pass writes it cannot read
	const auto Read = [&](u32 Slot, IRHISurface* Surface)
	{
		const u32 Typed = TypelessDepthFormat(Surface->GetFormat());

		if ((!Typed && !IsPlainColor(Surface->GetFormat())) || std::find(Written, Written + WrittenCount, Surface) != Written + WrittenCount)
		{
			return false;
		}

		Views[Slot] = Wrap(Surface, EView::Read, Typed);
		Bound |= 1ull << Slot;

		if (std::find(Surfaces, Surfaces + SurfaceCount, Surface) == Surfaces + SurfaceCount)
		{
			Surfaces[SurfaceCount++] = Surface;
		}

		return Views[Slot] != nullptr;
	};

	for (u32 Index = 0; Index < ReadCount; ++Index)
	{
		const NriBinding& Binding = Reads[Index];

		if (Binding.Slot >= TextureSlots || (!Binding.View && !Binding.Surface))
		{
			return Fail("a dispatch given a register past the last or nothing to read");
		}

		if (Binding.View)
		{
			Views[Binding.Slot] = Binding.View;
			Bound |= 1ull << Binding.Slot;
		}
		else if (!Read(Binding.Slot, Binding.Surface))
		{
			return Fail("a dispatch that reads what it writes or a texture it cannot read");
		}
	}

	for (u64 Used = Cmd.TextureMask[Compute] & Shader->ReadSlots & ~Bound; Used; Used &= Used - 1)
	{
		const u32 Slot = u32(std::countr_zero(Used));
		CTexture* Texture = Cmd.Textures[Compute][Slot];

		if (nri::Descriptor* View = Texture && Texture->pSurface ? Texture->BoundView() : nullptr)
		{
			Views[Slot] = View;
			Bound |= 1ull << Slot;
		}
		else if (Texture && Texture->pSurface && !Read(Slot, Texture->pSurface))
		{
			return Fail("a dispatch that reads what it writes or a texture it cannot read");
		}
	}

	// The old backend read zero where it bound nothing
	for (u64 Left = Shader->ReadSlots & ~Bound; Left; Left &= Left - 1)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		Views[Slot] = ZeroViews[u32(Shader->Kinds[Slot])];

		if (!Views[Slot])
		{
			return Fail("a dispatch with an empty read of a kind with no zero view");
		}
	}

	for (u32 Left = Shader->WriteSlots; Left; Left &= Left - 1)
	{
		const u32 Slot = u32(std::countr_zero(Left));
		nri::Descriptor*& View = Views[TextureSlots + Slot];
		View = View || !((Shader->StorageSlots >> Slot) & 1) ? View : DroppedView;

		if (!View)
		{
			return Fail("a dispatch that writes a buffer it was not given");
		}
	}

	const ComputePipeline* Pipeline = FindCompute(*Shader);
	nri::DescriptorSet* Set = nullptr;
	nri::DescriptorPool* Pool = Pipeline && Pipeline->Pipeline ? AllocateSets(Frames[FrameIndex].Passes, false, *Pipeline->Layout, &Set, 1) : nullptr;

	if (!Pool)
	{
		return Fail("a dispatch with no pipeline or descriptor set");
	}

	nri::UpdateDescriptorRangeDesc Updates[TextureSlots + StorageSlots];
	u32 UpdateCount = 0;

	for (u64 Left = Shader->ReadSlots; Left; ++UpdateCount)
	{
		const u32 Count = RunLength(Left, Shader->BufferSlots);
		Updates[UpdateCount] = { Set, UpdateCount, 0, &Views[std::countr_zero(Left)], Count };
		Left = WithoutRun(Left, Count);
	}

	for (u32 Left = Shader->WriteSlots; Left; Left &= Left - 1, ++UpdateCount)
	{
		Updates[UpdateCount] = { Set, UpdateCount, 0, &Views[TextureSlots + std::countr_zero(Left)], 1 };
	}

	Api.UpdateDescriptorRanges(Updates, UpdateCount);

	nri::TextureBarrierDesc Barriers[StorageSlots] = {};

	for (u32 Index = 0; Index < WrittenCount; ++Index)
	{
		Barriers[Index].texture = WrapNriSurface(Written[Index]);
		Barriers[Index].before = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::ALL };
		Barriers[Index].after = { nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::Layout::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER };
		Barriers[Index].mipNum = nri::REMAINING;
		Barriers[Index].layerNum = nri::REMAINING;
	}

	// The groups of the pass as a buffer holds them: read by shaders before and after, as what a pass writes
	if (Arguments)
	{
		BufferBarriers[BufferCount] = { Arguments, Readable, { nri::AccessBits::ARGUMENT_BUFFER, nri::StageBits::INDIRECT } };
	}

	nri::BarrierDesc Barrier = {};
	Barrier.buffers = BufferBarriers;
	Barrier.bufferNum = BufferCount + (Arguments ? 1 : 0);
	Barrier.textures = Barriers;
	Barrier.textureNum = WrittenCount;

	// D3D11 runs the pass when it ends and forgets the graphics shaders then: an old draw that skins its mesh goes on
	const auto Shaders = std::to_array(Cmd.Shaders);

	// The old backend hands every surface over as read by shaders and takes them back so
	nri::CommandBuffer& Commands = *BeginNriCommands(Surfaces, SurfaceCount, nullptr, 0, nullptr);
	Api.CmdBarrier(Commands, Barrier);
	Api.CmdSetDescriptorPool(Commands, *Pool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *Pipeline->Layout);
	Api.CmdSetPipeline(Commands, *Pipeline->Pipeline);

	u32 Block = 0;
	Cmd.Constants.ForEachBlock([&](u32, const void* Data, u32 Size)
	{
		nri::SetRootDescriptorDesc Root = {};
		Root.rootDescriptorIndex = Block;
		Root.descriptor = BlockViews[Block++];
		Root.offset = StreamApi.StreamConstantData(*Streamer, Data, Size);
		Api.CmdSetRootDescriptor(Commands, Root);
	});

	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = Set;
	Api.CmdSetDescriptorSet(Commands, SetDesc);

	if (Arguments)
	{
		Api.CmdDispatchIndirect(Commands, *Arguments, ArgumentsOffset);
		std::swap(BufferBarriers[BufferCount].before, BufferBarriers[BufferCount].after);
	}
	else
	{
		Api.CmdDispatch(Commands, { GroupsX, GroupsY, GroupsZ });
	}

	for (u32 Index = 0; Index < WrittenCount; ++Index)
	{
		std::swap(Barriers[Index].before, Barriers[Index].after);
	}

	for (u32 Index = 0; Index < BufferCount; ++Index)
	{
		BufferBarriers[Index].before = Writable;
		BufferBarriers[Index].after = Readable;
	}

	Api.CmdBarrier(Commands, Barrier);
	EndNriCommands(&Commands);

	for (u32 Index = 0; Index < WrittenCount; ++Index)
	{
		Api.DestroyTexture(Barriers[Index].texture);
	}

	// NRI's D3D11 replay leaves its views bound: what the pass wrote would be dropped from the next one's inputs
	IRHIUnorderedAccessView* const Unbound[StorageSlots] = {};
	GRHI->SetComputeUAVs(0, StorageSlots, Unbound);

	for (u32 Stage = 0; Stage < Shaders.size(); ++Stage)
	{
		Cmd.SetShader(Shaders[Stage], ERHI_SHADER_TYPE(Stage));
	}

	Cmd.RestoreInput();
	Cmd.UnbindComputeTextures();
}

nri::Descriptor* NriPass::StreamRecords(const void* Data, u32 Bytes, u32 Stride)
{
	if (!IsCreated)
	{
		Create(RImplementation.Imm().Constants);
	}

	// The same passes ask for the same records in the same order every frame: a frame slot keeps its buffers
	Frame& Current = Frames[FrameIndex];

	if (Current.BufferCount == Current.Buffers.size())
	{
		Current.Buffers.push_back({});
	}

	Records& Item = Current.Buffers[Current.BufferCount++];

	if (Item.Stride != Stride || Item.Bytes < Bytes)
	{
		Api.DestroyDescriptor(Item.View);
		Api.DestroyBuffer(Item.Buffer);
		Item = { nullptr, nullptr, Item.Stride == Stride ? std::max(Bytes, Item.Bytes) : Bytes, Stride };

		nri::BufferDesc Desc = {};
		Desc.size = Item.Bytes;
		Desc.structureStride = Stride;
		Desc.usage = nri::BufferUsageBits::SHADER_RESOURCE;
		Api.CreateCommittedBuffer(*Gpu, nri::MemoryLocation::HOST_UPLOAD, 0.0f, Desc, Item.Buffer);

		nri::BufferViewDesc View = {};
		View.buffer = Item.Buffer;
		View.type = nri::BufferView::STRUCTURED_BUFFER;
		View.structureStride = Stride;
		Api.CreateBufferView(View, Item.View);
	}

	std::memcpy(Api.MapBuffer(*Item.Buffer, 0, Bytes), Data, Bytes);
	Api.UnmapBuffer(*Item.Buffer);
	return Item.View;
}

nri::Pipeline* NriPass::FindMipPipeline(u32 Format)
{
	for (const auto& [Known, Pipeline] : MipPipelines)
	{
		if (Known == Format)
		{
			return Pipeline;
		}
	}

	const void* Code[2] = {};
	size_t Size[2] = {};

	if (!MipVertex || !MipPixel || !MipVertex->vs || !MipPixel->ps || !GRHI->DevicePtr->GetShaderCode(MipVertex->vs, Code[0], Size[0]) || !GRHI->DevicePtr->GetShaderCode(MipPixel->ps, Code[1], Size[1]))
	{
		return nullptr;
	}

	const nri::ShaderDesc Shaders[2] = { { nri::StageBits::VERTEX_SHADER, Code[0], Size[0] }, { nri::StageBits::FRAGMENT_SHADER, Code[1], Size[1] } };
	nri::ColorAttachmentDesc Color = {};
	Color.format = nri::Format(Format);
	Color.colorWriteMask = nri::ColorWriteBits::RGBA;

	nri::GraphicsPipelineDesc Desc = {};
	Desc.pipelineLayout = MipLayout;
	Desc.inputAssembly.topology = nri::Topology::TRIANGLE_LIST;
	Desc.rasterization.fillMode = nri::FillMode::SOLID;
	Desc.rasterization.cullMode = nri::CullMode::NONE;
	Desc.outputMerger.colors = &Color;
	Desc.outputMerger.colorNum = 1;
	Desc.shaders = Shaders;
	Desc.shaderNum = 2;

	nri::Pipeline* Pipeline = nullptr;
	Api.CreateGraphicsPipeline(*Gpu, Desc, Pipeline);
	MipPipelines.push_back({ Format, Pipeline });
	return Pipeline;
}

// A draw a mip and a layer: an sRGB target is read and written through its sRGB views, so the four texels are
// averaged in linear light, which a storage view of the format could not do
void NriPass::GenerateMips(GfxCmdList& Cmd, IRHISurface* Surface, u64 Layers)
{
	constexpr u32 MostMips = 16;

	if (Surface->GetSampleDescCount() > 1 || !IsPlainColor(Surface->GetFormat()))
	{
		return TellOnce("mips of a multisampled or typeless texture", "mips");
	}

	if (!IsCreated)
	{
		Create(Cmd.Constants);
	}

	if (!MipLayout)
	{
		MipVertex = DEV->_CreateVS("mip_downsample");
		MipPixel = DEV->_CreatePS("mip_downsample");

		const nri::DescriptorRangeDesc Source = { 0, 1, nri::DescriptorType::TEXTURE, nri::StageBits::FRAGMENT_SHADER, nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET };
		nri::DescriptorSetDesc Set = {};
		Set.ranges = &Source;
		Set.rangeNum = 1;
		const nri::RootConstantDesc Layer = { 0, sizeof(u32), nri::StageBits::FRAGMENT_SHADER };

		nri::PipelineLayoutDesc Desc = {};
		Desc.rootConstants = &Layer;
		Desc.rootConstantNum = 1;
		Desc.descriptorSets = &Set;
		Desc.descriptorSetNum = 1;
		Desc.shaderStages = nri::StageBits::VERTEX_SHADER | nri::StageBits::FRAGMENT_SHADER;
		Api.CreatePipelineLayout(*Gpu, Desc, MipLayout);
	}

	nri::Pipeline* Pipeline = MipLayout ? FindMipPipeline(u32(nri::nriConvertDXGIFormatToNRI(u32(Surface->GetFormat())))) : nullptr;
	nri::Texture* Texture = WrapNriSurface(Surface);
	const nri::TextureDesc& Desc = Api.GetTextureDesc(*Texture);
	const u32 Mips = Desc.mipNum;
	Layers &= Desc.layerNum < 64 ? (1ull << Desc.layerNum) - 1 : ~0ull;

	nri::DescriptorSet* Sets[MostMips];

	// A texture of one mip or no layer asked for has nothing to make
	if (Mips < 2 || !Layers)
	{
		Api.DestroyTexture(Texture);
		return;
	}

	nri::DescriptorPool* Pool = Mips <= MostMips && Pipeline ? AllocateSets(Frames[FrameIndex].Passes, false, *MipLayout, Sets, Mips - 1) : nullptr;

	if (!Pool)
	{
		Api.DestroyTexture(Texture);
		return TellOnce(Mips > MostMips ? "mips of a texture of more than sixteen" : "mips with no pipeline or descriptor sets", "mips");
	}

	nri::Descriptor* Sources[MostMips];
	nri::Descriptor* Targets[MostMips * 64];
	u32 TargetCount = 0;

	nri::TextureBarrierDesc Barrier = {};
	Barrier.texture = Texture;
	Barrier.mipNum = 1;
	Barrier.layerNum = nri::REMAINING;
	nri::BarrierDesc Barriers = {};
	Barriers.textures = &Barrier;
	Barriers.textureNum = 1;
	const nri::AccessLayoutStage Read = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::ALL };
	const nri::AccessLayoutStage Drawn = { nri::AccessBits::COLOR_ATTACHMENT, nri::Layout::COLOR_ATTACHMENT, nri::StageBits::COLOR_ATTACHMENT };

	// The old backend hands the surface over as read by shaders and takes it back so
	nri::CommandBuffer& Commands = *BeginNriCommands(&Surface, 1, nullptr, 0, nullptr);
	Api.CmdSetDescriptorPool(Commands, *Pool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::GRAPHICS, *MipLayout);
	Api.CmdSetPipeline(Commands, *Pipeline);

	for (u32 Mip = 1; Mip < Mips; ++Mip)
	{
		nri::TextureViewDesc View = {};
		View.texture = Texture;
		View.type = nri::TextureView::TEXTURE_ARRAY;
		View.mipOffset = nri::Dim_t(Mip - 1);
		View.mipNum = 1;
		Api.CreateTextureView(View, Sources[Mip - 1]);
		const nri::UpdateDescriptorRangeDesc Update = { Sets[Mip - 1], 0, 0, &Sources[Mip - 1], 1 };
		Api.UpdateDescriptorRanges(&Update, 1);

		Barrier.mipOffset = nri::Dim_t(Mip);
		Barrier.before = Read;
		Barrier.after = Drawn;
		Api.CmdBarrier(Commands, Barriers);

		const nri::Dim_t Width = nri::Dim_t(std::max(u32(Desc.width) >> Mip, 1u));
		const nri::Dim_t Height = nri::Dim_t(std::max(u32(Desc.height) >> Mip, 1u));
		const nri::Viewport Viewport = { 0.0f, 0.0f, float(Width), float(Height), 0.0f, 1.0f };
		const nri::Rect Scissor = { 0, 0, Width, Height };
		View.type = nri::TextureView::COLOR_ATTACHMENT;
		View.mipOffset = nri::Dim_t(Mip);
		View.layerNum = 1;

		for (u64 Left = Layers; Left; Left &= Left - 1)
		{
			const u32 Layer = u32(std::countr_zero(Left));
			View.layerOffset = nri::Dim_t(Layer);
			Api.CreateTextureView(View, Targets[TargetCount]);

			nri::AttachmentDesc Attachment = {};
			Attachment.descriptor = Targets[TargetCount++];
			Attachment.loadOp = nri::LoadOp::LOAD;
			Attachment.storeOp = nri::StoreOp::STORE;
			nri::RenderingDesc Rendering = {};
			Rendering.colors = &Attachment;
			Rendering.colorNum = 1;
			Api.CmdBeginRendering(Commands, Rendering);
			Api.CmdSetViewports(Commands, &Viewport, 1);
			Api.CmdSetScissors(Commands, &Scissor, 1);

			// Bound once the target is: on D3D11 the mip above was a render target a moment ago
			nri::SetDescriptorSetDesc SetDesc = {};
			SetDesc.descriptorSet = Sets[Mip - 1];
			Api.CmdSetDescriptorSet(Commands, SetDesc);
			nri::SetRootConstantsDesc Constants = {};
			Constants.data = &Layer;
			Constants.size = sizeof(Layer);
			Api.CmdSetRootConstants(Commands, Constants);
			Api.CmdDraw(Commands, { 3, 1, 0, 0 });
			Api.CmdEndRendering(Commands);
		}

		Barrier.before = Drawn;
		Barrier.after = Read;
		Api.CmdBarrier(Commands, Barriers);
	}

	EndNriCommands(&Commands);

	for (u32 Index = 0; Index < TargetCount; ++Index)
	{
		Api.DestroyDescriptor(Targets[Index]);
	}

	for (u32 Mip = 1; Mip < Mips; ++Mip)
	{
		Api.DestroyDescriptor(Sources[Mip - 1]);
	}

	Api.DestroyTexture(Texture);
	Cmd.RestoreInput();
}

void DrawFullscreen(GfxCmdList& Cmd, u32 VertexCount)
{
	RImplementation.NriPasses.Draw(Cmd, VertexCount);
}

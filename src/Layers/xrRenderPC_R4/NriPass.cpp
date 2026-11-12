#include "stdafx.h"
#include "NriPass.h"

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
nri::StreamerInterface StreamApi;
nri::WrapperD3D11Interface Wrapper11;
nri::WrapperD3D12Interface Wrapper12;

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

// The wrapper is made per pass: NRI caches what it has bound, and the old backend binds behind its
// back. Without NVAPI, NRI's D3D11 command buffer ignores the context it is given: it only stores
// the commands and replays them on the immediate context when submitted.
nri::CommandBuffer* BeginNriCommands(IRHISurface* const* Reads, u32 ReadCount, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth)
{
	LoadApi();
	void* Native = GRHI->BeginExternalPass(Reads, ReadCount, Targets, TargetCount, Depth);
	nri::CommandBuffer* Wrapper = nullptr;

	if (GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		nri::CommandBufferD3D12Desc Desc = {};
		Desc.d3d12CommandList = static_cast<ID3D12CommandList*>(Native);
		Wrapper12.CreateCommandBufferD3D12(*Gpu, Desc, Wrapper);
	}
	else
	{
		nri::CommandBufferD3D11Desc Desc = {};
		Desc.d3d11DeviceContext = static_cast<ID3D11DeviceContext*>(Native);
		Wrapper11.CreateCommandBufferD3D11(*Gpu, Desc, Wrapper);
		Api.BeginCommandBuffer(*Wrapper, nullptr);
	}

	return Wrapper;
}

void EndNriCommands(nri::CommandBuffer* Wrapper)
{
	if (GRHI->APILevel != ERHI_API_LAYER::D3D12)
	{
		Api.EndCommandBuffer(*Wrapper);
		nri::QueueSubmitDesc Submit = {};
		Submit.commandBuffers = &Wrapper;
		Submit.commandBufferNum = 1;
		Api.QueueSubmit(*GRHI->Nri->Queue, Submit);
	}

	Api.DestroyCommandBuffer(Wrapper);
	GRHI->EndExternalPass();
}

void NriPass::Create(const CShaderConstants& Constants)
{
	LoadApi();
	IsCreated = true;

	nri::StreamerDesc StreamerDesc = {};
	StreamerDesc.constantBufferMemoryLocation = nri::MemoryLocation::HOST_UPLOAD;
	StreamerDesc.constantBufferSize = 4 << 20;
	StreamerDesc.dynamicBufferMemoryLocation = nri::MemoryLocation::HOST_UPLOAD;
	StreamerDesc.dynamicBufferDesc.usage = nri::BufferUsageBits::VERTEX;
	StreamerDesc.queuedFrameNum = FrameCount - 1;
	StreamApi.CreateStreamer(*Gpu, StreamerDesc, Streamer);

	nri::RootDescriptorDesc Roots[BlockSlots] = {};
	nri::Buffer* ConstantBuffer = StreamApi.GetStreamerConstantBuffer(*Streamer);
	Constants.ForEachBlock([&](u32 Slot, const void*, u32 Size)
	{
		R_ASSERT(BlockCount < BlockSlots);
		Roots[BlockCount].registerIndex = Slot;
		Roots[BlockCount].descriptorType = nri::DescriptorType::CONSTANT_BUFFER;

		nri::BufferViewDesc View = {};
		View.buffer = ConstantBuffer;
		View.type = nri::BufferView::CONSTANT_BUFFER;
		View.size = (Size + 255) & ~255u;
		Api.CreateBufferView(View, BlockViews[BlockCount++]);
	});

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

	nri::DescriptorRangeDesc Textures = {};
	Textures.descriptorNum = TextureSlots;
	Textures.descriptorType = nri::DescriptorType::TEXTURE;
	Textures.shaderStages = nri::StageBits::FRAGMENT_SHADER;
	Textures.flags = nri::DescriptorRangeBits::PARTIALLY_BOUND;
	nri::DescriptorSetDesc Set = {};
	Set.ranges = &Textures;
	Set.rangeNum = 1;

	nri::PipelineLayoutDesc LayoutDesc = {};
	LayoutDesc.rootDescriptors = Roots;
	LayoutDesc.rootDescriptorNum = BlockCount;
	LayoutDesc.rootSamplers = Samplers;
	LayoutDesc.rootSamplerNum = SamplerCount;
	LayoutDesc.descriptorSets = &Set;
	LayoutDesc.descriptorSetNum = 1;
	LayoutDesc.shaderStages = nri::StageBits::VERTEX_SHADER | nri::StageBits::FRAGMENT_SHADER;
	Api.CreatePipelineLayout(*Gpu, LayoutDesc, Layout);

	nri::DescriptorPoolDesc PoolDesc = {};
	PoolDesc.descriptorSetMaxNum = 128;
	PoolDesc.textureMaxNum = 128 * TextureSlots;

	for (Frame& Item : Frames)
	{
		Api.CreateDescriptorPool(*Gpu, PoolDesc, Item.Pool);
	}
}

void NriPass::ReleaseFrame(Frame& Item)
{
	for (const Wrapped& Texture : Item.Textures)
	{
		Api.DestroyDescriptor(Texture.View);
		Api.DestroyTexture(Texture.Texture);
	}

	Item.Textures.clear();
	Api.ResetDescriptorPool(*Item.Pool);
}

void NriPass::EndFrame()
{
	if (!IsCreated)
	{
		return;
	}

	StreamApi.EndStreamerFrame(*Streamer);
	FrameIndex = (FrameIndex + 1) % FrameCount;
	ReleaseFrame(Frames[FrameIndex]);
}

void NriPass::Destroy()
{
	if (!IsCreated)
	{
		return;
	}

	// The old backend owns the queue: its flush covers the passes recorded into its lists
	GRHI->DevicePtr->Flush();

	for (Frame& Item : Frames)
	{
		ReleaseFrame(Item);
		Api.DestroyDescriptorPool(Item.Pool);
		Item.Pool = nullptr;
	}

	for (const CachedPipeline& Item : Pipelines)
	{
		Api.DestroyPipeline(Item.Pipeline);
	}
	Pipelines.clear();

	for (u32 Block = 0; Block < BlockCount; ++Block)
	{
		Api.DestroyDescriptor(BlockViews[Block]);
	}

	BlockCount = 0;
	Api.DestroyPipelineLayout(Layout);
	StreamApi.DestroyStreamer(Streamer);
	Gpu = nullptr;
	IsCreated = false;
}

nri::Descriptor* NriPass::Wrap(IRHISurface* Surface, EView Kind, u32 TypedFormat)
{
	Frame& Current = Frames[FrameIndex];

	for (const Wrapped& Item : Current.Textures)
	{
		if (Item.Surface == Surface && Item.Kind == Kind)
		{
			return Item.View;
		}
	}

	const s32 Format = s32(TypedFormat ? TypedFormat : u32(Surface->GetFormat()));
	Wrapped Item = { Surface, Kind, nullptr, nullptr };

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
	static const nri::TextureView Types[] = { nri::TextureView::TEXTURE, nri::TextureView::COLOR_ATTACHMENT, nri::TextureView::DEPTH_STENCIL_ATTACHMENT };
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

	View.mipNum = Kind == EView::Read ? nri::REMAINING : nri::Dim_t(1);
	View.layerNum = Kind == EView::Read ? nri::REMAINING : nri::Dim_t(1);
	Api.CreateTextureView(View, Item.View);

	Current.Textures.push_back(Item);
	return Item.View;
}

nri::Pipeline* NriPass::FindPipeline(const PipelineKey& Key)
{
	for (const CachedPipeline& Item : Pipelines)
	{
		if (!std::memcmp(&Item.Key, &Key, sizeof(Key)))
		{
			return Item.Pipeline;
		}
	}

	// ERHI_BLEND and ERHI_BLEND_OP keep the D3D numbering
	static const nri::BlendFactor Factors[] = {
		nri::BlendFactor::ZERO, nri::BlendFactor::ZERO, nri::BlendFactor::ONE, nri::BlendFactor::SRC_COLOR, nri::BlendFactor::ONE_MINUS_SRC_COLOR,
		nri::BlendFactor::SRC_ALPHA, nri::BlendFactor::ONE_MINUS_SRC_ALPHA, nri::BlendFactor::DST_ALPHA, nri::BlendFactor::ONE_MINUS_DST_ALPHA,
		nri::BlendFactor::DST_COLOR, nri::BlendFactor::ONE_MINUS_DST_COLOR, nri::BlendFactor::SRC_ALPHA_SATURATE, nri::BlendFactor::ZERO, nri::BlendFactor::ZERO,
		nri::BlendFactor::CONSTANT_COLOR, nri::BlendFactor::ONE_MINUS_CONSTANT_COLOR, nri::BlendFactor::SRC1_COLOR, nri::BlendFactor::ONE_MINUS_SRC1_COLOR,
		nri::BlendFactor::SRC1_ALPHA, nri::BlendFactor::ONE_MINUS_SRC1_ALPHA };
	static const nri::BlendOp Ops[] = { nri::BlendOp::ADD, nri::BlendOp::ADD, nri::BlendOp::SUBTRACT, nri::BlendOp::REVERSE_SUBTRACT, nri::BlendOp::MIN, nri::BlendOp::MAX };
	static const nri::CullMode Culls[] = { nri::CullMode::NONE, nri::CullMode::NONE, nri::CullMode::FRONT, nri::CullMode::BACK };

	nri::ShaderDesc Shaders[2] = {};
	const void* Code = nullptr;
	size_t Size = 0;
	bool HasCode = GRHI->DevicePtr->GetShaderCode(Key.Vertex, Code, Size);
	R_ASSERT(HasCode);
	Shaders[0].stage = nri::StageBits::VERTEX_SHADER;
	Shaders[0].bytecode = Code;
	Shaders[0].size = Size;
	HasCode = GRHI->DevicePtr->GetShaderCode(Key.Pixel, Code, Size);
	R_ASSERT(HasCode);
	Shaders[1].stage = nri::StageBits::FRAGMENT_SHADER;
	Shaders[1].bytecode = Code;
	Shaders[1].size = Size;

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
	Multisample.sampleNum = 1;
	Multisample.alphaToCoverage = Key.AlphaToCoverage != 0;

	nri::GraphicsPipelineDesc Desc = {};
	Desc.pipelineLayout = Layout;
	Desc.inputAssembly.topology = nri::Topology::TRIANGLE_LIST;
	Desc.rasterization.fillMode = Key.Wireframe ? nri::FillMode::WIREFRAME : nri::FillMode::SOLID;
	Desc.rasterization.cullMode = Culls[Key.Cull];
	Desc.rasterization.frontCounterClockwise = Key.FrontCounterClockwise != 0;
	Desc.multisample = Key.AlphaToCoverage ? &Multisample : nullptr;
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
	Desc.shaderNum = 2;

	nri::Pipeline* Pipeline = nullptr;
	Api.CreateGraphicsPipeline(*Gpu, Desc, Pipeline);
	Pipelines.push_back({ Key, Pipeline });
	return Pipeline;
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
} // namespace

u32 NriDepthFormat(ERHI_FORMAT Format)
{
	return DepthTargetFormat(Format);
}

bool NriPass::Draw(GfxCmdList& Cmd, u32 VertexCount)
{
	SPass* Pass = Cmd.Pass;

	if (!Pass || !Pass->vs || !Pass->ps || !Pass->vs->vs || !Pass->ps->ps)
	{
		return false;
	}

	if ((Pass->gs && Pass->gs->gs) || (Pass->hs && Pass->hs->sh) || (Pass->ds && Pass->ds->sh))
	{
		return false;
	}

	if (Cmd.RawPixelViews || Cmd.UavMask || Cmd.TextureMask[1] || Cmd.Raster.ScissorEnable)
	{
		return false;
	}

	IRHIDepthStencilView* DepthTarget = GRHI->GetDepthStencilView();
	const u32 DepthFormat = DepthTarget ? DepthTargetFormat(DepthTarget->GetSurface()->GetFormat()) : 0;

	// A view of one slice of an array (shadow maps, cube faces) is not wrapped yet
	if (DepthTarget && (!DepthFormat || DepthTarget->GetDimension() != ERHI_DSV_DIMENSION::TEXTURE2D || DepthTarget->GetSurface()->GetArraySize() > 1))
	{
		return false;
	}

	IRHIRenderTargetView* Targets[TargetSlots];
	u32 TargetCount = 0;

	for (u32 Slot = 0; Slot < TargetSlots; ++Slot)
	{
		IRHIRenderTargetView* Target = GRHI->GetRenderTargetView(Slot);

		if (!Target)
		{
			continue;
		}

		if (Slot != TargetCount || Target->GetSurface()->GetArraySize() > 1 || !IsPlainColor(Target->GetSurface()->GetFormat()))
		{
			return false;
		}

		Targets[TargetCount++] = Target;
	}

	if (!TargetCount && !DepthTarget)
	{
		return false;
	}

	IRHISurface* Reads[TextureSlots];
	u32 ReadFormats[TextureSlots];
	u32 Slots[TextureSlots];
	u32 ReadCount = 0;

	for (u64 Used = Cmd.TextureMask[0]; Used; Used &= Used - 1)
	{
		const u32 Slot = u32(std::countr_zero(Used));
		CTexture* Texture = Cmd.Textures[0][Slot];
		IRHISurface* Surface = Texture ? Texture->pSurface : nullptr;

		if (!Surface || Surface->GetSampleDescCount() > 1)
		{
			return false;
		}

		// Sampling the depth buffer a pass also tests against needs a read-only depth target
		const u32 Typed = TypelessDepthFormat(Surface->GetFormat());
		if ((!Typed && !IsPlainColor(Surface->GetFormat())) || (DepthTarget && Surface == DepthTarget->GetSurface()))
		{
			return false;
		}

		ReadFormats[ReadCount] = Typed;
		Slots[ReadCount] = Slot;
		Reads[ReadCount++] = Surface;
	}

	const RHIBlendDesc& Blend = Cmd.Blend;
	PipelineKey Key;
	std::memset(&Key, 0, sizeof(Key));
	Key.Vertex = Pass->vs->vs;
	Key.Pixel = Pass->ps->ps;
	Key.TargetCount = u8(TargetCount);
	Key.Cull = u8(Cmd.Raster.CullMode);
	Key.Wireframe = Cmd.Raster.FillMode == RHI_FILL_WIREFRAME;
	Key.FrontCounterClockwise = Cmd.Raster.FrontCounterClockwise;
	Key.AlphaToCoverage = Blend.AlphaToCoverageEnable;

	if (DepthTarget)
	{
		const RHIDepthStencilDesc& Depth = Cmd.Depth;
		const u8 Fields[13] = {
			u8(Depth.DepthEnable ? u32(Depth.DepthFunc) : 0), u8(Depth.DepthEnable && Depth.DepthWriteMask != RHI_DEPTH_WRITE_MASK_ZERO), u8(Depth.StencilEnable), Depth.StencilReadMask, Depth.StencilWriteMask,
			u8(Depth.FrontFace.StencilFailOp), u8(Depth.FrontFace.StencilDepthFailOp), u8(Depth.FrontFace.StencilPassOp), u8(Depth.FrontFace.StencilFunc),
			u8(Depth.BackFace.StencilFailOp), u8(Depth.BackFace.StencilDepthFailOp), u8(Depth.BackFace.StencilPassOp), u8(Depth.BackFace.StencilFunc) };
		std::memcpy(Key.Depth, Fields, sizeof(Fields));
		Key.Formats[TargetSlots] = u32(nri::nriConvertDXGIFormatToNRI(DepthFormat));
	}

	for (u32 Target = 0; Target < TargetCount; ++Target)
	{
		const RHIRenderTargetBlendDesc& Source = Blend.RenderTarget[Blend.IndependentBlendEnable ? Target : 0];
		const u8 Fields[8] = { u8(Source.BlendEnable), u8(Source.SrcBlend), u8(Source.DestBlend), u8(Source.BlendOp), u8(Source.SrcBlendAlpha), u8(Source.DestBlendAlpha), u8(Source.BlendOpAlpha), Source.RenderTargetWriteMask };

		// A blend factor constant is not passed on yet
		if (Source.BlendEnable && (Fields[1] == RHI_BLEND_BLEND_FACTOR || Fields[1] == RHI_BLEND_INV_BLEND_FACTOR || Fields[2] == RHI_BLEND_BLEND_FACTOR || Fields[2] == RHI_BLEND_INV_BLEND_FACTOR))
		{
			return false;
		}

		std::memcpy(Key.Blend[Target], Fields, sizeof(Fields));
		Key.Formats[Target] = u32(nri::nriConvertDXGIFormatToNRI(u32(Targets[Target]->GetSurface()->GetFormat())));
	}

	if (!IsCreated)
	{
		Create(Cmd.Constants);
	}

	Frame& Current = Frames[FrameIndex];

	nri::CommandBuffer& Commands = *BeginNriCommands(Reads, ReadCount, Targets, TargetCount, DepthTarget);

	nri::DescriptorSet* Set = nullptr;
	Api.AllocateDescriptorSets(*Current.Pool, *Layout, 0, &Set, 1, 0);
	nri::Descriptor* Views[TextureSlots];
	nri::UpdateDescriptorRangeDesc Updates[TextureSlots] = {};

	for (u32 Read = 0; Read < ReadCount; ++Read)
	{
		Views[Read] = Wrap(Reads[Read], EView::Read, ReadFormats[Read]);
		Updates[Read].descriptorSet = Set;
		Updates[Read].baseDescriptor = Slots[Read];
		Updates[Read].descriptors = &Views[Read];
		Updates[Read].descriptorNum = 1;
	}

	Api.UpdateDescriptorRanges(Updates, ReadCount);

	Api.CmdSetDescriptorPool(Commands, *Current.Pool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::GRAPHICS, *Layout);
	Api.CmdSetPipeline(Commands, *FindPipeline(Key));

	u32 Block = 0;
	Cmd.Constants.ForEachBlock([&](u32, const void* Data, u32 Size)
	{
		nri::SetRootDescriptorDesc Root = {};
		Root.rootDescriptorIndex = Block;
		Root.descriptor = BlockViews[Block++];
		Root.offset = StreamApi.StreamConstantData(*Streamer, Data, Size);
		Api.CmdSetRootDescriptor(Commands, Root);
	});

	// The old backend's viewport stays as it was last set; without one the target's size is used
	RHIViewport Area = Cmd.LastViewport;

	if (Area.Width <= 0.0f || Area.Height <= 0.0f)
	{
		IRHISurface* Sized = TargetCount ? Targets[0]->GetSurface() : DepthTarget->GetSurface();
		Area.Width = float(Sized->GetWidth());
		Area.Height = float(Sized->GetHeight());
	}

	const nri::Viewport Viewport = { Area.TopLeftX, Area.TopLeftY, Area.Width, Area.Height, Area.MinDepth, Area.MaxDepth };
	const nri::Rect Scissor = { s16(Area.TopLeftX), s16(Area.TopLeftY), nri::Dim_t(Area.Width), nri::Dim_t(Area.Height) };
	Api.CmdSetViewports(Commands, &Viewport, 1);
	Api.CmdSetScissors(Commands, &Scissor, 1);

	if (DepthTarget && Cmd.Depth.StencilEnable)
	{
		Api.CmdSetStencilReference(Commands, u8(Cmd.StencilRef), u8(Cmd.StencilRef));
	}

	nri::AttachmentDesc Colors[TargetSlots] = {};

	for (u32 Target = 0; Target < TargetCount; ++Target)
	{
		Colors[Target].descriptor = Wrap(Targets[Target]->GetSurface(), EView::Color);
		Colors[Target].loadOp = nri::LoadOp::LOAD;
		Colors[Target].storeOp = nri::StoreOp::STORE;
	}

	nri::RenderingDesc Rendering = {};
	Rendering.colors = Colors;
	Rendering.colorNum = TargetCount;

	if (DepthTarget)
	{
		Rendering.depth.descriptor = Wrap(DepthTarget->GetSurface(), EView::Depth, DepthFormat);
		Rendering.depth.loadOp = nri::LoadOp::LOAD;
		Rendering.depth.storeOp = nri::StoreOp::STORE;
	}

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
	return true;
}

bool DrawFullscreenNri(GfxCmdList& Cmd, u32 VertexCount)
{
	return RImplementation.NriPasses.Draw(Cmd, VertexCount);
}

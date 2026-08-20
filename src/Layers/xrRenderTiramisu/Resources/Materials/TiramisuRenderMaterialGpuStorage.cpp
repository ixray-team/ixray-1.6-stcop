#include "TiramisuRenderMaterialGpuStorage.h"

#include <MaterialParameterLayout.h>

#include "Resources/TiramisuRenderResourcesManager.h"
#include "Resources/Materials/Proxy/TiramisuMaterialRenderProxy.h"
#include "Resources/Materials/TiramisuRenderMaterialsManager.h"
#include "Resources/Materials/TiramisuRenderMaterialShaderLibrary.h"
#include "Resources/Textures/TiramisuRenderTextureResourceProxy.h"

namespace
{
constexpr u32 ParameterAlignment = MaterialParameterBlockAlignment;

u32 AlignParameterOffset(const u32 Value)
{
	return (Value + ParameterAlignment - 1) & ~(ParameterAlignment - 1);
}

void LogMaterialDiagnostics(const xr_span<const FMaterialDiagnostic> Diagnostics, const char* Stage)
{
	for (const FMaterialDiagnostic& Diagnostic : Diagnostics)
	{
		Msg("! Tiramisu material GPU %s [%s]: %s", Stage, Diagnostic.Code.c_str(), Diagnostic.Message.c_str());
	}
}

void IncludeDirtyRange(
	u32& DirtyBegin,
	u32& DirtyEnd,
	const u32 Offset,
	const u32 Size
)
{
	DirtyBegin = DirtyBegin == INDEX_NONE
		? Offset
		: std::min(DirtyBegin, Offset);
	DirtyEnd = std::max(DirtyEnd, Offset + Size);
}
} // namespace

TiramisuRenderMaterialGpuStorage::TiramisuRenderMaterialGpuStorage()
{
	CheckIsRenderThread();
	MaterialInstances.reserve(MaxMaterialInstances);
	MaterialParameterMirror.resize(MaterialParameterCapacity);

	CreateDrawBuffers_RenderThread();
	CreateBufferedTable_RenderThread(
		u64(MaxMaterialInstances) * MaterialInstanceGpuDataSize,
		MaterialInstanceBuffer,
		MaterialInstanceUploadBuffer,
		MaterialInstanceDescriptor,
		MaterialInstanceBufferIndex
	);
	CreateBufferedTable_RenderThread(
		MaterialParameterCapacity,
		MaterialParameterBuffer,
		MaterialParameterUploadBuffer,
		MaterialParameterDescriptor,
		MaterialParameterBufferIndex
	);
}

TiramisuRenderMaterialGpuStorage::~TiramisuRenderMaterialGpuStorage()
{
	CheckIsRenderThread();
	DestroyDrawBuffers_RenderThread();
	DestroyBufferedTable_RenderThread(
		MaterialInstanceBuffer,
		MaterialInstanceUploadBuffer,
		MaterialInstanceDescriptor,
		MaterialInstanceBufferIndex
	);
	DestroyBufferedTable_RenderThread(
		MaterialParameterBuffer,
		MaterialParameterUploadBuffer,
		MaterialParameterDescriptor,
		MaterialParameterBufferIndex
	);
}

void TiramisuRenderMaterialGpuStorage::CreateBufferedTable_RenderThread(
	const u64 Size,
	nri::Buffer*& Buffer,
	nri::Buffer*& UploadBuffer,
	nri::Descriptor*& Descriptor,
	u32& DescriptorIndex
)
{
	CheckIsRenderThread();

	nri::BufferDesc DeviceDescription = {};
	DeviceDescription.size = Size;
	DeviceDescription.structureStride = sizeof(u32);
	DeviceDescription.usage = nri::BufferUsageBits::SHADER_RESOURCE;
	NRI_CHECK(GRenderDevice.CoreInterface.CreateCommittedBuffer(
		*GRenderDevice.Device,
		nri::MemoryLocation::DEVICE,
		0.5f,
		DeviceDescription,
		Buffer
	));

	nri::BufferDesc UploadDescription = {};
	UploadDescription.size = Size;
	NRI_CHECK(GRenderDevice.CoreInterface.CreateCommittedBuffer(
		*GRenderDevice.Device,
		nri::MemoryLocation::HOST_UPLOAD,
		0.0f,
		UploadDescription,
		UploadBuffer
	));

	nri::BufferViewDesc ViewDescription = {};
	ViewDescription.buffer = Buffer;
	ViewDescription.type = nri::BufferView::BYTE_ADDRESS_BUFFER;
	ViewDescription.offset = 0;
	ViewDescription.size = Size;
	NRI_CHECK(GRenderDevice.CoreInterface.CreateBufferView(ViewDescription, Descriptor));

	DescriptorIndex = GRenderResourcesManager->DescriptorHeapAllocator->Alloc(Descriptor);
}

void TiramisuRenderMaterialGpuStorage::DestroyBufferedTable_RenderThread(
	nri::Buffer*& Buffer,
	nri::Buffer*& UploadBuffer,
	nri::Descriptor*& Descriptor,
	u32& DescriptorIndex
)
{
	CheckIsRenderThread();
	if (DescriptorIndex != INDEX_NONE)
	{
		GRenderResourcesManager->DescriptorHeapAllocator->Free(DescriptorIndex);
		DescriptorIndex = INDEX_NONE;
	}

	if (Descriptor)
	{
		GRenderDevice.CoreInterface.DestroyDescriptor(Descriptor);
		Descriptor = nullptr;
	}

	if (Buffer)
	{
		GRenderDevice.CoreInterface.DestroyBuffer(Buffer);
		Buffer = nullptr;
	}
	if (UploadBuffer)
	{
		GRenderDevice.CoreInterface.DestroyBuffer(UploadBuffer);
		UploadBuffer = nullptr;
	}
}

bool TiramisuRenderMaterialGpuStorage::WriteBuffer_RenderThread(nri::Buffer& Buffer, const u64 Offset, const void* Data, const u64 Size)
{
	CheckIsRenderThread();
	if (!Data || Size == 0)
	{
		return false;
	}
	void* Destination = GRenderDevice.CoreInterface.MapBuffer(Buffer, Offset, Size);
	if (!Destination)
	{
		return false;
	}
	std::memcpy(Destination, Data, static_cast<size_t>(Size));
	GRenderDevice.CoreInterface.UnmapBuffer(Buffer);
	return true;
}

void TiramisuRenderMaterialGpuStorage::BeginFrame_RenderThread(
	const u32 FrameSlot
)
{
	CheckIsRenderThread();
	VERIFY(FrameSlot <
		TiramisuMaterialDrawFrameLayout::BufferedFrameCount);
	DrawBaseIndex =
		TiramisuMaterialDrawFrameLayout::GetAbsoluteDrawIndex(
			FrameSlot, 0
		);
	DrawCount = 0;
}

void TiramisuRenderMaterialGpuStorage::CreateDrawBuffers_RenderThread()
{
	CheckIsRenderThread();

	nri::BufferDesc DeviceDescription = {};
	DeviceDescription.size =
		TiramisuMaterialDrawFrameLayout::BufferSize;
	DeviceDescription.structureStride = sizeof(u32);
	DeviceDescription.usage = nri::BufferUsageBits::SHADER_RESOURCE;
	NRI_CHECK(GRenderDevice.CoreInterface.CreateCommittedBuffer(
		*GRenderDevice.Device,
		nri::MemoryLocation::DEVICE,
		0.5f,
		DeviceDescription,
		DrawDataBuffer
	));

	nri::BufferDesc UploadDescription = {};
	UploadDescription.size = DeviceDescription.size;
	NRI_CHECK(GRenderDevice.CoreInterface.CreateCommittedBuffer(
		*GRenderDevice.Device,
		nri::MemoryLocation::HOST_UPLOAD,
		0.0f,
		UploadDescription,
		DrawDataUploadBuffer
	));

	nri::BufferViewDesc ViewDescription = {};
	ViewDescription.buffer = DrawDataBuffer;
	ViewDescription.type = nri::BufferView::BYTE_ADDRESS_BUFFER;
	ViewDescription.size = DeviceDescription.size;
	NRI_CHECK(GRenderDevice.CoreInterface.CreateBufferView(
		ViewDescription,
		DrawDataDescriptor
	));
	DrawDataBufferIndex =
		GRenderResourcesManager->DescriptorHeapAllocator->Alloc(
			DrawDataDescriptor
		);
}

void TiramisuRenderMaterialGpuStorage::DestroyDrawBuffers_RenderThread()
{
	CheckIsRenderThread();
	if (DrawDataBufferIndex != INDEX_NONE)
	{
		GRenderResourcesManager->DescriptorHeapAllocator->Free(
			DrawDataBufferIndex
		);
		DrawDataBufferIndex = INDEX_NONE;
	}
	if (DrawDataDescriptor)
	{
		GRenderDevice.CoreInterface.DestroyDescriptor(
			DrawDataDescriptor
		);
		DrawDataDescriptor = nullptr;
	}
	if (DrawDataBuffer)
	{
		GRenderDevice.CoreInterface.DestroyBuffer(DrawDataBuffer);
		DrawDataBuffer = nullptr;
	}
	if (DrawDataUploadBuffer)
	{
		GRenderDevice.CoreInterface.DestroyBuffer(
			DrawDataUploadBuffer
		);
		DrawDataUploadBuffer = nullptr;
	}
	DrawDataBufferState = {};
}

u32 TiramisuRenderMaterialGpuStorage::CreateMaterialInstance_RenderThread(const xr_span<const u8> ParameterData, const u64 LayoutHash)
{
	CheckIsRenderThread();
	if (ParameterData.empty() || MaterialInstances.size() >= MaxMaterialInstances)
	{
		return INDEX_NONE;
	}

	const u32 ParameterOffset = AlignParameterOffset(MaterialParameterSize);
	const u32 ParameterSize = static_cast<u32>(ParameterData.size());
	const u32 AlignedParameterSize = AlignParameterOffset(ParameterSize);
	if (ParameterOffset > MaterialParameterCapacity || AlignedParameterSize > MaterialParameterCapacity - ParameterOffset)
	{
		return INDEX_NONE;
	}

	std::memset(MaterialParameterMirror.data() + ParameterOffset, 0, AlignedParameterSize);
	std::memcpy(MaterialParameterMirror.data() + ParameterOffset, ParameterData.data(), ParameterData.size());
	if (!WriteBuffer_RenderThread(
			*MaterialParameterUploadBuffer,
			ParameterOffset,
			MaterialParameterMirror.data() + ParameterOffset,
			AlignedParameterSize
		))
	{
		return INDEX_NONE;
	}
	IncludeDirtyRange(
		MaterialParameterDirtyBegin,
		MaterialParameterDirtyEnd,
		ParameterOffset,
		AlignedParameterSize
	);

	FMaterialInstanceGpuData InstanceData;
	InstanceData.ParameterDataOffset = ParameterOffset;
	InstanceData.ParameterDataSize = AlignedParameterSize;
	InstanceData.LayoutHashLow = static_cast<u32>(LayoutHash);
	InstanceData.LayoutHashHigh = static_cast<u32>(LayoutHash >> 32u);

	const u32 MaterialInstanceIndex = static_cast<u32>(MaterialInstances.size());
	const u32 InstanceOffset =
		MaterialInstanceIndex * sizeof(InstanceData);
	if (!WriteBuffer_RenderThread(
			*MaterialInstanceUploadBuffer,
			InstanceOffset,
			&InstanceData,
			sizeof(InstanceData)
		))
	{
		return INDEX_NONE;
	}
	IncludeDirtyRange(
		MaterialInstanceDirtyBegin,
		MaterialInstanceDirtyEnd,
		InstanceOffset,
		sizeof(InstanceData)
	);

	MaterialInstances.push_back(InstanceData);
	MaterialParameterSize = ParameterOffset + AlignedParameterSize;
	return MaterialInstanceIndex;
}

bool TiramisuRenderMaterialGpuStorage::UpdateMaterialInstance_RenderThread(const u32 MaterialInstanceIndex, const xr_span<const u8> ParameterData)
{
	CheckIsRenderThread();
	if (MaterialInstanceIndex >= MaterialInstances.size())
	{
		return false;
	}
	const auto& InstanceData = MaterialInstances[MaterialInstanceIndex];
	if (ParameterData.size() != InstanceData.ParameterDataSize)
	{
		return false;
	}

	u8* CurrentData = MaterialParameterMirror.data() + InstanceData.ParameterDataOffset;
	if (std::memcmp(CurrentData, ParameterData.data(), ParameterData.size()) == 0)
	{
		return true;
	}
	std::memcpy(CurrentData, ParameterData.data(), ParameterData.size());
	if (!WriteBuffer_RenderThread(
			*MaterialParameterUploadBuffer,
			InstanceData.ParameterDataOffset,
			CurrentData,
			ParameterData.size()
		))
	{
		return false;
	}
	IncludeDirtyRange(
		MaterialParameterDirtyBegin,
		MaterialParameterDirtyEnd,
		InstanceData.ParameterDataOffset,
		static_cast<u32>(ParameterData.size())
	);
	return true;
}

u32 TiramisuRenderMaterialGpuStorage::GetOrCreateMaterial_RenderThread(const TiramisuMaterialRenderProxy& Material)
{
	CheckIsRenderThread();
	const FMaterialAssetId& Asset = Material.GetAssetReference();
	if (!Asset.IsValid())
	{
		return INDEX_NONE;
	}

	FMaterialInstanceCacheKey CacheKey;
	CacheKey.Asset = Asset;
	for (const FMaterialTextureParameterBinding& Binding : Material.GetTextureParameters())
	{
		if (!Binding.Parameter.IsValid() || !Binding.Texture || !Binding.Texture->Descriptor)
		{
			continue;
		}
		CacheKey.TextureParameters.emplace_back(Binding.Parameter, Binding.Texture->GetOrCreateHeapID());
	}
	std::ranges::sort(CacheKey.TextureParameters);

	if (const auto Existing = MaterialInstanceCache.find(CacheKey); Existing != MaterialInstanceCache.end())
	{
		return Existing->second;
	}

	const FResolvedMaterialInstance* Resolved = GRenderResourcesManager->MaterialShaderLibrary->ResolveMaterial_RenderThread(Asset);
	const FMaterialAsset* Master = GRenderResourcesManager->MaterialShaderLibrary->ResolveMaster_RenderThread(Asset);
	if (!Resolved || !Master)
	{
		Resolved = GRenderResourcesManager->MaterialsManager->ResolveSourceMaterial_RenderThread(Asset);
		Master = GRenderResourcesManager->MaterialsManager->ResolveSourceMaster_RenderThread(Asset);
	}
	if (!Resolved || !Master)
	{
		Msg("! Tiramisu material GPU: asset '%s' is absent from materials.xrm and the development source registry.", Asset.Value.c_str());
		return INDEX_NONE;
	}

	const FMaterialParameterLayoutResult Layout = BuildMaterialParameterLayout(Master->Parameters);
	if (!Layout.Succeeded())
	{
		LogMaterialDiagnostics(Layout.Diagnostics, "layout");
		return INDEX_NONE;
	}

	const FMaterialParameterPackResult Packed = PackMaterialParameters(Layout.Value, Master->Parameters, Resolved->Parameters);
	if (!Packed.Succeeded())
	{
		LogMaterialDiagnostics(Packed.Diagnostics, "pack");
		return INDEX_NONE;
	}

	const FMaterialParameterPackResult Patched = PatchMaterialParameterResources
	(
		Packed.Value, 
		[&CacheKey](const FMaterialParameterResourceReference& Reference) -> xr_optional<FDescriptorHeapIndex>
		{
			if (Reference.Type == EMaterialParameterType::SamplerPreset)
			{
				return FDescriptorHeapIndex{0};
			}

			const auto Override = std::ranges::find_if(CacheKey.TextureParameters, [&Reference](const auto& Entry)
													   { return Entry.first == Reference.Parameter; });

			if (Override != CacheKey.TextureParameters.end())
			{
				return FDescriptorHeapIndex{Override->second};
			}

			if (Reference.Type == EMaterialParameterType::Texture2D)
			{
				TiramisuRenderTextureResourceProxy* Fallback = GRenderResourcesManager->WhiteTexture->ResourceProxy;
				if (Fallback && Fallback->Descriptor)
				{
					return FDescriptorHeapIndex{Fallback->GetOrCreateHeapID()};
				}
			}
			return std::nullopt;
		}
	);

	if (!Patched.Succeeded())
	{
		LogMaterialDiagnostics(Patched.Diagnostics, "resource patch");
		return INDEX_NONE;
	}

	const u32 MaterialInstanceIndex = CreateMaterialInstance_RenderThread(Patched.Value.Data, Patched.Value.LayoutHash);
	if (MaterialInstanceIndex != INDEX_NONE)
	{
		MaterialInstanceCache.emplace(std::move(CacheKey), MaterialInstanceIndex);
	}
	return MaterialInstanceIndex;
}

u32 TiramisuRenderMaterialGpuStorage::AddDraw_RenderThread(const FMaterialDrawGpuData& DrawData)
{
	CheckIsRenderThread();
	if (DrawCount >= MaxDrawsPerFrame)
	{
		return INDEX_NONE;
	}
	const u32 DrawIndex = DrawBaseIndex + DrawCount;
	if (!WriteBuffer_RenderThread(
			*DrawDataUploadBuffer,
			u64(DrawIndex) * sizeof(DrawData),
			&DrawData,
			sizeof(DrawData)
		))
	{
		return INDEX_NONE;
	}
	++DrawCount;
	return DrawIndex;
}

void TiramisuRenderMaterialGpuStorage::UploadRange_RenderThread(
	nri::CommandBuffer& CommandBuffer,
	nri::Buffer& Buffer,
	nri::Buffer& UploadBuffer,
	nri::AccessStage& State,
	const u64 Offset,
	const u64 Size,
	const nri::StageBits ShaderStages
)
{
	CheckIsRenderThread();
	if (Size == 0)
	{
		return;
	}

	nri::BufferBarrierDesc Barrier = {};
	Barrier.buffer = &Buffer;
	Barrier.before = State;
	Barrier.after = {
		nri::AccessBits::COPY_DESTINATION,
		nri::StageBits::COPY
	};
	nri::BarrierDesc BarrierDescription = {};
	BarrierDescription.buffers = &Barrier;
	BarrierDescription.bufferNum = 1;
	GRenderDevice.CoreInterface.CmdBarrier(
		CommandBuffer,
		BarrierDescription
	);

	GRenderDevice.CoreInterface.CmdCopyBuffer(
		CommandBuffer,
		Buffer,
		Offset,
		UploadBuffer,
		Offset,
		Size
	);

	Barrier.before = Barrier.after;
	Barrier.after = {
		nri::AccessBits::SHADER_RESOURCE,
		ShaderStages
	};
	GRenderDevice.CoreInterface.CmdBarrier(
		CommandBuffer,
		BarrierDescription
	);
	State = Barrier.after;
}

void TiramisuRenderMaterialGpuStorage::Upload_RenderThread(
	nri::CommandBuffer& CommandBuffer
)
{
	CheckIsRenderThread();
	if (DrawCount != 0)
	{
		UploadRange_RenderThread(
			CommandBuffer,
			*DrawDataBuffer,
			*DrawDataUploadBuffer,
			DrawDataBufferState,
			u64(DrawBaseIndex) * MaterialDrawGpuDataSize,
			u64(DrawCount) * MaterialDrawGpuDataSize,
			nri::StageBits::VERTEX_SHADER
		);
	}

	const nri::StageBits MaterialShaderStages =
		nri::StageBits::VERTEX_SHADER |
		nri::StageBits::FRAGMENT_SHADER;
	if (MaterialInstanceDirtyBegin != INDEX_NONE)
	{
		UploadRange_RenderThread(
			CommandBuffer,
			*MaterialInstanceBuffer,
			*MaterialInstanceUploadBuffer,
			MaterialInstanceBufferState,
			MaterialInstanceDirtyBegin,
			MaterialInstanceDirtyEnd - MaterialInstanceDirtyBegin,
			MaterialShaderStages
		);
		MaterialInstanceDirtyBegin = INDEX_NONE;
		MaterialInstanceDirtyEnd = 0;
	}
	if (MaterialParameterDirtyBegin != INDEX_NONE)
	{
		UploadRange_RenderThread(
			CommandBuffer,
			*MaterialParameterBuffer,
			*MaterialParameterUploadBuffer,
			MaterialParameterBufferState,
			MaterialParameterDirtyBegin,
			MaterialParameterDirtyEnd - MaterialParameterDirtyBegin,
			MaterialShaderStages
		);
		MaterialParameterDirtyBegin = INDEX_NONE;
		MaterialParameterDirtyEnd = 0;
	}
}

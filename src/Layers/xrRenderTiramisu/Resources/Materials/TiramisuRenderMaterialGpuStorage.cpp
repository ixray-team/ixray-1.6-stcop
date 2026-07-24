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
} // namespace

TiramisuRenderMaterialGpuStorage::TiramisuRenderMaterialGpuStorage()
{
	CheckIsRenderThread();
	MaterialInstances.reserve(MaxMaterialInstances);
	MaterialParameterMirror.resize(MaterialParameterCapacity);

	CreateBuffer_RenderThread(u64(MaxDrawsPerFrame) * MaterialDrawGpuDataSize, DrawDataBuffer, DrawDataDescriptor, DrawDataBufferIndex);
	CreateBuffer_RenderThread(u64(MaxMaterialInstances) * MaterialInstanceGpuDataSize, MaterialInstanceBuffer, MaterialInstanceDescriptor, MaterialInstanceBufferIndex);
	CreateBuffer_RenderThread(MaterialParameterCapacity, MaterialParameterBuffer, MaterialParameterDescriptor, MaterialParameterBufferIndex);
}

TiramisuRenderMaterialGpuStorage::~TiramisuRenderMaterialGpuStorage()
{
	CheckIsRenderThread();
	DestroyBuffer_RenderThread(DrawDataBuffer, DrawDataDescriptor, DrawDataBufferIndex);
	DestroyBuffer_RenderThread(MaterialInstanceBuffer, MaterialInstanceDescriptor, MaterialInstanceBufferIndex);
	DestroyBuffer_RenderThread(MaterialParameterBuffer, MaterialParameterDescriptor, MaterialParameterBufferIndex);
}

void TiramisuRenderMaterialGpuStorage::CreateBuffer_RenderThread(const u64 Size, nri::Buffer*& Buffer, nri::Descriptor*& Descriptor, u32& DescriptorIndex)
{
	CheckIsRenderThread();

	nri::BufferDesc BufferDescription = {};
	BufferDescription.size = Size;
	BufferDescription.structureStride = sizeof(u32);
	BufferDescription.usage = nri::BufferUsageBits::SHADER_RESOURCE;
	NRI_CHECK(GRenderDevice.CoreInterface.CreateCommittedBuffer(*GRenderDevice.Device, nri::MemoryLocation::DEVICE_UPLOAD, 0.5f, BufferDescription, Buffer));

	nri::BufferViewDesc ViewDescription = {};
	ViewDescription.buffer = Buffer;
	ViewDescription.type = nri::BufferView::BYTE_ADDRESS_BUFFER;
	ViewDescription.offset = 0;
	ViewDescription.size = Size;
	NRI_CHECK(GRenderDevice.CoreInterface.CreateBufferView(ViewDescription, Descriptor));

	DescriptorIndex = GRenderResourcesManager->DescriptorHeapAllocator->Alloc(Descriptor);
}

void TiramisuRenderMaterialGpuStorage::DestroyBuffer_RenderThread(nri::Buffer*& Buffer, nri::Descriptor*& Descriptor, u32& DescriptorIndex)
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

void TiramisuRenderMaterialGpuStorage::BeginFrame_RenderThread()
{
	CheckIsRenderThread();
	DrawCount = 0;
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
	if (!WriteBuffer_RenderThread(*MaterialParameterBuffer, ParameterOffset, MaterialParameterMirror.data() + ParameterOffset, AlignedParameterSize))
	{
		return INDEX_NONE;
	}

	FMaterialInstanceGpuData InstanceData;
	InstanceData.ParameterDataOffset = ParameterOffset;
	InstanceData.ParameterDataSize = AlignedParameterSize;
	InstanceData.LayoutHashLow = static_cast<u32>(LayoutHash);
	InstanceData.LayoutHashHigh = static_cast<u32>(LayoutHash >> 32u);

	const u32 MaterialInstanceIndex = static_cast<u32>(MaterialInstances.size());
	if (!WriteBuffer_RenderThread(*MaterialInstanceBuffer, u64(MaterialInstanceIndex) * sizeof(InstanceData), &InstanceData, sizeof(InstanceData)))
	{
		return INDEX_NONE;
	}

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
	return WriteBuffer_RenderThread(*MaterialParameterBuffer, InstanceData.ParameterDataOffset, CurrentData, ParameterData.size());
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
	const u32 DrawIndex = DrawCount;
	if (!WriteBuffer_RenderThread(*DrawDataBuffer, u64(DrawIndex) * sizeof(DrawData), &DrawData, sizeof(DrawData)))
	{
		return INDEX_NONE;
	}
	++DrawCount;
	return DrawIndex;
}
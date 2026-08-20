#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuMaterialDrawFrameLayout.h"

#include <MaterialRuntime.h>

#include <span>
#include <utility>
#include <vector>

class TiramisuRenderTextureResourceProxy;
class TiramisuMaterialRenderProxy;

// Управляет индексируемыми GPU-буферами параметров material instances.
class TiramisuRenderMaterialGpuStorage
{
public:
	static constexpr u32 MaxMaterialInstances = 4096;
	static constexpr u32 MaxDrawsPerFrame =
		TiramisuMaterialDrawFrameLayout::MaxDrawsPerFrame;
	static constexpr u32 MaterialParameterCapacity = 4 * 1024 * 1024;

	TiramisuRenderMaterialGpuStorage();
	~TiramisuRenderMaterialGpuStorage();

	TiramisuRenderMaterialGpuStorage(const TiramisuRenderMaterialGpuStorage&) = delete;
	TiramisuRenderMaterialGpuStorage& operator=(const TiramisuRenderMaterialGpuStorage&) = delete;

	// Сбрасывает per-frame draw allocation после ожидания соответствующего frame fence.
	void BeginFrame_RenderThread(u32 FrameSlot);

	// Выделяет и обновляет packed parameter blocks; indices остаются стабильны для draw data.
	[[nodiscard]] u32 CreateMaterialInstance_RenderThread(
		xr_span<const u8> ParameterData, u64 LayoutHash
	);
	[[nodiscard]] bool UpdateMaterialInstance_RenderThread(
		u32 MaterialInstanceIndex, xr_span<const u8> ParameterData
	);
	[[nodiscard]] u32 GetOrCreateMaterial_RenderThread(
		const TiramisuMaterialRenderProxy& Material
	);
	// Добавляет transform/object/material indices одного draw в индексируемый GPU buffer.
	[[nodiscard]] u32 AddDraw_RenderThread(
		const FMaterialDrawGpuData& DrawData
	);
	// Копирует подготовленный frame-local region в device-local shader buffer.
	void Upload_RenderThread(nri::CommandBuffer& CommandBuffer);

	[[nodiscard]] u32 GetDrawDataBufferIndex() const noexcept { return DrawDataBufferIndex; }
	[[nodiscard]] u32 GetMaterialInstanceBufferIndex() const noexcept { return MaterialInstanceBufferIndex; }
	[[nodiscard]] u32 GetMaterialParameterBufferIndex() const noexcept { return MaterialParameterBufferIndex; }

private:
	void CreateDrawBuffers_RenderThread();
	void DestroyDrawBuffers_RenderThread();
	void CreateBufferedTable_RenderThread(
		u64 Size,
		nri::Buffer*& Buffer,
		nri::Buffer*& UploadBuffer,
		nri::Descriptor*& Descriptor,
		u32& DescriptorIndex
	);
	void DestroyBufferedTable_RenderThread(
		nri::Buffer*& Buffer,
		nri::Buffer*& UploadBuffer,
		nri::Descriptor*& Descriptor,
		u32& DescriptorIndex
	);
	void UploadRange_RenderThread(
		nri::CommandBuffer& CommandBuffer,
		nri::Buffer& Buffer,
		nri::Buffer& UploadBuffer,
		nri::AccessStage& State,
		u64 Offset,
		u64 Size,
		nri::StageBits ShaderStages
	);
	[[nodiscard]] bool WriteBuffer_RenderThread(
		nri::Buffer& Buffer, u64 Offset, const void* Data, u64 Size
	);

	nri::Buffer* DrawDataBuffer = nullptr;
	nri::Buffer* DrawDataUploadBuffer = nullptr;
	nri::Buffer* MaterialInstanceBuffer = nullptr;
	nri::Buffer* MaterialInstanceUploadBuffer = nullptr;
	nri::Buffer* MaterialParameterBuffer = nullptr;
	nri::Buffer* MaterialParameterUploadBuffer = nullptr;
	nri::Descriptor* DrawDataDescriptor = nullptr;
	nri::Descriptor* MaterialInstanceDescriptor = nullptr;
	nri::Descriptor* MaterialParameterDescriptor = nullptr;
	u32 DrawDataBufferIndex = INDEX_NONE;
	u32 MaterialInstanceBufferIndex = INDEX_NONE;
	u32 MaterialParameterBufferIndex = INDEX_NONE;

	xr_vector<FMaterialInstanceGpuData> MaterialInstances;
	xr_vector<u8> MaterialParameterMirror;
	// Ключ дедупликации material instance в GPU storage.
	struct FMaterialInstanceCacheKey
	{
		FMaterialAssetId Asset;
		xr_vector<xr_pair<FMaterialParameterId, u32>>
			TextureParameters;

		auto operator<=>(const FMaterialInstanceCacheKey&) const = default;
	};

	xr_map<FMaterialInstanceCacheKey, u32> MaterialInstanceCache;
	u32 MaterialParameterSize = 0;
	u32 DrawBaseIndex = 0;
	u32 DrawCount = 0;
	nri::AccessStage DrawDataBufferState = {};
	nri::AccessStage MaterialInstanceBufferState = {};
	nri::AccessStage MaterialParameterBufferState = {};
	u32 MaterialInstanceDirtyBegin = INDEX_NONE;
	u32 MaterialInstanceDirtyEnd = 0;
	u32 MaterialParameterDirtyBegin = INDEX_NONE;
	u32 MaterialParameterDirtyEnd = 0;
};

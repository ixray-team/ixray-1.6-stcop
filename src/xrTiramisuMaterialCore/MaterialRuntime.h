#pragma once

#include "MaterialTypes.h"

#include <array>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>

// Renderer-owned index into ResourceDescriptorHeap/SamplerDescriptorHeap.
// xrTiramisuMaterialCore carries the ABI value but never creates NRI descriptors.
struct FDescriptorHeapIndex
{
    static constexpr u32 Invalid = std::numeric_limits<u32>::max();

    u32 Value = Invalid;

    [[nodiscard]] bool IsValid() const noexcept { return Value != Invalid; }
    auto operator<=>(const FDescriptorHeapIndex&) const = default;
};

inline constexpr u32 MaterialGpuAbiVersion = 2;
inline constexpr u32 MaterialInstanceGpuDataSize = 16;
inline constexpr u32 MaterialDrawGpuDataSize = 144;
inline constexpr u32 MaterialLightGpuDataSize = 64;

using FMaterialGpuMatrix = xr_array<float, 16>;

// CPU matrices X-Ray используют row vectors: [position, 1] * Matrix. Material draw
// ByteAddressBuffer восстанавливается как явные HLSL rows и применяется
// через mul(Matrix, position), поэтому transpose выполняется только на этой ABI-границе.
// Constant/root buffers используют DXC column-major packing и не должны вызывать
// этот helper.
// Преобразует row-vector matrix X-Ray в ABI draw buffer без двойного transpose.
[[nodiscard]] constexpr FMaterialGpuMatrix MakeMaterialDrawBufferMatrix(
    const FMaterialGpuMatrix& XRayRowVectorMatrix) noexcept
{
    FMaterialGpuMatrix Result = {};
    for (size_t Row = 0; Row < 4; ++Row)
    {
        for (size_t Column = 0; Column < 4; ++Column)
            Result[Row * 4 + Column] =
                XRayRowVectorMatrix[Column * 4 + Row];
    }
    return Result;
}

// GPU ABI записи material instance в индексируемом structured buffer.
struct alignas(16) FMaterialInstanceGpuData
{
    u32 ParameterDataOffset = 0;
    u32 ParameterDataSize = 0;
    u32 LayoutHashLow = 0;
    u32 LayoutHashHigh = 0;

    auto operator<=>(const FMaterialInstanceGpuData&) const = default;
};

// GPU ABI per-draw записи, выбираемой через NRI_BASE_INSTANCE.
struct alignas(16) FMaterialDrawGpuData
{
    FMaterialGpuMatrix LocalToWorld{};
    FMaterialGpuMatrix PreviousLocalToWorld{};
    u32 MaterialInstanceIndex = 0;
    u32 ObjectId = 0;
    u32 Flags = 0;
    u32 Padding = 0;

    auto operator<=>(const FMaterialDrawGpuData&) const = default;
};

static_assert(sizeof(FMaterialInstanceGpuData) == MaterialInstanceGpuDataSize);
static_assert(sizeof(FMaterialDrawGpuData) == MaterialDrawGpuDataSize);

enum class EMaterialLightType : u32
{
    Directional,
    Point,
    Spot
};

enum class EMaterialLightFlags : u32
{
    None = 0,
    CastShadows = 1u << 0,
    Selected = 1u << 1
};

// GPU ABI runtime light, общий для editor preview и будущих clustered lists.
struct alignas(16) FMaterialLightGpuData
{
    xr_array<float, 3> Position = {};
    float Range = 0.0f;
    xr_array<float, 3> Direction = {0.0f, 0.0f, 1.0f};
    u32 Type =
        static_cast<u32>(EMaterialLightType::Point);
    xr_array<float, 3> Color = {1.0f, 1.0f, 1.0f};
    float Intensity = 1.0f;
    float CosInnerCone = 1.0f;
    float CosOuterCone = 0.0f;
    u32 Flags = 0;
    u32 Padding = 0;

    auto operator<=>(const FMaterialLightGpuData&) const = default;
};

static_assert(sizeof(FMaterialLightGpuData) == MaterialLightGpuDataSize);

// Связь material parameter GUID с bindless texture/sampler indices.
struct FMaterialTextureBinding
{
    xr_string AssetPath;
    FDescriptorHeapIndex ResourceIndex;
};

// Полный набор offsets и descriptors, необходимый material pass.
struct FMaterialGpuBinding
{
    u32 MaterialInstanceIndex = 0;
    FDescriptorHeapIndex SamplerIndex;
};

enum class EMaterialPass : u8
{
    Depth,
    Shadow,
    GBuffer,
    Forward,
    UI,
    PostProcess,
    Validation
};

// Generation-counted handle опубликованного material pipeline.
struct FMaterialPipelineHandle
{
    u32 Index = UINT32_MAX;
    u32 Generation = 0;

    [[nodiscard]] bool IsValid() const noexcept { return Index != UINT32_MAX && Generation != 0; }
    auto operator<=>(const FMaterialPipelineHandle&) const = default;
};

// Renderer-ready immutable snapshot material pass для одного pipeline key.
struct FMaterialPassProxy
{
    EMaterialPass Pass = EMaterialPass::GBuffer;
    u64 PipelineKey = 0;
    u32 MaterialInstanceIndex = 0;
    FMaterialPipelineHandle Pipeline;
    xr_string VertexFactory = "level_static";
    u64 Revision = 0;

    [[nodiscard]] bool IsValid() const noexcept
    {
        return PipelineKey != 0 && Pipeline.IsValid() && !VertexFactory.empty() && Revision != 0;
    }
};


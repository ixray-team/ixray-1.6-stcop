#pragma once

#include "TiramisuRenderTypes.h"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>

enum class ETiramisuGraphicsApi : u8
{
    Vulkan = 1u << 0u,
    D3D12 = 1u << 1u
};

enum class ETiramisuAdapterKind : u8
{
    Unknown,
    Software,
    Virtual,
    Integrated,
    Discrete
};

// Нормализованное описание GPU adapter для детерминированного выбора устройства.
struct FTiramisuAdapterCandidate
{
    u8 SupportedApis = 0;
    ETiramisuAdapterKind Kind = ETiramisuAdapterKind::Unknown;
    u32 GraphicsQueueCount = 0;
    u64 DedicatedVideoMemory = 0;
    u64 SharedSystemMemory = 0;
    u32 ComputeQueueCount = 0;
    u32 CopyQueueCount = 0;
};

[[nodiscard]] xr_optional<size_t> SelectBestTiramisuAdapter(
    xr_span<const FTiramisuAdapterCandidate> Candidates,
    ETiramisuGraphicsApi RequiredApi) noexcept;

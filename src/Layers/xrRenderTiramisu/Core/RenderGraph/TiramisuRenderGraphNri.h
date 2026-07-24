#pragma once

#include "TiramisuRenderTypes.h"

#include "TiramisuRenderGraph.h"

#include <NRI.h>

#include <optional>
#include <span>
#include <string>
#include <vector>

namespace Tiramisu::RenderGraph
{
// Каноническое NRI-состояние одного логического access render graph.
struct FNriAccessState
{
    nri::AccessBits Access = nri::AccessBits::NONE;
    nri::Layout TextureLayout = nri::Layout::UNDEFINED;
    nri::StageBits Stages = nri::StageBits::NONE;
};

// Возвращает nullopt, если access нельзя выполнить в выбранной queue.
// Централизованный перевод не даёт Vulkan и D3D12 расходиться в access/layout/stage.
[[nodiscard]] xr_optional<FNriAccessState> ToNriAccessState(
    EAccess Access, EQueue Queue) noexcept;

[[nodiscard]] nri::AccessLayoutStage ToNriTextureState(
    const FNriAccessState& State) noexcept;
[[nodiscard]] nri::AccessStage ToNriBufferState(
    const FNriAccessState& State) noexcept;

// Связывает логический handle графа с реальным NRI texture или buffer.
struct FNriResourceBinding
{
    FResourceHandle Resource;
    nri::Texture* Texture = nullptr;
    nri::Buffer* Buffer = nullptr;
    nri::Dim_t MipOffset = 0;
    nri::Dim_t MipNum = nri::REMAINING;
    nri::Dim_t LayerOffset = 0;
    nri::Dim_t LayerNum = nri::REMAINING;
    nri::PlaneBits Planes = nri::PlaneBits::ALL;
    // NRI textures по умолчанию используют CONCURRENT sharing. Флаг нужен только
    // для textures, явно созданных с SharingMode::EXCLUSIVE.
    bool ExclusiveQueueOwnership = false;
};

// Набор физических GPU queues, доступных executor.
struct FNriQueueBindings
{
    nri::Queue* Graphics = nullptr;
    nri::Queue* Compute = nullptr;
    nri::Queue* Copy = nullptr;

    [[nodiscard]] nri::Queue* Get(EQueue Queue) const noexcept;
};

// Результат перевода одного submission в NRI barriers.
struct FNriBarrierBatch
{
    xr_vector<nri::TextureBarrierDesc> TextureBarriers;
    xr_vector<nri::BufferBarrierDesc> BufferBarriers;
    xr_vector<xr_string> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept
    {
        return Diagnostics.empty();
    }
};

// Переводит submission в NRI barriers. Queue waits/signals остаются
// в FQueueSubmission и обрабатываются executor.
[[nodiscard]] FNriBarrierBatch BuildNriBarrierBatch(
    const FCompiledGraph& Graph,
    const FQueueSubmission& Submission,
    xr_span<const FNriResourceBinding> Bindings,
    const FNriQueueBindings& Queues);
} // namespace Tiramisu::RenderGraph

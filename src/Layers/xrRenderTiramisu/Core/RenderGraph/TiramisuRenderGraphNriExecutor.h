#pragma once

#include "TiramisuRenderTypes.h"

#include "TiramisuRenderGraphNri.h"

#include <array>
#include <cstdint>
#include <functional>
#include <span>
#include <string>
#include <vector>

namespace Tiramisu::RenderGraph
{
// Callback записи команд конкретного pass в предоставленный command buffer.
struct FNriPassCallback
{
    FPassHandle Pass;
    std::function<void(nri::CommandBuffer&, const FCompiledPass&)> Execute;
};

// Внешние waits/signals для связи графа со swapchain и соседними системами.
struct FNriExternalQueueSync
{
    EQueue Queue = EQueue::Graphics;
    xr_span<const nri::FenceSubmitDesc> InitialWaits;
    xr_span<const nri::FenceSubmitDesc> FinalSignals;
};

// Статистика исполнения и полный список runtime-диагностик.
struct FNriGraphExecuteResult
{
    xr_vector<xr_string> Diagnostics;
    u32 RecordedPassCount = 0;
    u32 SubmittedPassCount = 0;

    [[nodiscard]] bool Succeeded() const noexcept
    {
        return Diagnostics.empty();
    }
};

// Исполняет скомпилированный render graph через NRI на Vulkan или D3D12.
class TiramisuNriRenderGraphExecutor
{
public:
    TiramisuNriRenderGraphExecutor() = default;
    ~TiramisuNriRenderGraphExecutor();

    TiramisuNriRenderGraphExecutor(const TiramisuNriRenderGraphExecutor&) = delete;
    TiramisuNriRenderGraphExecutor& operator=(const TiramisuNriRenderGraphExecutor&) = delete;

    // Создаёт циклические frame contexts и timeline fences для GPU queues.
    [[nodiscard]] bool Initialize(nri::Device& Device,
        const nri::CoreInterface& CoreInterface,
        const FNriQueueBindings& Queues,
        u32 QueuedFrameCount,
        xr_string& Diagnostic);
    void Destroy();

    // Записывает callbacks/barriers и отправляет submissions с вычисленной синхронизацией.
    [[nodiscard]] FNriGraphExecuteResult Execute(
        u64 FrameNumber,
        const FCompiledGraph& Graph,
        xr_span<const FNriResourceBinding> ResourceBindings,
        xr_span<const FNriPassCallback> PassCallbacks,
        nri::DescriptorPool* DescriptorPool = nullptr,
        xr_span<const FNriExternalQueueSync> ExternalSync = {});

    [[nodiscard]] bool IsInitialized() const noexcept { return Device != nullptr; }
    [[nodiscard]] u32 GetQueuedFrameCount() const noexcept
    {
        return static_cast<u32>(Frames.size());
    }

private:
    static constexpr size_t QueueCount = 3;

    // Переиспользуемые allocator и command buffers одной queue в одном кадре.
    struct FQueueFrameContext
    {
        nri::CommandAllocator* Allocator = nullptr;
        xr_vector<nri::CommandBuffer*> CommandBuffers;
        size_t UsedCommandBufferCount = 0;
        u64 LastFenceValue = 0;
    };

    // Полный набор queue-local ресурсов одного кадра.
    struct FFrameContext
    {
        xr_array<FQueueFrameContext, QueueCount> Queues;
    };

    // Fence/value, опубликованные submission конкретного pass.
    struct FPassSignal
    {
        nri::Fence* Fence = nullptr;
        u64 Value = 0;
    };

    [[nodiscard]] static size_t QueueIndex(EQueue Queue) noexcept;
    [[nodiscard]] nri::CommandBuffer* AcquireCommandBuffer(
        FQueueFrameContext& Context, xr_string& Diagnostic);

    nri::Device* Device = nullptr;
    nri::CoreInterface Core = {};
    FNriQueueBindings QueueBindings;
    xr_array<nri::Fence*, QueueCount> QueueFences = {};
    xr_array<u64, QueueCount> NextFenceValues = {};
    xr_vector<FFrameContext> Frames;
};
} // namespace Tiramisu::RenderGraph

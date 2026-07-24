#include "TiramisuRenderGraphNriExecutor.h"

#include <algorithm>
#include <optional>
#include <utility>

namespace Tiramisu::RenderGraph
{
namespace
{
const FCompiledPass* FindPass(const FCompiledGraph& Graph,
    const FPassHandle Handle)
{
    const auto It = std::find_if(Graph.Passes.begin(), Graph.Passes.end(),
        [Handle](const FCompiledPass& Pass)
        {
            return Pass.Handle == Handle;
        });
    return It == Graph.Passes.end() ? nullptr : &*It;
}

const FNriPassCallback* FindCallback(
    const xr_span<const FNriPassCallback> Callbacks,
    const FPassHandle Handle)
{
    const auto It = std::find_if(Callbacks.begin(), Callbacks.end(),
        [Handle](const FNriPassCallback& Callback)
        {
            return Callback.Pass == Handle;
        });
    return It == Callbacks.end() ? nullptr : &*It;
}

const FNriResourceBinding* FindBinding(
    const xr_span<const FNriResourceBinding> Bindings,
    const FResourceHandle Handle)
{
    const auto It = std::find_if(Bindings.begin(), Bindings.end(),
        [Handle](const FNriResourceBinding& Binding)
        {
            return Binding.Resource == Handle;
        });
    return It == Bindings.end() ? nullptr : &*It;
}
} // namespace

TiramisuNriRenderGraphExecutor::~TiramisuNriRenderGraphExecutor()
{
    Destroy();
}

size_t TiramisuNriRenderGraphExecutor::QueueIndex(const EQueue Queue) noexcept
{
    return static_cast<size_t>(Queue);
}

bool TiramisuNriRenderGraphExecutor::Initialize(nri::Device& InDevice,
    const nri::CoreInterface& CoreInterface,
    const FNriQueueBindings& Queues,
    const u32 QueuedFrameCount,
    xr_string& Diagnostic)
{
    if (IsInitialized())
    {
        Diagnostic = "NRI render-graph executor is already initialized.";
        return false;
    }
    if (QueuedFrameCount == 0 || !Queues.Graphics)
    {
        Diagnostic = "NRI render-graph executor requires frame contexts and a graphics queue.";
        return false;
    }
    if (!CoreInterface.CreateCommandAllocator ||
        !CoreInterface.CreateCommandBuffer || !CoreInterface.CreateFence ||
        !CoreInterface.DestroyCommandAllocator ||
        !CoreInterface.DestroyCommandBuffer || !CoreInterface.DestroyFence ||
        !CoreInterface.BeginCommandBuffer || !CoreInterface.EndCommandBuffer ||
        !CoreInterface.CmdBarrier || !CoreInterface.CmdBeginAnnotation ||
        !CoreInterface.CmdEndAnnotation || !CoreInterface.QueueSubmit ||
        !CoreInterface.QueueWaitIdle || !CoreInterface.Wait ||
        !CoreInterface.ResetCommandAllocator)
    {
        Diagnostic = "NRI render-graph executor received an incomplete core interface.";
        return false;
    }

    Device = &InDevice;
    Core = CoreInterface;
    QueueBindings = Queues;
    Frames.resize(QueuedFrameCount);

    for (size_t Queue = 0; Queue < QueueCount; ++Queue)
    {
        nri::Queue* NriQueue = QueueBindings.Get(static_cast<EQueue>(Queue));
        if (!NriQueue)
            continue;
        if (Core.CreateFence(*Device, 0, QueueFences[Queue]) != nri::Result::SUCCESS)
        {
            Diagnostic = "Failed to create a render-graph queue fence.";
            Destroy();
            return false;
        }
        for (FFrameContext& Frame : Frames)
        {
            if (Core.CreateCommandAllocator(*NriQueue,
                    Frame.Queues[Queue].Allocator) != nri::Result::SUCCESS)
            {
                Diagnostic = "Failed to create a render-graph command allocator.";
                Destroy();
                return false;
            }
        }
    }
    return true;
}

void TiramisuNriRenderGraphExecutor::Destroy()
{
    if (!Device)
        return;

    for (size_t Queue = 0; Queue < QueueCount; ++Queue)
    {
        nri::Queue* NriQueue = QueueBindings.Get(static_cast<EQueue>(Queue));
        if (NriQueue)
            Core.QueueWaitIdle(NriQueue);
    }

    for (FFrameContext& Frame : Frames)
    {
        for (FQueueFrameContext& Queue : Frame.Queues)
        {
            for (nri::CommandBuffer* CommandBuffer : Queue.CommandBuffers)
                Core.DestroyCommandBuffer(CommandBuffer);
            Queue.CommandBuffers.clear();
            if (Queue.Allocator)
                Core.DestroyCommandAllocator(Queue.Allocator);
            Queue = {};
        }
    }
    Frames.clear();

    for (nri::Fence*& Fence : QueueFences)
    {
        if (Fence)
            Core.DestroyFence(Fence);
        Fence = nullptr;
    }
    NextFenceValues = {};
    QueueBindings = {};
    Core = {};
    Device = nullptr;
}

nri::CommandBuffer* TiramisuNriRenderGraphExecutor::AcquireCommandBuffer(
    FQueueFrameContext& Context, xr_string& Diagnostic)
{
    if (Context.UsedCommandBufferCount == Context.CommandBuffers.size())
    {
        nri::CommandBuffer* CommandBuffer = nullptr;
        if (Core.CreateCommandBuffer(*Context.Allocator, CommandBuffer) !=
            nri::Result::SUCCESS)
        {
            Diagnostic = "Failed to create a render-graph command buffer.";
            return nullptr;
        }
        Context.CommandBuffers.push_back(CommandBuffer);
    }
    return Context.CommandBuffers[Context.UsedCommandBufferCount++];
}

FNriGraphExecuteResult TiramisuNriRenderGraphExecutor::Execute(
    const u64 FrameNumber,
    const FCompiledGraph& Graph,
    const xr_span<const FNriResourceBinding> ResourceBindings,
    const xr_span<const FNriPassCallback> PassCallbacks,
    nri::DescriptorPool* DescriptorPool,
    const xr_span<const FNriExternalQueueSync> ExternalSync)
{
    FNriGraphExecuteResult Result;
    if (!IsInitialized())
    {
        Result.Diagnostics.push_back("NRI render-graph executor is not initialized.");
        return Result;
    }
    if (Graph.Passes.empty() || Graph.Submissions.size() != Graph.Passes.size())
    {
        Result.Diagnostics.push_back("Compiled render graph has an invalid submission plan.");
        return Result;
    }

    xr_array<xr_optional<size_t>, QueueCount> FirstSubmission;
    xr_array<xr_optional<size_t>, QueueCount> LastSubmission;
    xr_vector<FNriBarrierBatch> BarrierBatches;
    BarrierBatches.reserve(Graph.Submissions.size());
    for (size_t Index = 0; Index < Graph.Submissions.size(); ++Index)
    {
        const FQueueSubmission& Submission = Graph.Submissions[Index];
        const size_t Queue = QueueIndex(Submission.Queue);
        if (!QueueBindings.Get(Submission.Queue))
        {
            Result.Diagnostics.push_back("Render graph requests an unavailable " +
                xr_string(ToString(Submission.Queue)) + " queue.");
            continue;
        }
        const FCompiledPass* Pass = FindPass(Graph, Submission.Pass);
        const FNriPassCallback* Callback = FindCallback(PassCallbacks, Submission.Pass);
        if (!Pass || !Callback || !Callback->Execute)
        {
            Result.Diagnostics.push_back("Render graph pass has no executable callback.");
            continue;
        }
        if (Submission.Pass.Index >= Graph.Passes.size())
        {
            Result.Diagnostics.push_back("Render-graph pass handle is out of range.");
            continue;
        }
        for (const FBarrier& Barrier : Submission.Barriers)
        {
            const FNriResourceBinding* Binding =
                FindBinding(ResourceBindings, Barrier.Resource);
            if (Barrier.QueueTransfer && Binding && Binding->Texture &&
                Binding->ExclusiveQueueOwnership)
            {
                Result.Diagnostics.push_back(
                    "Exclusive NRI texture ownership transfer requires paired "
                    "release/acquire barriers and is not executable yet.");
            }
        }
        if (!FirstSubmission[Queue]) FirstSubmission[Queue] = Index;
        LastSubmission[Queue] = Index;

        BarrierBatches.push_back(BuildNriBarrierBatch(Graph, Submission,
            ResourceBindings, QueueBindings));
        for (const xr_string& Diagnostic : BarrierBatches.back().Diagnostics)
            Result.Diagnostics.push_back(Diagnostic);
    }
    if (!Result.Diagnostics.empty())
        return Result;

    FFrameContext& Frame = Frames[FrameNumber % Frames.size()];
    for (size_t Queue = 0; Queue < QueueCount; ++Queue)
    {
        FQueueFrameContext& Context = Frame.Queues[Queue];
        if (!Context.Allocator)
            continue;
        if (Context.LastFenceValue != 0)
            Core.Wait(*QueueFences[Queue], Context.LastFenceValue);
        Core.ResetCommandAllocator(*Context.Allocator);
        Context.UsedCommandBufferCount = 0;
        Context.LastFenceValue = 0;
    }

    xr_vector<xr_optional<FPassSignal>> PassSignals(Graph.Passes.size());
    for (size_t Index = 0; Index < Graph.Submissions.size(); ++Index)
    {
        const FQueueSubmission& Submission = Graph.Submissions[Index];
        const size_t Queue = QueueIndex(Submission.Queue);
        FQueueFrameContext& Context = Frame.Queues[Queue];
        xr_string Diagnostic;
        nri::CommandBuffer* CommandBuffer = AcquireCommandBuffer(Context, Diagnostic);
        if (!CommandBuffer)
        {
            Result.Diagnostics.push_back(std::move(Diagnostic));
            return Result;
        }
        if (Core.BeginCommandBuffer(*CommandBuffer, DescriptorPool) != nri::Result::SUCCESS)
        {
            Result.Diagnostics.push_back("Failed to begin a render-graph command buffer.");
            return Result;
        }

        const FNriBarrierBatch& Barriers = BarrierBatches[Index];
        if (!Barriers.TextureBarriers.empty() || !Barriers.BufferBarriers.empty())
        {
            nri::BarrierDesc BarrierDesc = {};
            BarrierDesc.textures = Barriers.TextureBarriers.data();
            BarrierDesc.textureNum = static_cast<u32>(Barriers.TextureBarriers.size());
            BarrierDesc.buffers = Barriers.BufferBarriers.data();
            BarrierDesc.bufferNum = static_cast<u32>(Barriers.BufferBarriers.size());
            Core.CmdBarrier(*CommandBuffer, BarrierDesc);
        }

        const FCompiledPass* Pass = FindPass(Graph, Submission.Pass);
        const FNriPassCallback* Callback = FindCallback(PassCallbacks, Submission.Pass);
        Core.CmdBeginAnnotation(*CommandBuffer, Pass->Name.c_str(), nri::BGRA_UNUSED);
        Callback->Execute(*CommandBuffer, *Pass);
        Core.CmdEndAnnotation(*CommandBuffer);
        if (Core.EndCommandBuffer(*CommandBuffer) != nri::Result::SUCCESS)
        {
            Result.Diagnostics.push_back("Failed to end a render-graph command buffer.");
            return Result;
        }
        ++Result.RecordedPassCount;

        xr_vector<nri::FenceSubmitDesc> Waits;
        for (const FQueueWait& Wait : Submission.Waits)
        {
            if (Wait.SourcePass.Index >= PassSignals.size() ||
                !PassSignals[Wait.SourcePass.Index])
            {
                Result.Diagnostics.push_back("Render-graph queue wait references an unsignaled pass.");
                return Result;
            }
            const FPassSignal& Signal = *PassSignals[Wait.SourcePass.Index];
            Waits.push_back({Signal.Fence, Signal.Value, nri::StageBits::ALL});
        }
        for (const FNriExternalQueueSync& Sync : ExternalSync)
            if (Sync.Queue == Submission.Queue && FirstSubmission[Queue] == Index)
                Waits.insert(Waits.end(), Sync.InitialWaits.begin(), Sync.InitialWaits.end());

        const u64 SignalValue = ++NextFenceValues[Queue];
        xr_vector<nri::FenceSubmitDesc> Signals = {
            {QueueFences[Queue], SignalValue, nri::StageBits::ALL}};
        for (const FNriExternalQueueSync& Sync : ExternalSync)
            if (Sync.Queue == Submission.Queue && LastSubmission[Queue] == Index)
                Signals.insert(Signals.end(), Sync.FinalSignals.begin(), Sync.FinalSignals.end());

        nri::QueueSubmitDesc QueueSubmit = {};
        QueueSubmit.waitFences = Waits.data();
        QueueSubmit.waitFenceNum = static_cast<u32>(Waits.size());
        QueueSubmit.commandBuffers = &CommandBuffer;
        QueueSubmit.commandBufferNum = 1;
        QueueSubmit.signalFences = Signals.data();
        QueueSubmit.signalFenceNum = static_cast<u32>(Signals.size());
        if (Core.QueueSubmit(*QueueBindings.Get(Submission.Queue), QueueSubmit) !=
            nri::Result::SUCCESS)
        {
            Result.Diagnostics.push_back("Failed to submit a render-graph pass.");
            return Result;
        }
        ++Result.SubmittedPassCount;

        Context.LastFenceValue = SignalValue;
        if (Submission.Pass.Index >= PassSignals.size())
        {
            Result.Diagnostics.push_back("Render-graph pass handle is out of range.");
            return Result;
        }
        PassSignals[Submission.Pass.Index] = FPassSignal{
            QueueFences[Queue], SignalValue};
    }

    return Result;
}
} // namespace Tiramisu::RenderGraph

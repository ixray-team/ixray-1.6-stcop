#include "TiramisuRenderGraphNri.h"

#include <algorithm>

namespace Tiramisu::RenderGraph
{
namespace
{
xr_optional<nri::StageBits> ShaderStages(const EQueue Queue) noexcept
{
    switch (Queue)
    {
    case EQueue::Graphics: return nri::StageBits::GRAPHICS_SHADERS;
    case EQueue::Compute: return nri::StageBits::COMPUTE_SHADER;
    case EQueue::Copy: return std::nullopt;
    }
    return std::nullopt;
}
} // namespace

xr_optional<FNriAccessState> ToNriAccessState(const EAccess Access,
    const EQueue Queue) noexcept
{
    switch (Access)
    {
    case EAccess::None:
        return FNriAccessState{};
    case EAccess::ShaderRead:
    {
        const xr_optional<nri::StageBits> Stages = ShaderStages(Queue);
        if (!Stages) return std::nullopt;
        return FNriAccessState{nri::AccessBits::SHADER_RESOURCE,
            nri::Layout::SHADER_RESOURCE, *Stages};
    }
    case EAccess::StorageRead:
    case EAccess::StorageWrite:
    {
        const xr_optional<nri::StageBits> Stages = ShaderStages(Queue);
        if (!Stages) return std::nullopt;
        return FNriAccessState{nri::AccessBits::SHADER_RESOURCE_STORAGE,
            nri::Layout::SHADER_RESOURCE_STORAGE, *Stages};
    }
    case EAccess::ColorAttachmentWrite:
        if (Queue != EQueue::Graphics) return std::nullopt;
        return FNriAccessState{nri::AccessBits::COLOR_ATTACHMENT_WRITE,
            nri::Layout::COLOR_ATTACHMENT, nri::StageBits::COLOR_ATTACHMENT};
    case EAccess::DepthStencilRead:
        if (Queue != EQueue::Graphics) return std::nullopt;
        return FNriAccessState{nri::AccessBits::DEPTH_STENCIL_ATTACHMENT_READ,
            nri::Layout::DEPTH_STENCIL_READONLY,
            nri::StageBits::DEPTH_STENCIL_ATTACHMENT};
    case EAccess::DepthStencilWrite:
        if (Queue != EQueue::Graphics) return std::nullopt;
        return FNriAccessState{nri::AccessBits::DEPTH_STENCIL_ATTACHMENT_WRITE,
            nri::Layout::DEPTH_STENCIL_ATTACHMENT,
            nri::StageBits::DEPTH_STENCIL_ATTACHMENT};
    case EAccess::CopySource:
        return FNriAccessState{nri::AccessBits::COPY_SOURCE,
            nri::Layout::COPY_SOURCE, nri::StageBits::COPY};
    case EAccess::CopyDestination:
        return FNriAccessState{nri::AccessBits::COPY_DESTINATION,
            nri::Layout::COPY_DESTINATION, nri::StageBits::COPY};
    case EAccess::IndirectArgument:
        if (Queue != EQueue::Graphics && Queue != EQueue::Compute)
            return std::nullopt;
        return FNriAccessState{nri::AccessBits::ARGUMENT_BUFFER,
            nri::Layout::GENERAL, nri::StageBits::INDIRECT};
    case EAccess::Present:
        if (Queue != EQueue::Graphics) return std::nullopt;
        return FNriAccessState{nri::AccessBits::NONE,
            nri::Layout::PRESENT, nri::StageBits::NONE};
    }
    return std::nullopt;
}

nri::AccessLayoutStage ToNriTextureState(const FNriAccessState& State) noexcept
{
    return {State.Access, State.TextureLayout, State.Stages};
}

nri::AccessStage ToNriBufferState(const FNriAccessState& State) noexcept
{
    return {State.Access, State.Stages};
}

nri::Queue* FNriQueueBindings::Get(const EQueue Queue) const noexcept
{
    switch (Queue)
    {
    case EQueue::Graphics: return Graphics;
    case EQueue::Compute: return Compute;
    case EQueue::Copy: return Copy;
    }
    return nullptr;
}

FNriBarrierBatch BuildNriBarrierBatch(const FCompiledGraph& Graph,
    const FQueueSubmission& Submission,
    const xr_span<const FNriResourceBinding> Bindings,
    const FNriQueueBindings& Queues)
{
    FNriBarrierBatch Result;
    Result.TextureBarriers.reserve(Submission.Barriers.size());
    Result.BufferBarriers.reserve(Submission.Barriers.size());

    auto FindPassQueue = [&Graph](const FPassHandle Handle)
        -> xr_optional<EQueue>
    {
        if (!Handle.IsValid()) return std::nullopt;
        const auto It = std::find_if(Graph.Passes.begin(), Graph.Passes.end(),
            [Handle](const FCompiledPass& Pass)
            {
                return Pass.Handle == Handle;
            });
        if (It == Graph.Passes.end()) return std::nullopt;
        return It->Queue;
    };

    for (const FBarrier& Barrier : Submission.Barriers)
    {
        const auto Binding = std::find_if(Bindings.begin(), Bindings.end(),
            [&Barrier](const FNriResourceBinding& Candidate)
            {
                return Candidate.Resource == Barrier.Resource;
            });
        if (Binding == Bindings.end())
        {
            Result.Diagnostics.push_back("Missing NRI binding for render-graph resource " +
                std::to_string(Barrier.Resource.Index) + ".");
            continue;
        }
        if ((Binding->Texture == nullptr) == (Binding->Buffer == nullptr))
        {
            Result.Diagnostics.push_back("Render-graph resource " +
                std::to_string(Barrier.Resource.Index) +
                " must bind exactly one NRI texture or buffer.");
            continue;
        }

        const xr_optional<EQueue> SourceQueue = FindPassQueue(Barrier.SourcePass);
        const EQueue BeforeQueue = SourceQueue.value_or(Submission.Queue);
        const xr_optional<FNriAccessState> Before =
            ToNriAccessState(Barrier.Before, BeforeQueue);
        const xr_optional<FNriAccessState> After =
            ToNriAccessState(Barrier.After, Submission.Queue);
        if (!Before || !After)
        {
            Result.Diagnostics.push_back("Unsupported NRI access/queue mapping for resource " +
                std::to_string(Barrier.Resource.Index) + ".");
            continue;
        }

        if (Binding->Texture)
        {
            nri::TextureBarrierDesc Desc = {};
            Desc.texture = Binding->Texture;
            Desc.before = ToNriTextureState(*Before);
            Desc.after = ToNriTextureState(*After);
            Desc.mipOffset = Binding->MipOffset;
            Desc.mipNum = Binding->MipNum;
            Desc.layerOffset = Binding->LayerOffset;
            Desc.layerNum = Binding->LayerNum;
            Desc.planes = Binding->Planes;
            if (Barrier.QueueTransfer && Binding->ExclusiveQueueOwnership)
            {
                Desc.srcQueue = Queues.Get(BeforeQueue);
                Desc.dstQueue = Queues.Get(Submission.Queue);
                if (!Desc.srcQueue || !Desc.dstQueue)
                {
                    Result.Diagnostics.push_back(
                        "Missing NRI queue binding for texture ownership transfer.");
                    continue;
                }
            }
            Result.TextureBarriers.push_back(Desc);
        }
        else
        {
            nri::BufferBarrierDesc Desc = {};
            Desc.buffer = Binding->Buffer;
            Desc.before = ToNriBufferState(*Before);
            Desc.after = ToNriBufferState(*After);
            Result.BufferBarriers.push_back(Desc);
        }
    }

    return Result;
}
} // namespace Tiramisu::RenderGraph

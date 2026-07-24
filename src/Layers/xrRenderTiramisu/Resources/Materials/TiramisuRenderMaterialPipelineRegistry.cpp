#include "stdafx.h"
#include "TiramisuRenderMaterialPipelineRegistry.h"

#include <limits>

TiramisuRenderMaterialPipelineRegistry::~TiramisuRenderMaterialPipelineRegistry()
{
    CheckIsRenderThread();
    for (FSlot& Slot : Slots)
    {
        if (Slot.Pipeline && Slot.Owned)
            GRenderDevice.CoreInterface.DestroyPipeline(Slot.Pipeline);
        Slot.Pipeline = nullptr;
    }
    ActivePipelineCount = 0;
}

FMaterialPipelineHandle TiramisuRenderMaterialPipelineRegistry::RegisterPipeline_RenderThread(
    nri::Pipeline* Pipeline, const bool TakeOwnership)
{
    CheckIsRenderThread();
    if (!Pipeline)
        return {};

    u32 Index = 0;
    if (!FreeSlots.empty())
    {
        Index = FreeSlots.back();
        FreeSlots.pop_back();
    }
    else
    {
        if (Slots.size() >= std::numeric_limits<u32>::max())
            return {};
        Index = static_cast<u32>(Slots.size());
        Slots.emplace_back();
    }

    FSlot& Slot = Slots[Index];
    VERIFY(!Slot.Pipeline);
    Slot.Pipeline = Pipeline;
    Slot.Owned = TakeOwnership;
    ++ActivePipelineCount;
    return {Index, Slot.Generation};
}

void TiramisuRenderMaterialPipelineRegistry::ReleasePipeline_RenderThread(
    const FMaterialPipelineHandle Handle)
{
    CheckIsRenderThread();
    if (Handle.Index >= Slots.size())
        return;

    FSlot& Slot = Slots[Handle.Index];
    if (Slot.Generation != Handle.Generation || !Slot.Pipeline)
        return;

    if (Slot.Owned)
    {
        nri::Pipeline* RetiredPipeline = Slot.Pipeline;
        if (GRender)
        {
            GRender->DeferDelete_RenderThread([RetiredPipeline]
            {
                CheckIsRenderThread();
                GRenderDevice.CoreInterface.DestroyPipeline(RetiredPipeline);
            });
        }
        else
        {
            GRenderDevice.CoreInterface.DestroyPipeline(RetiredPipeline);
        }
    }
    Slot.Pipeline = nullptr;
    Slot.Owned = false;
    Slot.Generation = Slot.Generation == std::numeric_limits<u32>::max() ? 1 : Slot.Generation + 1;
    FreeSlots.push_back(Handle.Index);
    VERIFY(ActivePipelineCount > 0);
    --ActivePipelineCount;
}

nri::Pipeline* TiramisuRenderMaterialPipelineRegistry::ResolvePipeline_RenderThread(
    const FMaterialPipelineHandle Handle) const noexcept
{
    CheckIsRenderThread();
    if (Handle.Index >= Slots.size())
        return nullptr;
    const FSlot& Slot = Slots[Handle.Index];
    return Slot.Generation == Handle.Generation ? Slot.Pipeline : nullptr;
}

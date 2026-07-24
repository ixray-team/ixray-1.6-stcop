#pragma once

#include "TiramisuRenderTypes.h"

#include <MaterialRuntime.h>

#include <cstddef>
#include <cstdint>
#include <vector>

namespace nri
{
struct Pipeline;
}

// Публикует material pipelines по generation-counted handles.
class TiramisuRenderMaterialPipelineRegistry
{
public:
    TiramisuRenderMaterialPipelineRegistry() = default;
    ~TiramisuRenderMaterialPipelineRegistry();

    // Публикация, разрешение и удаление pipeline выполняются только в render thread.
    [[nodiscard]] FMaterialPipelineHandle RegisterPipeline_RenderThread(
        nri::Pipeline* Pipeline, bool TakeOwnership);
    void ReleasePipeline_RenderThread(FMaterialPipelineHandle Handle);
    [[nodiscard]] nri::Pipeline* ResolvePipeline_RenderThread(
        FMaterialPipelineHandle Handle) const noexcept;
    [[nodiscard]] size_t GetActivePipelineCount() const noexcept { return ActivePipelineCount; }

private:
    // Внутренняя запись ресурса с поколением и состоянием публикации.
    struct FSlot
    {
        nri::Pipeline* Pipeline = nullptr;
        u32 Generation = 1;
        bool Owned = false;
    };

    xr_vector<FSlot> Slots;
    xr_vector<u32> FreeSlots;
    size_t ActivePipelineCount = 0;
};

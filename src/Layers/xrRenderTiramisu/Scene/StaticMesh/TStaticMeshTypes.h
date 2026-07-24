#pragma once

#include "TiramisuRenderTypes.h"

#include <cstdint>

// Диапазон индексов и экземпляров одного draw внутри mesh batch.
struct FMeshBatchElement
{
    u32 CountVertex = 0;
    s32 OffsetVertex = 0;
    u32 CountIndex = 0;
    u32 OffsetIndex = 0;
};

// Material section static mesh с границами index range.
struct FStaticMeshSection
{
    u32 FirstIndex = 0;
    u32 NumTriangles = 0;
    s32 BaseVertexIndex = 0;
    u32 MinVertexIndex = 0;
    u32 MaxVertexIndex = 0;
    u32 MaterialSlot = 0;
};

[[nodiscard]] bool BuildStaticMeshBatchElement(
    const FStaticMeshSection& Section, FMeshBatchElement& Result) noexcept;

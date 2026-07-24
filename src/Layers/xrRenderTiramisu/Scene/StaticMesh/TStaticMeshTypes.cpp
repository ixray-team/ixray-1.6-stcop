#include "TStaticMeshTypes.h"

#include <limits>

bool BuildStaticMeshBatchElement(const FStaticMeshSection& Section,
    FMeshBatchElement& Result) noexcept
{
    if (Section.NumTriangles == 0 || Section.MaxVertexIndex < Section.MinVertexIndex ||
        Section.NumTriangles > std::numeric_limits<u32>::max() / 3u)
    {
        return false;
    }

    Result = {};
    Result.OffsetIndex = Section.FirstIndex;
    Result.CountIndex = Section.NumTriangles * 3u;
    Result.OffsetVertex = Section.BaseVertexIndex;
    Result.CountVertex = Section.MaxVertexIndex - Section.MinVertexIndex + 1u;
    return Result.CountVertex != 0;
}

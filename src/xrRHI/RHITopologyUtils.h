#pragma once
#include "RHIEnums.h"
#include "RHITypes.h"

struct RHITopologyUtils
{
    static u32 GetIndexCount(u32 primitiveCount, ERHI_PRIMITIVE_TOPOLOGY topology)
    {
        static u32 multipliers[] = {
            0,  // Undefined
            1,  // PointList
            2,  // LineList
            1,  // LineStrip (+1)
            3,  // TriangleList
            1,  // TriangleStrip (+2)
            1   // TriangleFan (+2)
        };

        static u32 additions[] = {
            0,  // Undefined
            0,  // PointList
            0,  // LineList
            1,  // LineStrip
            0,  // TriangleList
            2,  // TriangleStrip
            2   // TriangleFan
        };

        size_t index = static_cast<size_t>(topology);
        return primitiveCount * multipliers[index] + additions[index];
    }

    static u32 GetVertexCount(u32 primitiveCount, ERHI_PRIMITIVE_TOPOLOGY topology)
    {
        return GetIndexCount(primitiveCount, topology);
    }
};
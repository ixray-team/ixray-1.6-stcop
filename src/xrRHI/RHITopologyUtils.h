/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#pragma once
#include "RHIEnums.h"
#include "RHITypes.h"

struct RHITopologyUtils
{
    static u32 GetIndexCount(u32 primitiveCount, ERHI_PRIMITIVE_TOPOLOGY topology)
    {
        if (topology == ERHI_PRIMITIVE_TOPOLOGY::CONTROL_POINT_3_PATCH)
        {
            topology = ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST;
        }

        static u32 multipliers[] =
        {
            0,  // Undefined
            1,  // PointList
            2,  // LineList
            1,  // LineStrip (+1)
            3,  // TriangleList
            1,  // TriangleStrip (+2)
            1   // TriangleFan (+2)
        };

        static u32 additions[] =
        {
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

    static u32 GetPrimitiveCount(u32 count, ERHI_PRIMITIVE_TOPOLOGY topology)
    {
        switch (topology)
        {
        case ERHI_PRIMITIVE_TOPOLOGY::POINT_LIST: return count;
        case ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST: return count / 2;
        case ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP: return count > 1 ? count - 1 : 0;
        case ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_STRIP:
        case ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN: return count > 2 ? count - 2 : 0;
        default: return count / 3;
        }
    }
};
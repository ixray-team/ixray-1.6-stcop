#pragma once

inline u32 GetIndexCount(u32 primitiveCount, u32 topology)
{
    switch (topology)
    {
    case 1: // PointList
        return primitiveCount;
    case 2: // LineList
        return primitiveCount * 2;
    case 3: // LineStrip
        return primitiveCount + 1;
    case 4: // TriangleList
        return primitiveCount * 3;
    case 5: // TriangleStrip
        return primitiveCount + 2;
    default:
        return 0;
    }
}

inline u32 GetVertexCount(u32 primitiveCount, u32 topology)
{
    switch (topology)
    {
    case 1: // PointList
        return primitiveCount;
    case 2: // LineList
        return primitiveCount * 2;
    case 3: // LineStrip
        return primitiveCount + 1;
    case 4: // TriangleList
        return primitiveCount * 3;
    case 5: // TriangleStrip
        return primitiveCount + 2;
    default:
        return 0;
    }
}
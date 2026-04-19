#include "StdAfx.h"

#include <cmath>

#include "IxAiConstants.h"
#include "IxAiPerceptionSpatialGrid.h"

namespace
{
    u64 PackCell(s32 cellX, s32 cellZ)
    {
        return (u64)(u32)cellX << 32 | (u32)cellZ;
    }
}

IxAiPerceptionSpatialGrid::IxAiPerceptionSpatialGrid()
    : _cellSize(IxAiConstants::kPerceptionSpatialCellSize)
{
}

void IxAiPerceptionSpatialGrid::Clear()
{
    _buckets.clear();
}

void IxAiPerceptionSpatialGrid::Rebuild(const xr_vector<IxAiPerceptionEvent>& events)
{
    _buckets.clear();

    if (events.empty())
    {
        return;
    }

    const size_t cap = (size_t)IxAiConstants::kPerceptionGlobalEventCap;
    const size_t bucketReserve = events.size() < cap ? events.size() : cap;
    _buckets.reserve(bucketReserve);

    const f32 invCell = 1.f / _cellSize;

    for (u32 eventIndex = 0; eventIndex < (u32)events.size(); ++eventIndex)
    {
        const IxAiPerceptionEvent& event = events[eventIndex];
        const s32 cellX = (s32)floorf(event._position.x * invCell);
        const s32 cellZ = (s32)floorf(event._position.z * invCell);
        const u64 key = PackCell(cellX, cellZ);
        xr_vector<u32>& indices = _buckets[key];

        if (indices.empty())
        {
            indices.reserve(16u);
        }

        indices.push_back(eventIndex);
    }
}

void IxAiPerceptionSpatialGrid::GatherCandidateIndices(const Fvector& origin, f32 radius, xr_vector<u32>& outIndices) const
{
    outIndices.clear();

    if (outIndices.capacity() < (size_t)IxAiConstants::kPerceptionGlobalEventCap)
    {
        outIndices.reserve(IxAiConstants::kPerceptionGlobalEventCap);
    }

    if (_buckets.empty())
    {
        return;
    }

    const f32 invCell = 1.f / _cellSize;
    const s32 ix0 = (s32)floorf((origin.x - radius) * invCell);
    const s32 ix1 = (s32)floorf((origin.x + radius) * invCell);
    const s32 iz0 = (s32)floorf((origin.z - radius) * invCell);
    const s32 iz1 = (s32)floorf((origin.z + radius) * invCell);

    for (s32 cellZ = iz0; cellZ <= iz1; ++cellZ)
    {
        for (s32 cellX = ix0; cellX <= ix1; ++cellX)
        {
            const u64 key = PackCell(cellX, cellZ);
            const xr_hash_map<u64, xr_vector<u32>>::const_iterator bucketIt = _buckets.find(key);

            if (bucketIt == _buckets.cend())
            {
                continue;
            }

            for (u32 eventIndex : bucketIt->second)
            {
                outIndices.push_back(eventIndex);
            }
        }
    }
}


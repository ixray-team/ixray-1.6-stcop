#pragma once

#include "../../xrCore/_stl_extensions.h"
#include "../../xrCore/_types.h"
#include "../../xrCore/_vector3d.h"

#include "IxAiTypes.h"

class IxAiPerceptionSpatialGrid final
{
public:
    IxAiPerceptionSpatialGrid();

    void Clear();
    void Rebuild(const xr_vector<IxAiPerceptionEvent>& events);

    void GatherCandidateIndices(const Fvector& origin, f32 radius, xr_vector<u32>& outIndices) const;

private:
    f32 _cellSize{};
    xr_hash_map<u64, xr_vector<u32>> _buckets{};
};


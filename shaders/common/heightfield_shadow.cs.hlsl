/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
// The sun's shadow over the level's heightfield (r4_heightfield_shadows). The grid is walked along the sun in rows,
// sheared to follow it; a texel's ceiling is the highest any texel before it on its row casts, the ceiling dropping
// by the sun's slope a step. HEIGHTFIELD_PASS 0: the running maximum inside blocks of 64 steps, and each block's
// last; 1: the running maximum of the blocks of a row; 2: each texel of the grid takes its ceiling out of the
// sheared rows. The output holds the ceiling negated, so the comparison sampler's less-equal test is "lit"

#include "common\fixed_cb.hlsli"

#define HEIGHTFIELD_BLOCK 64
#define HEIGHTFIELD_EMPTY (-1.0e20f)

Texture2D<float> s_heightfield_input : register(t27);
RWTexture2D<float> heightfield_output : register(u0);

// returns how far the row of the sheared scan is moved at a step along it; the resolve undoes the same whole shift
int row_offset(uint step)
{
    return int(floor(HeightfieldScan.z * step - HeightfieldScan.w + 0.5f));
}

#if HEIGHTFIELD_PASS < 2
groupshared float group_heights[HEIGHTFIELD_BLOCK];

// Leaves in group_heights the running maximum of the group's values. Every lane takes part in every barrier
void prefix_max(uint lane, float value)
{
    group_heights[lane] = value;
    GroupMemoryBarrierWithGroupSync();

    [unroll]
    for (uint offset = 1; offset < HEIGHTFIELD_BLOCK; offset <<= 1) {
        float before = lane >= offset ? group_heights[lane - offset] : HEIGHTFIELD_EMPTY;
        GroupMemoryBarrierWithGroupSync();
        group_heights[lane] = max(group_heights[lane], before);
        GroupMemoryBarrierWithGroupSync();
    }
}
#endif

#if HEIGHTFIELD_PASS == 0
RWTexture2D<float> heightfield_blocks : register(u1);

[numthreads(HEIGHTFIELD_BLOCK, 1, 1)]
void main(uint3 group : SV_GroupID, uint lane : SV_GroupIndex)
{
    uint step = group.x * HEIGHTFIELD_BLOCK + lane;
    int major = HeightfieldScan.y > 0.0f ? int(step) : int(HeightfieldGrid.z) - 1 - int(step);
    int minor = int(group.y) + row_offset(step);
    int2 texel = HeightfieldScan.x < 0.5f ? int2(major, minor) : int2(minor, major);
    float height = HEIGHTFIELD_EMPTY;

    // A code of zero is a hole in the heightfield: it casts nothing
    if (all(texel >= 0) && all(texel < int2(HeightfieldGrid.xy))) {
        float code = s_heightfield_input.Load(int3(texel, 0));
        height = code > 0.0f ? code * HeightfieldDecode.x + HeightfieldDecode.y : HEIGHTFIELD_EMPTY;
    }

    prefix_max(lane, height + step * HeightfieldParams.y);
    heightfield_output[uint2(step, group.y)] = group_heights[lane];

    if (lane == HEIGHTFIELD_BLOCK - 1) {
        heightfield_blocks[group.xy] = group_heights[lane];
    }
}
#elif HEIGHTFIELD_PASS == 1
[numthreads(HEIGHTFIELD_BLOCK, 1, 1)]
void main(uint3 group : SV_GroupID, uint lane : SV_GroupIndex)
{
    uint blocks = (uint(HeightfieldGrid.z) + HEIGHTFIELD_BLOCK - 1) / HEIGHTFIELD_BLOCK;
    prefix_max(lane, lane < blocks ? s_heightfield_input.Load(int3(lane, group.y, 0)) : HEIGHTFIELD_EMPTY);

    if (lane < blocks) {
        heightfield_output[uint2(lane, group.y)] = group_heights[lane];
    }
}
#else
Texture2D<float> s_heightfield_blocks : register(t28);

[numthreads(8, 8, 1)]
void main(uint2 texel : SV_DispatchThreadID)
{
    if (any(texel >= uint2(HeightfieldGrid.xy))) {
        return;
    }

    int major = HeightfieldScan.x < 0.5f ? texel.x : texel.y;
    int minor = HeightfieldScan.x < 0.5f ? texel.y : texel.x;
    int step = HeightfieldScan.y > 0.0f ? major : int(HeightfieldGrid.z) - 1 - major;
    int row = minor - row_offset(step);
    float height = HEIGHTFIELD_EMPTY;

    if (row >= 0 && row < int(HeightfieldGrid.w)) {
        height = s_heightfield_input.Load(int3(step, row, 0));
        int block = step / HEIGHTFIELD_BLOCK;

        if (block > 0) {
            height = max(height, s_heightfield_blocks.Load(int3(block - 1, row, 0)));
        }

        height -= step * HeightfieldParams.y;
    }

    // Nearest, not filtered: the lookup filters the comparisons, as a ceiling filtered between a roof and the ground
    // would make a slope that is not there
    heightfield_output[texel] = -height;
}
#endif

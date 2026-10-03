#include "common.hlsli"

RWTexture2D<float> u_sslr_depth_min : register(u0);
groupshared float tile_depth[64];

[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID, uint2 Gid : SV_GroupID, uint GI : SV_GroupIndex)
{
    uint width, height;
    s_position.GetDimensions(width, height);
    float depth = 1.0f;
    if (all(DTid < uint2(width, height)))
    {
        depth = s_position.Load(int3(DTid, 0)).x;
    }
    tile_depth[GI] = depth;
    GroupMemoryBarrierWithGroupSync();

    [unroll]
    for (uint stride = 32; stride > 0; stride >>= 1)
    {
        if (GI < stride)
        {
            tile_depth[GI] = min(tile_depth[GI], tile_depth[GI + stride]);
        }
        GroupMemoryBarrierWithGroupSync();
    }

    if (GI == 0)
    {
        u_sslr_depth_min[Gid] = tile_depth[0];
    }
}

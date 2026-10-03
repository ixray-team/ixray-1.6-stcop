#include "common.hlsli"

RWTexture2D<float> u_hiz0 : register(u0);
RWTexture2D<float> u_hiz1 : register(u1);
RWTexture2D<float> u_hiz2 : register(u2);
RWTexture2D<float> u_hiz3 : register(u3);
RWTexture2D<float> u_hiz4 : register(u4);
groupshared float hiz_tile[64];

float HiZSource(uint2 Pixel)
{
#ifdef SSLR_HIZ_MIPS
    return u_hiz0[Pixel];
#else
    float Depth = s_position.Load(int3(Pixel, 0)).x;
    return Depth < 0.02f ? 1.0f : Depth;
#endif
}

[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID, uint2 GTid : SV_GroupThreadID, uint2 Gid : SV_GroupID)
{
    uint2 Pixel = DTid * 2u;
    float Depth = min(min(HiZSource(Pixel), HiZSource(Pixel + uint2(1u, 0u))), min(HiZSource(Pixel + uint2(0u, 1u)), HiZSource(Pixel + 1u)));
    u_hiz1[DTid] = Depth;
    uint Index = GTid.y * 8u + GTid.x;
    hiz_tile[Index] = Depth;

    [unroll]
    for (uint level_idx = 1; level_idx < 4; ++level_idx)
    {
        uint Stride = 1u << level_idx;
        uint Offset = Stride >> 1;
        GroupMemoryBarrierWithGroupSync();
        if (all(GTid % Stride == 0u))
        {
            Depth = min(min(Depth, hiz_tile[Index + Offset]), min(hiz_tile[Index + Offset * 8u], hiz_tile[Index + Offset * 9u]));
            hiz_tile[Index] = Depth;
            uint2 Texel = Gid * (8u / Stride) + GTid / Stride;
            if (level_idx == 1)
                u_hiz2[Texel] = Depth;
            else if (level_idx == 2)
                u_hiz3[Texel] = Depth;
            else
                u_hiz4[Texel] = Depth;
        }
    }
}

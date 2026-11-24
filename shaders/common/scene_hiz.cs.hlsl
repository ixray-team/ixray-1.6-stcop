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
// The scene's depth pyramid: the farthest depth under each of its texels. A dispatch writes up to six levels at once,
// as a single pass downsampler does: a group of 16 x 16 threads takes 32 x 32 texels of the first of them and hands
// every next level down through its shared memory. The first dispatch reads the frame's depth, the next one the last
// level of the one before

#define HIZ_LEVELS 6u

cbuffer SceneHizBlock : register(b0)
{
    uint4 hiz_level; // xy: size of the first level written, z: bit 0 when it is taken from the depth, bit 1 for a loose pyramid, w: levels written
    float4 hiz_source; // xy: depth texels to one texel of the first level, zw: size of the view in the depth, which holds it in its corner
}

#ifdef USE_MSAA
Texture2DMS<float> g_depth : register(t0);
#else
Texture2D<float> g_depth : register(t0);
#endif
SamplerState smp_depth : register(s0);
RWTexture2D<float> g_levels[HIZ_LEVELS] : register(u0);
RWTexture2D<float> g_before : register(u6);

groupshared float group_depth[16][16];

// The levels a loose pyramid lets one texel in four be a hole in: what is cut out by its texture hides what is behind
// it where it covers most of a texel, not all of it. Such a pyramid may hide what a hole would show
#define HIZ_LOOSE_LEVELS 3u

// returns the farthest of four, or in a loose pyramid the second farthest
float reduce(float4 depths, bool is_loose)
{
    float2 high = max(depths.xy, depths.zw);
    float2 low = min(depths.xy, depths.zw);
    return is_loose ? max(min(high.x, high.y), max(low.x, low.y)) : max(high.x, high.y);
}

float fetch_first(uint2 at)
{
    float farthest = 0.0f;

    [branch] if (hiz_level.z & 1u) {
        uint2 first = uint2(float2(at) * hiz_source.xy);
        uint2 last = min(uint2(ceil(float2(at + 1u) * hiz_source.xy)), uint2(hiz_source.zw));
#ifdef USE_MSAA
        // Every sample: at an edge a pixel is as far as the farthest of what it covers
        uint3 depth_size;
        g_depth.GetDimensions(depth_size.x, depth_size.y, depth_size.z);

        for (uint y = first.y; y < last.y; y++) {
            for (uint x = first.x; x < last.x; x++) {
                for (uint sample_idx = 0u; sample_idx < depth_size.z; sample_idx++) {
                    farthest = max(farthest, g_depth.Load(int2(x, y), sample_idx));
                }
            }
        }
#else
        float2 depth_size;
        g_depth.GetDimensions(depth_size.x, depth_size.y);

        // Four texels a read; a read at the edge of the depth takes texels of the other edge with it, which can only
        // make the texel farther
        for (uint y = first.y; y < last.y; y += 2u) {
            for (uint x = first.x; x < last.x; x += 2u) {
                float4 depths = g_depth.GatherRed(smp_depth, float2(x + 1u, y + 1u) / depth_size);
                farthest = max(farthest, max(max(depths.x, depths.y), max(depths.z, depths.w)));
            }
        }
#endif
    } else {
        uint2 before_size;
        g_before.GetDimensions(before_size.x, before_size.y);

        for (uint texel_idx = 0u; texel_idx < 4u; texel_idx++) {
            uint2 from = at * 2u + uint2(texel_idx & 1u, texel_idx >> 1);

            if (all(from < before_size)) {
                farthest = max(farthest, g_before[from]);
            }
        }
    }

    return farthest;
}

void store(uint level, uint2 at, float depth)
{
    if (level >= hiz_level.w || any(at >= max(hiz_level.xy >> level, 1u))) {
        return;
    }

    switch (level) {
    case 0u:
        g_levels[0][at] = depth;
        break;
    case 1u:
        g_levels[1][at] = depth;
        break;
    case 2u:
        g_levels[2][at] = depth;
        break;
    case 3u:
        g_levels[3][at] = depth;
        break;
    case 4u:
        g_levels[4][at] = depth;
        break;
    default:
        g_levels[5][at] = depth;
        break;
    }
}

[numthreads(16, 16, 1)]
void main(uint3 group_id : SV_GroupID, uint3 thread_id : SV_GroupThreadID)
{
    // A texel outside its level stays 0, which no maximum takes
    float4 first = 0.0f;
    bool is_whole = true;
    bool is_loose = (hiz_level.z & 2u) != 0u;

    for (uint texel_idx = 0u; texel_idx < 4u; texel_idx++) {
        uint2 at = group_id.xy * 32u + thread_id.xy * 2u + uint2(texel_idx & 1u, texel_idx >> 1);

        if (all(at < hiz_level.xy)) {
            first[texel_idx] = fetch_first(at);
            store(0u, at, first[texel_idx]);
        } else {
            is_whole = false;
        }
    }

    float farthest = reduce(first, is_loose && is_whole);
    store(1u, group_id.xy * 16u + thread_id.xy, farthest);

    // One thread in four goes on to the next level, with what it and its three neighbours of this one found
    [unroll] for (uint level = 2u; level < HIZ_LEVELS; level++) {
        group_depth[thread_id.y][thread_id.x] = farthest;
        GroupMemoryBarrierWithGroupSync();

        uint step = 1u << (level - 1u);
        uint half_step = step >> 1;

        if (all((thread_id.xy & (step - 1u)) == 0u)) {
            float4 four = float4(farthest, group_depth[thread_id.y][thread_id.x + half_step], group_depth[thread_id.y + half_step][thread_id.x],
                group_depth[thread_id.y + half_step][thread_id.x + half_step]);
            farthest = reduce(four, is_loose && (hiz_level.z & 1u) && level <= HIZ_LOOSE_LEVELS && all(four > 0.0f));
            store(level, group_id.xy * (32u >> level) + thread_id.xy / step, farthest);
        }

        GroupMemoryBarrierWithGroupSync();
    }
}

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
// The cube around the eye as the octahedral map its mirrors read (common\mirror_cube.hlsli), every level in one
// dispatch. Level 0 is what the cube holds, by its share (the sky is the mirrors' own). r_offscreen_reflection_quality
// medium and up (the default): each level after it is that convolved by GGX for a roughness of level / MIRROR_IBL_LAST,
// the eye taken along the normal (the split sum of Karis, SIGGRAPH 2013), a level to a z of the dispatch, and level 0
// settles over frames of jittered faces in u_history while the eye stays. Low (MIRROR_IBL_BOX): the levels are boxed
// down as AMD's single pass downsampler boxes them, a group to a tile of 64 x 64 texels of level 0
#include "common\common.hlsli"
#include "common\post_cb.hlsli"
#include "common\mirror_cube.hlsli"

RWTexture2D<float4> u_ibl0 : register(u0);
RWTexture2D<float4> u_ibl1 : register(u1);
RWTexture2D<float4> u_ibl2 : register(u2);
RWTexture2D<float4> u_ibl3 : register(u3);
RWTexture2D<float4> u_ibl4 : register(u4);
RWTexture2D<float4> u_ibl5 : register(u5);
RWTexture2D<float4> u_ibl6 : register(u6);

#define IBL_SAMPLES 48u

// returns what the cube holds in a direction, by its share: .a - how much of it there is, fading out with the fog over
// its distance; .rgb - its light times that. The sky is not in the map: the mirrors lay their own over what is left,
// dimmed as they dim it. With the weather's sky in the map, a mirror showed it whole beside its own dimmed one, a bright
// halo round every edge. A texel at a time: filtered, the cube's colour bled what its never-cleared sky holds into edges
float4 ibl_scene(float3 dir)
{
    float fog = saturate(s_env_dist.SampleLevel(smp_nofilter, dir, 0.0f).x * fog_params.w + fog_params.x);
    float share = 1.0f - fog * fog;
    return float4(min(s_env.SampleLevel(smp_nofilter, dir, 0.0f).rgb, 20.0f) * share, share);
}

void ibl_store(uint level, uint2 texel, float4 value)
{
    switch (level) {
    case 0u: u_ibl0[texel] = value; break;
    case 1u: u_ibl1[texel] = value; break;
    case 2u: u_ibl2[texel] = value; break;
    case 3u: u_ibl3[texel] = value; break;
    case 4u: u_ibl4[texel] = value; break;
    case 5u: u_ibl5[texel] = value; break;
    default: u_ibl6[texel] = value; break;
    }
}

#ifdef MIRROR_IBL_BOX
// A level's tile below level 2, a texel to a thread of the group
groupshared float4 g_box[16][16];

[numthreads(16, 16, 1)]
void cs_main(uint3 group : SV_GroupID, uint3 thread : SV_GroupThreadID)
{
    uint width, height;
    u_ibl0.GetDimensions(width, height);

    // A thread takes 4 x 4 texels of level 0 down to one of level 2
    uint2 corner = group.xy * 64u + thread.xy * 4u;
    float4 block = 0.0f;
    [unroll]
    for (uint quad_idx = 0u; quad_idx < 4u; quad_idx++) {
        uint2 quad = corner + uint2(quad_idx & 1u, quad_idx >> 1u) * 2u;
        float4 sum = 0.0f;
        [unroll]
        for (uint texel_idx = 0u; texel_idx < 4u; texel_idx++) {
            uint2 texel = quad + uint2(texel_idx & 1u, texel_idx >> 1u);
            float4 light = ibl_scene(mirror_oct_dir((float2(texel) + 0.5f) / float(width)));
            ibl_store(0u, texel, light);
            sum += light;
        }
        ibl_store(1u, quad / 2u, sum * 0.25f);
        block += sum * 0.25f;
    }
    block *= 0.25f;
    ibl_store(2u, corner / 4u, block);
    g_box[thread.y][thread.x] = block;
    GroupMemoryBarrierWithGroupSync();

    // Each level after it from four texels of the one before, in the group's memory: a level's tile is 64 >> level
    [unroll]
    for (uint level = 3u; level <= 6u; level++) {
        uint side = 64u >> level;
        bool is_active = all(thread.xy < side);
        float4 average = 0.0f;
        if (is_active) {
            uint2 source = thread.xy * 2u;
            average = (g_box[source.y][source.x] + g_box[source.y][source.x + 1u] + g_box[source.y + 1u][source.x] + g_box[source.y + 1u][source.x + 1u]) * 0.25f;
            ibl_store(level, group.xy * side + thread.xy, average);
        }
        GroupMemoryBarrierWithGroupSync();
        if (is_active) {
            g_box[thread.y][thread.x] = average;
        }
        GroupMemoryBarrierWithGroupSync();
    }
}
#else
// The sharpest level as it has settled over frames: read and written by the thread of its texel alone
RWTexture2D<float4> u_history : register(u7);

// returns a half vector of GGX around the normal for a point of the Hammersley set
float3 ibl_half(uint sample_idx, float roughness, float3 normal)
{
    float2 xi = float2(float(sample_idx) / float(IBL_SAMPLES), float(reversebits(sample_idx)) * 2.3283064e-10f);
    float alpha = roughness * roughness;
    float phi = 6.2831853f * xi.x;
    float cos_theta = sqrt((1.0f - xi.y) / (1.0f + (alpha * alpha - 1.0f) * xi.y));
    float sin_theta = sqrt(1.0f - cos_theta * cos_theta);
    float3 side = normalize(cross(abs(normal.y) < 0.999f ? float3(0.0f, 1.0f, 0.0f) : float3(1.0f, 0.0f, 0.0f), normal));
    float3 up = cross(normal, side);
    return normalize((side * cos(phi) + up * sin(phi)) * sin_theta + normal * cos_theta);
}

[numthreads(8, 8, 1)]
void cs_main(uint3 DTid : SV_DispatchThreadID)
{
    uint width, height;
    u_ibl0.GetDimensions(width, height);
    uint size = max(width >> DTid.z, 1u);
    [branch] if (any(DTid.xy >= size)) {
        return;
    }

    float3 normal = mirror_oct_dir((float2(DTid.xy) + 0.5f) / float(size));
    float4 light;

    [branch] if (DTid.z == 0u) {
        light = lerp(ibl_scene(normal), u_history[DTid.xy], mirror_history.x);
        u_history[DTid.xy] = light;
    } else {
        float roughness = float(DTid.z) / MIRROR_IBL_LAST;
        float4 sum = 0.0f;
        float weight = 0.0f;
        [loop]
        for (uint sample_idx = 0u; sample_idx < IBL_SAMPLES; sample_idx++) {
            float3 half_dir = ibl_half(sample_idx, roughness, normal);
            float3 to_light = 2.0f * dot(normal, half_dir) * half_dir - normal;
            float facing = saturate(dot(normal, to_light));
            [branch] if (facing > 0.0f) {
                sum += ibl_scene(to_light) * facing;
                weight += facing;
            }
        }
        light = sum / max(weight, 1e-4f);
    }

    ibl_store(DTid.z, DTid.xy, light);
}
#endif

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
// Sampling of the scene's texture buckets. The shader that includes this declares the buckets (g_buckets, or
// g_bucket0..15 for a compiler without resource arrays), their sampler smp_scene and, unless it defines
// SCENE_BUCKETS_TAILS, the heads' table resolve_heads, the frame resolve_frame and the feedback g_feedback

// A pipeline of materials with a texture bound as it is (SCENE_BUCKETS_BOUND) reads the buckets as sample_bucket, the
// bound textures by the includer's sample_bound, both through sample_slice at the end
#ifdef SCENE_BUCKETS_BOUND
#define sample_slice sample_bucket
#endif

#ifdef __hlsl_dx_compiler
float4 sample_slice(uint slot, float2 uv, float2 dx, float2 dy)
{
    return g_buckets[NonUniformResourceIndex(slot >> 16)].SampleGrad(smp_scene, float3(uv, float(slot & 0xFFFFu)), dx, dy);
}
#else
float4 sample_slice(uint slot, float2 uv, float2 uv_dx, float2 uv_dy)
{
    float3 location = float3(uv, float(slot & 0xFFFFu));

    switch (slot >> 16) {
    case 1u:
        return g_bucket1.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 2u:
        return g_bucket2.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 3u:
        return g_bucket3.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 4u:
        return g_bucket4.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 5u:
        return g_bucket5.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 6u:
        return g_bucket6.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 7u:
        return g_bucket7.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 8u:
        return g_bucket8.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 9u:
        return g_bucket9.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 10u:
        return g_bucket10.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 11u:
        return g_bucket11.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 12u:
        return g_bucket12.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 13u:
        return g_bucket13.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 14u:
        return g_bucket14.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    case 15u:
        return g_bucket15.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    default:
        return g_bucket0.SampleGrad(smp_scene, location, uv_dx, uv_dy);
    }
}
#endif

#ifdef SCENE_BUCKETS_BOUND
#undef sample_slice

float4 sample_slice(uint slot, float2 uv, float2 uv_dx, float2 uv_dy)
{
    uint bound = (slot >> 16) - SCENE_TEXTURE_BUCKETS;

    [branch] if (bound < SCENE_TEXTURE_BOUND) {
        return sample_bound(bound, uv, uv_dx, uv_dy);
    }

    return sample_bucket(slot, uv, uv_dx, uv_dy);
}
#endif

// A map of a material: its head where that is loaded, else its tail, in one read. A head holds the whole chain
// of the texture, so the level is the sampler's to pick; working it out here and blending two reads behind
// branches cost a wall's pixel half as much again. Taps: the most the anisotropic filter may take; the short
// axis of the footprint is widened to keep it to that
float4 sample_map(uint tail, uint head, uint info, float2 uv, float2 dx, float2 dy, float taps)
{
    if (taps < 16.0f) {
        float2 sizes = float2(dot(dx, dx), dot(dy, dy));
        float least = max(sizes.x, sizes.y) / (taps * taps);
        dx *= sizes.x < least ? sqrt(least / max(sizes.x, 1e-20f)) : 1.0f;
        dy *= sizes.y < least ? sqrt(least / max(sizes.y, 1e-20f)) : 1.0f;
    }

    float2 scale = 1.0f / float2(1u << (info & 15u), 1u << ((info >> 4) & 15u));
    uv *= scale;
    dx *= scale;
    dy *= scale;
    uint stream = info >> SCENE_TEXTURE_STREAM_SHIFT;

#ifdef SCENE_BUCKETS_TAILS
    // Nothing loads the streamed heads for this shader yet: it has their tails
    head = stream != 0u ? SCENE_TEXTURE_ABSENT : head;
#else
    uint loaded = resolve_heads[max(stream, 1u) - 1u];
    head = stream != 0u ? loaded : head;

    // Asked for while it is needed, loaded or not: a head nobody asks for gives its place away. A shader that
    // leaves the asking to others (SCENE_BUCKETS_QUIET) does not work the level out at all
#ifndef SCENE_BUCKETS_QUIET
    float2 axes = float2(dot(dx, dx), dot(dy, dy));

    [branch] if (stream != 0u && max(min(axes.x, axes.y), max(axes.x, axes.y) / (taps * taps)) * (SCENE_TEXTURE_TAIL * SCENE_TEXTURE_TAIL) < 1.0f) {
        g_feedback[stream - 1u] = (resolve_frame.x << 8) | 128u;
    }
#endif
#endif

    return sample_slice(head != SCENE_TEXTURE_ABSENT ? head : tail, uv, dx, dy);
}

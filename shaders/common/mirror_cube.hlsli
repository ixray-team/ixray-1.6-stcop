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
#ifndef MIRROR_CUBE_HLSLI
#define MIRROR_CUBE_HLSLI

// The cube around the eye for a surface the reflection passes never see (water, glass). s_mirror_cube: what it holds,
// the scene's light by its share (.a; the sky is the mirror's own), as an octahedral map whose levels are filtered by
// GGX for a roughness of level / MIRROR_IBL_LAST (lighting\mirror_ibl.hlsl). s_mirror_cube_dist: how far each of its
// texels is from the eye. It stands along the world's axes with the eye at its middle. The map is read with the
// sampler that blends its levels: by the nearest one, a roughness between two read the blurrier
Texture2D s_mirror_cube : register(t46);
TextureCube s_mirror_cube_dist : register(t47);

#define MIRROR_CUBE_STEPS 12
#define MIRROR_CUBE_GROWTH 1.5f
// Halvings of the step a crossing is found in. The steps grow to metres: four left the crossing as much as a third of a
// metre off, neighbours fell on different halvings, and a facade came out in hard-edged patches
#define MIRROR_CUBE_REFINE 8
#define MIRROR_IBL_LAST 6.0f

// returns where a direction lies in the octahedral map, 0..1: y up, the lower half folded out to the corners
float2 mirror_oct(float3 dir)
{
    dir /= abs(dir.x) + abs(dir.y) + abs(dir.z);
    float2 folded = (1.0f - abs(dir.zx)) * float2(dir.x >= 0.0f ? 1.0f : -1.0f, dir.z >= 0.0f ? 1.0f : -1.0f);
    return (dir.y >= 0.0f ? dir.xz : folded) * 0.5f + 0.5f;
}

// returns the direction a place of the octahedral map stands for
float3 mirror_oct_dir(float2 uv)
{
    float2 place = uv * 2.0f - 1.0f;
    float3 dir = float3(place.x, 1.0f - abs(place.x) - abs(place.y), place.y);
    float fold = saturate(-dir.y);
    dir.xz += float2(dir.x >= 0.0f ? -fold : fold, dir.z >= 0.0f ? -fold : fold);
    return normalize(dir);
}

// returns where a point of the world is in the cube's space: from its middle, which stays put while the eye sways
// (m_probe's translation is the eye's place in it)
float3 mirror_cube_origin(float3 position_world)
{
    return position_world - eye_position + m_probe._14_24_34;
}

// returns the pixel's share of a step: the same every frame without TAA, a new one each frame with it
float mirror_noise(float2 pixel)
{
    pixel += m_taa_jitter.z < 0.0f ? 0.0f : 5.588238f * float(uint(m_taa_jitter.w) % 64u);
    return frac(52.9829189f * frac(dot(pixel, float2(0.06711056f, 0.00583715f))));
}

// returns how far the cube holds a surface in the direction of this point of its space, squared, between its texels:
// read a texel at a time, whether a ray met a surface or passed it changed a texel at a time, and every edge the mirror
// showed, a roof against the sky most of all, came out in the steps of the cube's faces enlarged
float mirror_cube_reach(float3 point_probe)
{
    float reach = s_mirror_cube_dist.SampleLevel(smp_rtlinear, point_probe, 0.0f).x;
    return reach * reach;
}

// returns what the cube shows where a ray from origin (from the eye, along the world's axes) meets what it holds, .a -
// how much of the mirror it is, as FastViewReflections finds it: the ray is marched against the cube's distances in
// steps that grow and its crossing is halved four times; lod: the level of the map its colour is read at. It ends at the first surface it passes behind, but for one
// nearer the eye than its origin, which it passes on behind when the crossing is not on it. The cube's colour is never
// cleared: where the ray meets nothing, the share falls to the sky by the fog over the farthest distance it passed
// open: how much of the ray the march found reaching the sky past everything the cube holds. The mirror's sky is
// dimmed by how much sky the point sees, but a ray the march saw escape is not: dimmed, the sky above a roof across a
// street came out darker than the roof, a dark band along its edge
float4 mirror_cube_at_eye(float3 along, float roughness, out float open);

// start: how far along the ray the march begins (where a screen trace left it); hit: how far along the ray it met a
// surface, -1 where it met none
float4 mirror_cube_from(float3 origin, float3 along, float start, float noise, float lod, out float open, out float hit)
{
    hit = -1.0f;
#ifndef USE_OFFSCREEN_MARCH
    // r_offscreen_reflection_quality low: the map along the ray, as from the eye. Its parallax is wrong for what is near,
    // but no ray is marched: no edge a march misjudges, no noise of its steps
    return mirror_cube_at_eye(along, lod / MIRROR_IBL_LAST, open);
#endif
    open = 0.0f;
    float origin_sq = dot(origin, origin);
    float range = fog_params.z - sqrt(origin_sq) - start;
    [branch]
    if (range <= 0.0f) {
        return 0.0f;
    }
    float scale = range / (pow(MIRROR_CUBE_GROWTH, float(MIRROR_CUBE_STEPS)) - 1.0f);
    float growth = pow(MIRROR_CUBE_GROWTH, noise);
    float before = start + 0.011f;
    float at = before;
    float3 found = origin;
    float found_sq = 0.0f;
    float farthest = 0.0f;
    float was = 0.0f;
    bool is_valid = false;
    bool is_hit = false;
    [loop]
    for (int step_idx = 0; step_idx < MIRROR_CUBE_STEPS; step_idx++) {
        float next = start + 0.011f + scale * (growth - 1.0f);
        growth *= MIRROR_CUBE_GROWTH;
        float3 point_probe = origin + along * next;
        float reach = mirror_cube_reach(point_probe);
        before = at;
        at = next;
        found = point_probe;
        found_sq = reach;
        farthest = max(farthest, reach);
        is_valid = true;
        float past = dot(found, found) - reach;
        [branch]
        if (past > 0.0f && was <= 0.0f) {
            float3 half_step = along * (at - before) * 0.5f;
            float3 refined = found - half_step;
            [unroll]
            for (int refine_idx = 0; refine_idx < MIRROR_CUBE_REFINE; refine_idx++) {
                float refined_reach = mirror_cube_reach(refined);
                half_step *= 0.5f;
                refined += dot(refined, refined) > refined_reach ? -half_step : half_step;
            }
            float refined_sq = mirror_cube_reach(refined);
            float refined_dist = sqrt(refined_sq);
            // Between the texels of an edge against what lies far behind it (a roof against the sky) the distances are
            // blended into ones nothing has, which a ray passing it met as a surface, a speckle along every edge: a
            // crossing far from the distance of its own texel is no surface. Small steps (a window's recess) are
            float texel_dist = s_mirror_cube_dist.SampleLevel(smp_nofilter, refined, 0.0f).x;
            bool is_on_surface = abs(length(refined) - refined_dist) < 0.25f + 0.02f * refined_dist && abs(refined_dist - texel_dist) < 0.25f * texel_dist;
            // Off the surface the crossing is an edge the ray passed behind, as the eye sees it, not one it met: it goes on.
            // Stopped there, every edge it mirrored, a roof against the sky, came out wider than it is
            [branch]
            if (is_on_surface) {
                found = refined;
                found_sq = refined_sq;
                hit = length(refined - origin);
                is_hit = true;
                break;
            }
        }
        was = past;
    }
    [branch]
    if (!is_valid) {
        return 0.0f;
    }
    float share = is_hit ? 1.0f : 1.0f - saturate(2.5f * sqrt(farthest) * fog_params.w + fog_params.x);
    open = 1.0f - share;
    float fog = saturate(sqrt(found_sq) * fog_params.w + fog_params.x);
    float4 held = s_mirror_cube.SampleLevel(smp_linear, mirror_oct(found), lod);
    return float4(min(held.rgb / max(held.a, 1e-4f), 20.0f), share * (1.0f - fog * fog) * held.a);
}

float4 mirror_cube(float3 origin, float3 along, float noise, float lod, out float open)
{
    float hit;
    return mirror_cube_from(origin, along, 0.0f, noise, lod, open, hit);
}

// returns what the cube shows along a ray from the eye, which is what it holds there, at the level of the map filtered
// for this roughness, .a - its share: for the HUD, which stands at the eye
float4 mirror_cube_at_eye(float3 along, float roughness, out float open)
{
    float4 held = s_mirror_cube.SampleLevel(smp_linear, mirror_oct(along), roughness * MIRROR_IBL_LAST);
    open = 1.0f - held.a;
    return float4(min(held.rgb / max(held.a, 1e-4f), 20.0f), held.a);
}

#endif

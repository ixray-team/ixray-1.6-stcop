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
// The level's LOD cards the main view shows: in the frustum, in a visited sector, inside the fog, not hidden by the depth
// pyramid where the view has one, and with the group small enough on screen for its card to stand in. A kept card
// writes what the graph's card draw (r_dsgraph_render_lods) puts into its vertex record: the two views it blends and
// how far it has faded in
#include "common\scene_tables.hlsli"

cbuffer SceneViewBlock : register(b0)
{
    SceneView view;
}

StructuredBuffer<SceneCard> g_cards : register(t0);
Texture2D<float> g_hiz : register(t1);
RWBuffer<uint> g_args : register(u0); // a draw of six vertices an instance, an instance a kept card
RWBuffer<uint> g_visible : register(u1); // SCENE_CARD_WORDS a kept card, from second.w

#include "common\scene_occlusion.hlsli"

bool is_shown(SceneCard card, out float ssa)
{
    float3 offset = card.sphere.xyz - view.camera.xyz;
    float distance_sq = dot(offset, offset) + 0.00001f;
    ssa = card.sphere.w / distance_sq * card.lod_factor;

    if (ssa >= view.lod.y || ssa < view.thresholds.x || distance_sq > (view.camera.w + card.sphere.w) * (view.camera.w + card.sphere.w)) {
        return false;
    }

    if (card.sector < SCENE_VIEW_SECTORS && !(view.sectors[card.sector >> 7][(card.sector >> 5) & 3u] & (1u << (card.sector & 31u)))) {
        return false;
    }

    for (uint plane = 0; plane < view.counts.z; ++plane) {
        if (dot(view.planes[plane].xyz, card.sphere.xyz) + view.planes[plane].w > card.sphere.w) {
            return false;
        }
    }

    return !(view.counts.y & SCENE_VIEW_UNSEEN) || !sphere_occluded(g_hiz, card.sphere);
}

[numthreads(64, 1, 1)]
void main(uint3 id : SV_DispatchThreadID)
{
    if (id.x == 0u) {
        g_args[0] = 6u;
    }

    if (id.x >= view.second.z) {
        return;
    }

    SceneCard card = g_cards[id.x];
    float ssa;

    if (!is_shown(card, ssa)) {
        return;
    }

    // The two views that face the eye most, and how far it is from the second towards the first
    float3 to_card = normalize(card.sphere.xyz - view.camera.xyz);
    float facing[8];
    uint best = 0u;

    for (uint view_idx = 0u; view_idx < 8u; ++view_idx) {
        facing[view_idx] = dot(to_card, card.facets[view_idx].xyz);
        best = facing[view_idx] > facing[best] ? view_idx : best;
    }

    uint next = best == 0u ? 1u : 0u;

    for (uint view_idx = 0u; view_idx < 8u; ++view_idx) {
        next = view_idx != best && facing[view_idx] > facing[next] ? view_idx : next;
    }

    float third = -2.0f;

    for (uint view_idx = 0u; view_idx < 8u; ++view_idx) {
        third = view_idx != best && view_idx != next ? max(third, facing[view_idx]) : third;
    }

    float blend = 0.5f + 0.5f * (1.0f - (facing[next] - third) / max(facing[best] - third, 1e-6f));
    float fade = 1.0f - (ssa - view.lod.x) / max(view.lod.y - view.lod.x, 1e-7f);
    uint tf = best | (next << 3) | (uint(clamp(floor(fade * 255.0f), 0.0f, 255.0f)) << 8) | (uint(clamp(floor(blend * 255.5f), 0.0f, 255.0f)) << 16);

    uint slot;
    InterlockedAdd(g_args[1], 1u, slot);
    uint at = view.second.w + slot * SCENE_CARD_WORDS;
    float3 shift = to_card * (-0.5f * card.sphere.w);
    g_visible[at] = asuint(shift.x);
    g_visible[at + 1u] = asuint(shift.y);
    g_visible[at + 2u] = asuint(shift.z);
    g_visible[at + 3u] = card.vertices;
    g_visible[at + 4u] = tf;
    g_visible[at + 5u] = card.material;
}

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
// The first level of the scene's cull: of the runs of clusters whose sphere is in the view, the clusters of the detail
// the view draws, listed one after another. The cluster pass (scene_cull) is dispatched over the lists, every thread
// of it on a cluster that may be drawn, and never reads the rest of the level

#include "common\scene_tables.hlsli"

cbuffer SceneViewBlock : register(b0)
{
    SceneView view;
}

StructuredBuffer<SceneRange> g_ranges : register(t0);
// Past the cluster pass's outputs: it reads the chunks in the same command list, and the D3D11 backend of NRI then
// unbinds the slot they were written through, whatever that pass has put there since
RWBuffer<uint> g_chunks : register(u5);
RWBuffer<uint> g_groups : register(u6); // the groups of the cluster pass over a list: x, 1, 1; from word 4 those of the cut-out list
Texture2D<float> g_hiz : register(t1);
StructuredBuffer<SceneClusterLod> g_lods : register(t2);

#include "common\scene_occlusion.hlsli"

// The place of count entries in a list, and the groups that reach them
uint take_entries(uint list, uint count)
{
    uint slot;
    InterlockedAdd(g_chunks[list], count, slot);
    InterlockedMax(g_groups[list * 4u], (slot + count + SCENE_CHUNK_CLUSTERS - 1u) / SCENE_CHUNK_CLUSTERS);
    return slot;
}

// True for the one detail of its place the view wants: an error that would show a pixel wide is too much
bool detail_fits(uint index)
{
    SceneClusterLod lod = g_lods[index];

    if (view.detail.w <= 0.0f) {
        return lod.errors.x == 0.0f;
    }

    float own = lod.errors.x * view.detail.w / max(distance(lod.self.xyz, view.detail.xyz) - lod.self.w, 0.1f);
    float coarser = lod.errors.y * view.detail.w / max(distance(lod.parent.xyz, view.detail.xyz) - lod.parent.w, 0.1f);
    return own <= 1.0f && coarser > 1.0f;
}

[numthreads(64, 1, 1)]
void main(uint3 thread_id : SV_DispatchThreadID)
{
    uint range_count, stride;
    g_ranges.GetDimensions(range_count, stride);

    if (thread_id.x == 0u) {
        g_groups[1] = 1u;
        g_groups[2] = 1u;
        g_groups[5] = 1u;
        g_groups[6] = 1u;

        // The frame's dynamic meshes follow the level's clusters, as the cluster pass numbers them
        uint dynamic_count = (view.counts.y & ~SCENE_VIEW_MODES) >> SCENE_VIEW_DYNAMIC_SHIFT;

        if (dynamic_count != 0u) {
            uint place = 2u + take_entries(0u, dynamic_count);

            for (uint mesh_idx = 0u; mesh_idx < dynamic_count; mesh_idx++) {
                g_chunks[place + mesh_idx] = view.counts.x + mesh_idx;
            }
        }
    }

    // A view of the dynamic meshes alone has no clusters of the level
    if (thread_id.x >= range_count || view.counts.x == 0u) {
        return;
    }

    SceneRange range = g_ranges[thread_id.x];

    for (uint plane_idx = 0u; plane_idx < view.counts.z; plane_idx++) {
        if (dot(view.planes[plane_idx].xyz, range.bounds.xyz) + view.planes[plane_idx].w > range.bounds.w) {
            return;
        }
    }

    // A run of levels of detail the view does not draw: all too coarse for it, or all finer than it needs. Its
    // clusters are tested the same way below, with their own spheres in place of the run's bounds
    float away = distance(range.bounds.xyz, view.detail.xyz);

    if (view.detail.w <= 0.0f ? range.detail.x != 0.0f : (range.detail.x * view.detail.w > max(away + range.detail.y, 0.1f) || range.detail.z * view.detail.w <= max(away - range.detail.w, 0.1f))) {
        return;
    }

    // A mesh's clusters are all its levels of detail, of which the view wants one: the rest are left here, where
    // each took a thread of the cluster pass to be dropped. One that is not wanted keeps the mark it had of being
    // seen: when it is wanted again it is drawn first once, and tested then
    uint fit[2] = { 0u, 0u };

    for (uint cluster_idx = 0u; cluster_idx < range.count; cluster_idx++) {
        if (detail_fits(range.first + cluster_idx)) {
            fit[cluster_idx >> 5u] |= 1u << (cluster_idx & 31u);
        }
    }

    uint count = countbits(fit[0]) + countbits(fit[1]);

    if (count == 0u) {
        return;
    }

    // A run the depth of the frame's first part hides as a whole: its clusters are only marked, so that none of
    // them is drawn first the next frame
    bool is_hidden = (view.counts.y & SCENE_VIEW_UNSEEN) != 0u && sphere_occluded(g_hiz, range.bounds);
    // What is cut out by its texture a view with pipeline lists takes in bands of distance, from a list of its own
    bool is_cut = !is_hidden && (view.counts.y & SCENE_VIEW_BINS) != 0u && (range.flags & SCENE_CHUNK_ALPHA) != 0u;
    uint slot = take_entries(is_cut ? 1u : 0u, count);
    uint size;
    g_chunks.GetDimensions(size);

    for (uint fit_idx = 0u; fit_idx < range.count; fit_idx++) {
        if (fit[fit_idx >> 5u] & (1u << (fit_idx & 31u))) {
            g_chunks[is_cut ? size - 1u - slot : 2u + slot] = (range.first + fit_idx) | (is_hidden ? SCENE_CHUNK_HIDDEN : 0u);
            slot++;
        }
    }
}

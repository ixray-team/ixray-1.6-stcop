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
// The solid list of a shadow view as meshlets, a group an entry of the list (scene_depth.vs.hlsl draws the same list
// as triangles): a cluster's vertices that stand in one place are one vertex here, a run of a moving model has its
// corners as they are
#include "common\scene_tables.hlsli"
#include "common\scene_depth.hlsli"

#define MESH_THREADS 64u
#define MESH_VERTICES (SCENE_CLUSTER_TRIANGLES * 3u)

struct mesh_vertex {
    float4 position : SV_Position;
};

float4 model_position(SceneModel model, uint corner_idx)
{
    uint page = model.source & SCENE_DYNAMIC_PAGE_MASK;
    uint vertex_idx = model.vertices + LoadIndex(page, model.index_start + corner_idx);
    SceneVertex vertex;

    [branch] if (model.source & SCENE_DYNAMIC_SKINNED) {
        vertex = g_SkinnedVertices[vertex_idx];
    } else {
        vertex = LoadVertex(page, vertex_idx);
    }

    return mul(view.view_projection, float4(mul(model.world, float4(vertex.P, 1.0f)), 1.0f));
}

float4 cluster_position(SceneCluster cluster, uint vertex_idx)
{
    float3 position = scene_page_position(cluster.vertices, vertex_idx);
    uint transform = (cluster.triangles >> SCENE_CLUSTER_TRANSFORM_SHIFT) & SCENE_CLUSTER_TRANSFORM_MASK;

    [branch] if (transform != 0u) {
        float3x4 world = g_Transforms[transform].world;
        position = mul(world, float4(position, 1.0f));

        [branch] if (cluster.triangles & SCENE_CLUSTER_WAVE) {
            float sway = (position.y - world._24) * calc_cyclic(view.wave.w + dot(position, view.wave.xyz));
            float weight = float(asint(scene_page_baked(cluster.vertices, vertex_idx) << 16) >> 16) * view.wind.w;
            position.xz += view.wind.xz * (sway * weight);
        }
    }

    return mul(view.view_projection, float4(position, 1.0f));
}

[outputtopology("triangle")]
[numthreads(MESH_THREADS, 1, 1)]
void main(uint3 group_id : SV_GroupID, uint3 inside : SV_GroupThreadID, out vertices mesh_vertex vertices[MESH_VERTICES], out indices uint3 triangles[SCENE_CLUSTER_TRIANGLES])
{
    uint entry = g_Visible[group_id.x];
    bool is_model = (entry & SCENE_VISIBLE_DYNAMIC) != 0u;
    SceneModel model = g_Models[view.second.z + (entry & (SCENE_MODELS - 1u))];
    SceneCluster cluster = g_Clusters[is_model ? 0u : entry];
    uint first = ((entry >> SCENE_VISIBLE_MODEL_SHIFT) & SCENE_MODEL_RUNS) * MESH_VERTICES;
    uint places = scene_page_places(cluster.index_start, cluster.triangles & SCENE_CLUSTER_COUNT_MASK);
    uint triangle_count = cluster.triangles & SCENE_CLUSTER_COUNT_MASK;
    uint vertex_count = scene_page_corner(places, 0u);

    if (is_model) {
        triangle_count = min(model.triangle_count - min(model.triangle_count, first / 3u), SCENE_CLUSTER_TRIANGLES);
        vertex_count = triangle_count * 3u;
    }

    SetMeshOutputCounts(vertex_count, triangle_count);

    for (uint vertex_idx = inside.x; vertex_idx < vertex_count; vertex_idx += MESH_THREADS) {
        [branch] if (is_model) {
            vertices[vertex_idx].position = model_position(model, first + vertex_idx);
        } else {
            vertices[vertex_idx].position = cluster_position(cluster, scene_page_corner(places, 1u + vertex_idx));
        }
    }

    if (inside.x < triangle_count) {
        uint at = 1u + vertex_count + inside.x * 3u;
        triangles[inside.x] = is_model ? inside.x * 3u + uint3(0u, 1u, 2u) : uint3(scene_page_corner(places, at), scene_page_corner(places, at + 1u), scene_page_corner(places, at + 2u));
    }
}

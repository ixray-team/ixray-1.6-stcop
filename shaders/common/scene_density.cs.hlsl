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
// Run once for a level: how much of its texture a cluster lays over a metre (uv units a metre, by the areas of its
// triangles). The cull asks for the heads of a visible cluster's textures by it and the cluster's distance, in the
// place of every pixel asking for its own

#include "common\scene_tables.hlsli"
#include "common\vertex.hlsli"

StructuredBuffer<SceneCluster> g_clusters : register(t0);
StructuredBuffer<SceneTransform> g_transforms : register(t1);
Buffer<uint> g_corners : register(t10);
RWBuffer<float> g_density : register(u0);

#define SCENE_PAGE_WORDS g_corners
#include "common\scene_pages.hlsli"

[numthreads(64, 1, 1)]
void main(uint3 thread_id : SV_DispatchThreadID)
{
    uint cluster_count, stride;
    g_clusters.GetDimensions(cluster_count, stride);

    if (thread_id.x >= cluster_count) {
        return;
    }

    SceneCluster cluster = g_clusters[thread_id.x];
    uint triangle_count = cluster.triangles & SCENE_CLUSTER_COUNT_MASK;
    float uv_area = 0.0f;
    float area = 0.0f;

    for (uint triangle_idx = 0u; triangle_idx < triangle_count; triangle_idx++) {
        float3 positions[3];
        float2 uvs[3];

        for (uint corner_idx = 0u; corner_idx < 3u; corner_idx++) {
            // A cluster of the level is a page, one of a grass model its mesh in the heap
            [branch] if (cluster.extent.w != 0.0f) {
                uint vertex_idx = scene_page_corner(cluster.index_start, triangle_idx * 3u + corner_idx);
                positions[corner_idx] = scene_page_position(cluster.vertices, vertex_idx);
                uvs[corner_idx] = scene_page_uv(cluster.vertices, vertex_idx);
            } else {
                PackedVertex vertex = LoadPage(cluster.vertices >> 24, (cluster.vertices & 0xFFFFFFu) + g_corners[cluster.index_start + triangle_idx * 3u + corner_idx]);
                positions[corner_idx] = vertex.P;
                uvs[corner_idx] = DecodeUV(vertex.TF, vertex.UV);
            }
        }

        float2 uv_a = uvs[1] - uvs[0];
        float2 uv_b = uvs[2] - uvs[0];
        area += length(cross(positions[1] - positions[0], positions[2] - positions[0]));
        uv_area += abs(uv_a.x * uv_b.y - uv_a.y * uv_b.x);
    }

    // A tree is drawn by its transform: only its scale changes the measure
    uint transform = (cluster.triangles >> SCENE_CLUSTER_TRANSFORM_SHIFT) & SCENE_CLUSTER_TRANSFORM_MASK;
    float3x4 world = g_transforms[transform].world;
    float scale = transform != 0u ? length(float3(world._11, world._21, world._31)) : 1.0f;

    // No area: 0, which asks at any distance
    g_density[thread_id.x] = area > 0.0f ? sqrt(uv_area / area) / max(scale, 0.0001f) : 0.0f;
}

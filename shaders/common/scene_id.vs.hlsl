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
// The id pass of the level's clusters: depth, and the triangle a pixel shows. Drawn like a forward bin, an instance
// a visible cluster, by the forward frame's own tables
#include "common\fixed_cb.hlsli"
#include "common\scene_tables.hlsli"

cbuffer SceneForwardBlock : register(b8)
{
    uint4 forward_draw; // x: where the draw's part of the visible list starts, z: not 0 for a bin of terrain
    float4 forward_ambient;
    float4 forward_grass;
}

StructuredBuffer<SceneCluster> g_clusters : register(t0);
StructuredBuffer<SceneTransform> g_transforms : register(t1);
StructuredBuffer<SceneInstance> g_instances : register(t2);
StructuredBuffer<SceneMaterial> g_materials : register(t3);
Buffer<uint> g_words : register(t10);
Buffer<uint> g_visible : register(t11);

#define SCENE_PAGE_WORDS g_words
#include "common\scene_pages.hlsli"
#define SCENE_ID_TRANSFORMS g_transforms
#include "common\scene_id.hlsli"
#include "common\scene_gbuffer.hlsli"

struct id_vertex {
    nointerpolation uint id : TEXCOORD0;
#ifdef USE_AREF
    float2 uv : TEXCOORD1;
    nointerpolation uint material : TEXCOORD2;
#endif
    float4 position : SV_Position;
};

id_vertex main(uint corner_idx : SV_VertexID, uint slot : SV_InstanceID)
{
    id_vertex output = (id_vertex)0;
    // Past the cluster's last triangle: behind the eye
    output.position = float4(0.0f, 0.0f, -1.0f, 1.0f);
    uint cluster_idx = g_visible[forward_draw.x + slot];
    SceneCluster cluster = g_clusters[cluster_idx];

    [branch] if (corner_idx >= (cluster.triangles & SCENE_CLUSTER_COUNT_MASK) * 3u) {
        return output;
    }

    uint vertex_idx = scene_page_corner(cluster.index_start, corner_idx);
    output.position = mul(m_VP, float4(scene_id_position(cluster, vertex_idx), 1.0f));
    output.position.xy += m_taa_jitter.xy * output.position.w;
    // A tree by its instance: the material's record is thirteen times the size, and a vertex needs nothing else of it
    SceneInstance instance = g_instances[cluster.instance];
    output.id = scene_id_pack(cluster_idx, corner_idx / 3u, forward_draw.z != 0u ? SceneClass_Terrain : ((instance.flags & SCENE_INSTANCE_TREE) ? SceneClass_Foliage : ((instance.flags & SCENE_INSTANCE_STEEP) ? SCENE_ID_CLASS_STEEP : SceneClass_Plain)));
#ifdef USE_AREF
    output.uv = scene_page_uv(cluster.vertices, vertex_idx);
    output.material = instance.material;
#endif
    return output;
}

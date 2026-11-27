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
// A forward Surface shader drawn from the scene's clusters (SURFACE_CLUSTER): one instanced draw per pipeline, an
// instance a visible cluster. The vertex stage pulls its vertex by cluster, the pixel stage takes its material's
// maps from the texture buckets. Without the define a map is the texture the pass binds.

#ifndef SURFACE_CLUSTER
#define SURFACE_MAP(name, sampler, uv) name.Sample(sampler, uv)
#define SURFACE_MAP_AT(name, sampler, uv, uv_dx, uv_dy) name.Sample(sampler, uv)
#define SURFACE_ENV(direction) s_env.Sample(smp_rtlinear, direction)
#define SURFACE_ALPHA(uv) s_base.Sample(smp_base, uv).a
#define SURFACE_ALPHA_GRAD(uv, uv_dx, uv_dy) s_base.SampleGrad(smp_base, uv, uv_dx, uv_dy).a
#else
#include "common\scene_tables.hlsli"

#undef PULL_INPUT
#define PULL_INPUT uint VertexId : SV_VertexID, uint DrawId : SV_InstanceID

cbuffer SceneForwardBlock : register(b8)
{
    uint4 forward_draw; // x: where the draw's part of the visible list starts (grass: the draw, see pull_grass), y: grass: the model's bin
    float4 forward_ambient; // the weather's ambient, as a tree's baked light takes it
    float4 forward_grass; // x: how much a blade's sway adds to its light, y: the light it has at rest
}

// Vertex stage
StructuredBuffer<SceneCluster> g_forward_clusters : register(t0);
StructuredBuffer<SceneTransform> g_forward_transforms : register(t1);
StructuredBuffer<SceneInstance> g_forward_instances : register(t2);
StructuredBuffer<SceneMaterial> g_forward_materials : register(t3);
StructuredBuffer<SceneTreeLight> g_forward_tree_lights : register(t4);
StructuredBuffer<SceneGrassItem> g_forward_items : register(t5);
StructuredBuffer<SceneModel> g_forward_models : register(t6);
StructuredBuffer<PackedVertex> g_forward_skinned : register(t7);
Buffer<uint> g_forward_corners : register(t10); // the level's clusters as pages, the grass models' indices
Buffer<uint> g_forward_visible : register(t11);
Buffer<uint> g_forward_grass : register(t12); // a word a visible blade, see scene_grass_cull
// The index data of the geometry heap's pages, which the runs of a model read
Buffer<uint> g_forward_indices0 : register(t13);
Buffer<uint> g_forward_indices1 : register(t14);
Buffer<uint> g_forward_indices2 : register(t15);
Buffer<uint> g_forward_indices3 : register(t16);
Buffer<uint> g_forward_indices4 : register(t17);

// The view of the frame's grass cull: its wind and the waves of its variants
cbuffer SceneGrassBlock : register(b10)
{
    SceneGrassView grass;
}

// Pixel stage
#ifdef __hlsl_dx_compiler
Texture2DArray g_buckets[SCENE_TEXTURE_BUCKETS] : register(t6);
#endif
Texture2DArray g_bucket0 : register(t6);
Texture2DArray g_bucket1 : register(t7);
Texture2DArray g_bucket2 : register(t8);
Texture2DArray g_bucket3 : register(t9);
Texture2DArray g_bucket4 : register(t10);
Texture2DArray g_bucket5 : register(t11);
Texture2DArray g_bucket6 : register(t12);
Texture2DArray g_bucket7 : register(t13);
Texture2DArray g_bucket8 : register(t14);
Texture2DArray g_bucket9 : register(t15);
Texture2DArray g_bucket10 : register(t16);
Texture2DArray g_bucket11 : register(t17);
Texture2DArray g_bucket12 : register(t18);
Texture2DArray g_bucket13 : register(t19);
Texture2DArray g_bucket14 : register(t20);
Texture2DArray g_bucket15 : register(t21);
StructuredBuffer<SceneMaterial> g_surface_materials : register(t22);

// The heads of the streamed textures, and where a pixel says which of them it needs
cbuffer SceneHeadsBlock : register(b9)
{
    uint4 resolve_frame; // x: the frame, what is written as feedback
}

// Per streamed texture its head slice, absent while not loaded. Records, not a block: one word of 8 KB read by index
// from a constant block cost the terrain's depth pass five times its time
StructuredBuffer<uint> resolve_heads : register(t57);

// No pixel asks for its textures: the cull does, for the clusters it shows (AskHeads of scene_cull). A store in the
// pixel shader cost the colour pass 0.14 ms whether one pixel in sixteen made it or all
#define SCENE_BUCKETS_QUIET

#define smp_scene smp_base

// The maps the buckets have no place for, as they are: a file of another format or size, or one past a full bucket. Only
// the pipelines of materials that have one read them (SCENE_BUCKETS_BOUND)
#ifdef SCENE_BUCKETS_BOUND
#ifdef __hlsl_dx_compiler
Texture2D g_bound[SCENE_TEXTURE_BOUND] : register(t64);

float4 sample_bound(uint bound, float2 uv, float2 uv_dx, float2 uv_dy)
{
    return g_bound[NonUniformResourceIndex(bound)].SampleGrad(smp_scene, uv, uv_dx, uv_dy);
}
#else
Texture2D g_bound0 : register(t64);
Texture2D g_bound1 : register(t65);
Texture2D g_bound2 : register(t66);
Texture2D g_bound3 : register(t67);
Texture2D g_bound4 : register(t68);
Texture2D g_bound5 : register(t69);
Texture2D g_bound6 : register(t70);
Texture2D g_bound7 : register(t71);
Texture2D g_bound8 : register(t72);
Texture2D g_bound9 : register(t73);
Texture2D g_bound10 : register(t74);
Texture2D g_bound11 : register(t75);
Texture2D g_bound12 : register(t76);
Texture2D g_bound13 : register(t77);
Texture2D g_bound14 : register(t78);
Texture2D g_bound15 : register(t79);

float4 sample_bound(uint bound, float2 uv, float2 uv_dx, float2 uv_dy)
{
    switch (bound) {
    case 1u:
        return g_bound1.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 2u:
        return g_bound2.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 3u:
        return g_bound3.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 4u:
        return g_bound4.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 5u:
        return g_bound5.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 6u:
        return g_bound6.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 7u:
        return g_bound7.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 8u:
        return g_bound8.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 9u:
        return g_bound9.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 10u:
        return g_bound10.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 11u:
        return g_bound11.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 12u:
        return g_bound12.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 13u:
        return g_bound13.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 14u:
        return g_bound14.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    case 15u:
        return g_bound15.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    default:
        return g_bound0.SampleGrad(smp_scene, uv, uv_dx, uv_dy);
    }
}
#endif
#endif

#include "common\scene_buckets.hlsli"

#define SCENE_PAGE_WORDS g_forward_corners
#include "common\scene_pages.hlsli"

// The alpha of every alpha-tested base texture, a slice a material: what the depth pass cuts its pixels by
Texture2DArray<float> g_forward_alpha : register(t24);

struct cluster_vertex {
    PulledVertex vertex;
    uint material;
    uint cluster;
    uint instance;
    uint transform; // slot of the scene's table, 0 for what stands where it was built
    float detail_scale;
    bool is_culled; // past the cluster's last triangle
};

cluster_vertex pull_cluster(uint corner_idx, uint slot)
{
    cluster_vertex pulled = (cluster_vertex)0;
    uint cluster_idx = g_forward_visible[forward_draw.x + slot];
    SceneCluster cluster = g_forward_clusters[cluster_idx];
    pulled.is_culled = corner_idx >= (cluster.triangles & SCENE_CLUSTER_COUNT_MASK) * 3u;

    [branch] if (pulled.is_culled) {
        return pulled;
    }

    pulled.vertex = scene_page_vertex(cluster.vertices, scene_page_corner(cluster.index_start, corner_idx));
    pulled.material = g_forward_instances[cluster.instance].material;
    pulled.cluster = cluster_idx;
    pulled.instance = cluster.instance;
    pulled.transform = (cluster.triangles >> SCENE_CLUSTER_TRANSFORM_SHIFT) & SCENE_CLUSTER_TRANSFORM_MASK;
    pulled.detail_scale = g_forward_materials[pulled.material].detail_scale;
    return pulled;
}

uint forward_index(uint page, uint idx)
{
    [branch] if (page == 1u) {
        return g_forward_indices1[idx];
    }
    [branch] if (page == 2u) {
        return g_forward_indices2[idx];
    }
    [branch] if (page == 3u) {
        return g_forward_indices3[idx];
    }
    [branch] if (page == 4u) {
        return g_forward_indices4[idx];
    }
    return g_forward_indices0[idx];
}

struct model_vertex {
    PackedVertex packed;
    SceneModel record;
    uint model;
    bool is_culled; // past the run's last triangle
};

// A corner of a run of a model the forward frame took from the graph: the model's own index data, its vertices in
// their heap page or as the skinning pass left them
model_vertex pull_model(uint corner_idx, uint slot)
{
    model_vertex pulled = (model_vertex)0;
    uint entry = g_forward_visible[forward_draw.x + slot];
    pulled.model = entry & (SCENE_MODELS - 1u);
    pulled.record = g_forward_models[pulled.model];
    uint offset = ((entry >> SCENE_VISIBLE_MODEL_SHIFT) & SCENE_MODEL_RUNS) * SCENE_CLUSTER_TRIANGLES * 3u + corner_idx;
    pulled.is_culled = offset >= pulled.record.triangle_count * 3u;

    [branch] if (pulled.is_culled) {
        return pulled;
    }

    uint page = pulled.record.source & SCENE_DYNAMIC_PAGE_MASK;
    uint vertex_idx = pulled.record.vertices + forward_index(page, pulled.record.index_start + offset);

    [branch] if (pulled.record.source & SCENE_DYNAMIC_SKINNED) {
        pulled.packed = g_forward_skinned[vertex_idx];
    } else {
        pulled.packed = LoadPage(page, vertex_idx);
    }

    return pulled;
}

// The material of the pixel, set by the entry before anything samples. Its record is read where a field is used:
// held whole, the terrain's colour pass copied all of it a pixel and took half as long again
static uint surface_material_idx;
#define surface_material g_surface_materials[surface_material_idx]

float4 surface_map(uint tail, uint head, uint info, float2 uv)
{
    return sample_map(tail, head, info, uv, ddx(uv), ddy(uv), 16.0f);
}

#define SURFACE_MAP(name, sampler, uv) surface_map_##name(uv)
// The same with the slopes handed in, for a map read where only some materials have it
#define SURFACE_MAP_AT(name, sampler, uv, uv_dx, uv_dy) surface_at_##name(uv, uv_dx, uv_dy)
#define SURFACE_ALPHA(uv) surface_alpha(uv, ddx(uv), ddy(uv))
#define SURFACE_ALPHA_GRAD(uv, uv_dx, uv_dy) surface_alpha(uv, uv_dx, uv_dy)

// returns the alpha the depth pass cuts a pixel by: from the alpha array where the material has a slice there, else
// from its base texture. SURFACE_ALPHA_MIP_SCALE: what a mip of the array adds to it, for cuts thinner than a texel
// of the far mips, which those mips average away
float surface_alpha(float2 uv, float2 uv_dx, float2 uv_dy)
{
    [branch] if (surface_material.alpha != SCENE_TEXTURE_ABSENT) {
        float alpha = g_forward_alpha.SampleGrad(smp_base, float3(uv, float(surface_material.alpha)), uv_dx, uv_dy);
#ifdef SURFACE_ALPHA_MIP_SCALE
        float3 size;
        g_forward_alpha.GetDimensions(size.x, size.y, size.z);
        float2 step_x = uv_dx * size.xy;
        float2 step_y = uv_dy * size.xy;
        alpha *= 1.0f + max(0.5f * log2(max(dot(step_x, step_x), dot(step_y, step_y))), 0.0f) * SURFACE_ALPHA_MIP_SCALE;
#endif
        return alpha;
    }

    return sample_map(surface_material.base, surface_material.base_head, surface_material.base_info, uv, uv_dx, uv_dy, 16.0f).a;
}

float4 surface_map_s_base(float2 uv)
{
    return surface_map(surface_material.base, surface_material.base_head, surface_material.base_info, uv);
}

float4 surface_map_s_bump(float2 uv)
{
    return surface_map(surface_material.bump, surface_material.bump_head, surface_material.bump_info, uv);
}

float4 surface_map_s_bumpX(float2 uv)
{
    return surface_map(surface_material.bump_x, surface_material.bump_x_head, surface_material.bump_x_info, uv);
}

float4 surface_map_s_detail(float2 uv)
{
    return surface_map(surface_material.detail, surface_material.detail_head, surface_material.detail_info, uv);
}

float4 surface_at_s_base(float2 uv, float2 uv_dx, float2 uv_dy)
{
    return sample_map(surface_material.base, surface_material.base_head, surface_material.base_info, uv, uv_dx, uv_dy, 16.0f);
}

float4 surface_at_s_detail(float2 uv, float2 uv_dx, float2 uv_dy)
{
    return sample_map(surface_material.detail, surface_material.detail_head, surface_material.detail_info, uv, uv_dx, uv_dy, 16.0f);
}

float4 surface_at_s_detailBump(float2 uv, float2 uv_dx, float2 uv_dy)
{
    return sample_map(surface_material.detail_bump, surface_material.detail_bump_head, surface_material.detail_bump_info, uv, uv_dx, uv_dy, 16.0f);
}

float4 surface_at_s_detailBumpX(float2 uv, float2 uv_dx, float2 uv_dy)
{
    return sample_map(surface_material.detail_bump_x, surface_material.detail_bump_x_head, surface_material.detail_bump_x_info, uv, uv_dx, uv_dy, 16.0f);
}

float4 surface_at_s_hemi(float2 uv, float2 uv_dx, float2 uv_dy)
{
    return sample_map(surface_material.hemi, surface_material.hemi_head, surface_material.hemi_info, uv, uv_dx, uv_dy, 16.0f);
}

float4 surface_at_s_bump(float2 uv, float2 uv_dx, float2 uv_dy)
{
    return sample_map(surface_material.bump, surface_material.bump_head, surface_material.bump_info, uv, uv_dx, uv_dy, 16.0f);
}

float4 surface_at_s_lmap(float2 uv, float2 uv_dx, float2 uv_dy)
{
    return sample_map(surface_material.lightmap, surface_material.lightmap_head, surface_material.lightmap_info, uv, uv_dx, uv_dy, 16.0f);
}

float4 surface_map_s_hemi(float2 uv)
{
    return surface_map(surface_material.hemi, surface_material.hemi_head, surface_material.hemi_info, uv);
}

float4 surface_map_s_lmap(float2 uv)
{
    return surface_map(surface_material.lightmap, surface_material.lightmap_head, surface_material.lightmap_info, uv);
}

// The terrain's mask and the maps of its four layers
#define SURFACE_TERRAIN_MAP(name, slot) float4 surface_map_##name(float2 uv) { return surface_map(surface_material.terrain[slot].x, surface_material.terrain[slot].y, surface_material.terrain[slot].z, uv); }

SURFACE_TERRAIN_MAP(s_mask, 0u)
SURFACE_TERRAIN_MAP(s_dt_r, SCENE_TERRAIN_COLOUR)
SURFACE_TERRAIN_MAP(s_dt_g, SCENE_TERRAIN_COLOUR + 1u)
SURFACE_TERRAIN_MAP(s_dt_b, SCENE_TERRAIN_COLOUR + 2u)
SURFACE_TERRAIN_MAP(s_dt_a, SCENE_TERRAIN_COLOUR + 3u)
SURFACE_TERRAIN_MAP(s_dn_r, SCENE_TERRAIN_BUMP)
SURFACE_TERRAIN_MAP(s_dn_g, SCENE_TERRAIN_BUMP + 1u)
SURFACE_TERRAIN_MAP(s_dn_b, SCENE_TERRAIN_BUMP + 2u)
SURFACE_TERRAIN_MAP(s_dn_a, SCENE_TERRAIN_BUMP + 3u)
SURFACE_TERRAIN_MAP(s_dn_rX, SCENE_TERRAIN_BUMP_X)
SURFACE_TERRAIN_MAP(s_dn_gX, SCENE_TERRAIN_BUMP_X + 1u)
SURFACE_TERRAIN_MAP(s_dn_bX, SCENE_TERRAIN_BUMP_X + 2u)
SURFACE_TERRAIN_MAP(s_dn_aX, SCENE_TERRAIN_BUMP_X + 3u)
// What an emissive surface gives off, in the place of the mask: such a surface is no terrain
SURFACE_TERRAIN_MAP(s_emission, 0u)

// The cubes the materials mirror, one array for all of them, in the place of the cube a pass binds
TextureCubeArray g_forward_env : register(t30);
#define SURFACE_ENV(direction) g_forward_env.Sample(smp_rtlinear, float4(direction, float(surface_material.env)))

// returns a map of one terrain layer, for the loops that skip the layers the mask leaves out of a pixel
float4 surface_layer_map(uint slot, float2 uv, float2 uv_dx, float2 uv_dy)
{
    uint4 map = surface_material.terrain[slot];
    return sample_map(map.x, map.y, map.z, uv, uv_dx, uv_dy, 16.0f);
}
#endif

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
// The pixel stage of the id pass: the id the vertex stage handed over. What is cut out by its texture is cut by the
// alpha array, a slice a material, or by its base texture where the material has no slice there
#include "common\fixed_cb.hlsli"
#include "common\common_samplers.hlsli"
#include "common\scene_tables.hlsli"

struct id_vertex {
    nointerpolation uint id : TEXCOORD0;
#ifdef USE_AREF
    float2 uv : TEXCOORD1;
    nointerpolation uint material : TEXCOORD2;
#endif
    float4 position : SV_Position;
};

#ifdef USE_AREF
Texture2DArray<float> g_alpha : register(t24);
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
StructuredBuffer<SceneMaterial> g_materials : register(t22);
StructuredBuffer<uint> resolve_heads : register(t57);

#define SCENE_BUCKETS_QUIET
#define smp_scene smp_base
#include "common\scene_buckets.hlsli"

float id_alpha(uint material_idx, float2 uv, float2 uv_dx, float2 uv_dy)
{
    uint slice = g_materials[material_idx].alpha;

    [branch] if (slice != SCENE_TEXTURE_ABSENT) {
        return g_alpha.SampleGrad(smp_base, float3(uv, float(slice)), uv_dx, uv_dy);
    }

    return sample_map(g_materials[material_idx].base, g_materials[material_idx].base_head, g_materials[material_idx].base_info, uv, uv_dx, uv_dy, 16.0f).a;
}

struct id_output {
    uint id : SV_Target0;
#ifdef USE_COARSE_ALPHA
    uint coverage : SV_Coverage;
#endif
};

id_output main(id_vertex input)
{
    id_output output;
    output.id = input.id;
#ifdef USE_COARSE_ALPHA
    // One run a block of two by two pixels: each pixel is cut by its own place in the texture, the four answers go
    // out as the block's coverage, whose lowest bit is the last pixel (surface_forward_entry.hlsli)
    float2 uv_dx = ddx(input.uv) * 0.5f;
    float2 uv_dy = ddy(input.uv) * 0.5f;
    output.coverage = 0u;

    [unroll] for (uint pixel_idx = 0u; pixel_idx < 4u; pixel_idx++) {
        float2 place = float2(pixel_idx & 1u, pixel_idx >> 1u) - 0.5f;
        float alpha = id_alpha(input.material, input.uv + uv_dx * place.x + uv_dy * place.y, uv_dx, uv_dy);
        output.coverage |= alpha > m_AlphaRef ? 8u >> pixel_idx : 0u;
    }

    if (output.coverage == 0u) {
        discard;
    }
#else
    if (id_alpha(input.material, input.uv, ddx(input.uv), ddy(input.uv)) <= m_AlphaRef) {
        discard;
    }
#endif
    return output;
}
#else
[earlydepthstencil]
uint main(id_vertex input) : SV_Target0
{
    return input.id;
}
#endif

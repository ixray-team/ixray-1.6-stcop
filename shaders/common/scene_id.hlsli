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
// The id a pixel of the level keeps of the triangle drawn there, and where that triangle's corners stand. The id
// pass and whatever reads its ids take a corner's place from here: the reader finds the pixel in the triangle by it,
// so the two must agree to the bit. The includer has scene_pages.hlsli, names the transforms
// (#define SCENE_ID_TRANSFORMS <a StructuredBuffer<SceneTransform>>) and has consts, wave and wind of CBPass
#ifndef SCENE_ID_HLSLI
#define SCENE_ID_HLSLI

// A cluster above, its triangle below it, the class of its material lowest; 0 is no triangle
#define SCENE_ID_CLUSTER_SHIFT 8u
#define SCENE_ID_TRIANGLE_SHIFT 2u
#define SCENE_ID_TRIANGLE_MASK 63u
#define SCENE_ID_CLASS_MASK 3u
// The class past those of a surface (SceneClass_): what is plain but for the height its texture is marched through
#define SCENE_ID_CLASS_STEEP 3u

uint scene_id_pack(uint cluster_idx, uint triangle_idx, uint material_class)
{
    return (cluster_idx + 1u) << SCENE_ID_CLUSTER_SHIFT | triangle_idx << SCENE_ID_TRIANGLE_SHIFT | material_class;
}

#ifdef SCENE_ID_TRANSFORMS
// returns where a vertex of a cluster stands in the world: a tree's by its transform, swayed by the wind
float3 scene_id_position(SceneCluster cluster, uint vertex_idx)
{
    precise float3 position = scene_page_position(cluster.vertices, vertex_idx);
    uint transform = (cluster.triangles >> SCENE_CLUSTER_TRANSFORM_SHIFT) & SCENE_CLUSTER_TRANSFORM_MASK;

    [branch] if (transform != 0u) {
        float3x4 world = SCENE_ID_TRANSFORMS[transform].world;
        position = mul(world, float4(position, 1.0f));

        [branch] if (cluster.triangles & SCENE_CLUSTER_WAVE) {
            float phase = 1.4142f * sin((wave.w + dot(position, wave.xyz)) * 3.14159f);
            float sway = (position.y - world._24) * (phase * phase - 1.0f);
            float weight = float(asint(scene_page_baked(cluster.vertices, vertex_idx) << 16) >> 16) * consts.x;
            position.xz += wind.xz * (sway * weight);
        }
    }

    return position;
}
#endif

#endif

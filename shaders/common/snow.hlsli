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
#ifndef SNOW_HLSLI
#define SNOW_HLSLI

// Snow on a surface, as a part of its material: what faces up under the open sky takes the material of snow
// before anything lights it. The includer has IXRayMaterial and the material helpers of common\material.hlsli
Texture2D s_snow_base : register(t31);
Texture2D s_snow_bump : register(t32);
Texture2D s_snow_bumpX : register(t33);

// The snow's texture repeats every five metres
#define SNOW_TILING 0.2f

// material: the surface, its normal in view space; snow takes it over where it lies. Level geometry only: models
// and foliage keep theirs. world_pos is the surface's point
void snow_cover(inout IXRayMaterial material, float3 world_pos)
{
    float3 normal = normalize(mul((float3x3)m_invV, material.Normal));
    float cover = smoothstep(0.2f, 0.3f, material.Hemi) * smoothstep(0.7f, 0.8f, normal.y);

    // The uv's steps across the pixel are taken before the branch: a sample inside one has none
    float2 uv = world_pos.xz * SNOW_TILING;
    float2 uv_across = ddx(uv);
    float2 uv_down = ddy(uv);

    [branch]
    if (cover <= 0.0f || material.MaterialID == OBJECT_ID || material.MaterialID == FOLIAGE_ID) {
        return;
    }

    IXRayMaterial snow = material;
    snow.Color = s_snow_base.SampleGrad(smp_base, uv, uv_across, uv_down);
#ifdef SNOW_PBR
    MaterialBump(snow, s_snow_bump.SampleGrad(smp_base, uv, uv_across, uv_down), s_snow_bumpX.SampleGrad(smp_base, uv, uv_across, uv_down), true, F0_BASE);
    MaterialFinish(snow, true, L_material.w);
#else
    MaterialBump(snow, s_snow_bump.SampleGrad(smp_base, uv, uv_across, uv_down), s_snow_bumpX.SampleGrad(smp_base, uv, uv_across, uv_down), false, F0_BASE);
    MaterialFinish(snow, false, L_material.w);
#endif

    // The snow's own normal map lies on the surface's normal
    float3 forward = abs(normal.z) < 0.999f ? float3(0.0f, 0.0f, 1.0f) : float3(1.0f, 0.0f, 0.0f);
    float3 tangent = normalize(cross(forward, normal));
    float3 bitangent = cross(normal, tangent);
    float3 mapped = tangent * snow.Normal.x + bitangent * snow.Normal.y + normal * snow.Normal.z;
    snow.Normal = normalize(mul((float3x3)m_V, mapped));

    material = IXRayMaterial_Lerp(material, snow, cover);
}

#endif

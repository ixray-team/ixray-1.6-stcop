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
// Fills one mip of one cube of the scene's environment array from a cube a material mirrors: a texel of a face takes
// the same place of the same face of the source, at the source's mip that matches, so no direction is worked out

Texture2DArray g_source : register(t0); // the six faces of the source
SamplerState smp_source : register(s0);
RWTexture2DArray<float4> g_env : register(u0); // the six faces of the cube in this mip

[numthreads(8, 8, 1)]
void main(uint3 id : SV_DispatchThreadID)
{
    uint3 size;
    g_env.GetDimensions(size.x, size.y, size.z);

    if (any(id.xy >= size.xy)) {
        return;
    }

    uint width, height, faces, levels;
    g_source.GetDimensions(0u, width, height, faces, levels);
    float level = clamp(log2(float(width) / float(size.x)), 0.0f, float(levels - 1u));
    // Within its face: the linear filter must not reach across the edge into the opposite side
    float texels = max(float(width) / exp2(floor(level)), 1.0f);
    float2 uv = clamp((float2(id.xy) + 0.5f) / float2(size.xy), 0.5f / texels, 1.0f - 0.5f / texels);
    g_env[id] = g_source.SampleLevel(smp_source, float3(uv, float(id.z)), level);
}

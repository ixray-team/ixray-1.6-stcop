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
#include "common\common.hlsli"
#include "common\thin_surface.hlsli"

Texture2D<float> s_contact : register(t27);

// returns the HUD's contact shadow of the pixel settled: the mean of the 4x4 pixels around it that belong to the
// same surface, by their depth and their normal. The trace shifts its ray by a pattern of 4x4 pixels, so the box
// holds every shift once; pixels of another surface would bring that surface's shadow or light across the edge
float ps_main(PSInputFullscreen input) : SV_Target
{
    int2 pixel = int2(input.hpos.xy);
    float depth = s_position.Load(int3(pixel, 0)).x;
    if (depth >= 0.02f) {
        return 1.0f;
    }

    float view_z = GbufferGetViewZ(depth);
    float3 normal = thin_unpack(s_thin.Load(int3(pixel, 0))).normal;
    float sum = 0.0f;
    float weight = 0.0f;

    for (int y = -1; y <= 2; y++) {
        for (int x = -1; x <= 2; x++) {
            int3 tap = int3(pixel + int2(x, y), 0);
            float tap_depth = s_position.Load(tap).x;
            float tap_weight = step(tap_depth, 0.02f)
                * saturate(1.0f - abs(GbufferGetViewZ(tap_depth) - view_z) / (0.005f + view_z * 0.02f))
                * saturate(dot(normal, thin_unpack(s_thin.Load(tap)).normal) * 2.0f - 1.0f);
            sum += s_contact.Load(tap) * tap_weight;
            weight += tap_weight;
        }
    }

    return weight > 1e-4f ? sum / weight : s_contact.Load(int3(pixel, 0));
}

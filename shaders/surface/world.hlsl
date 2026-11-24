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
#include "common\surface_forward.hlsli"

// returns the surface under the pixel
surface_data fetch_entry(surface_vertex input)
{
    return default_fetch(input);
}

// returns what one local light adds to the surface's light
float3 light_entry(surface_vertex input, surface_data surface, light_shading_data light)
{
    return default_light(input, surface, light);
}

// returns final output color
float4 shade_entry(surface_vertex input, surface_data surface)
{
    return default_shade(input, surface);
}

#include "common\surface_forward_entry.hlsli"

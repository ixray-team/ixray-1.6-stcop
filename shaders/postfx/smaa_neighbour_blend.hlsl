/**
 * SMAA: Enhanced Subpixel Morphological Antialiasing (https://github.com/iryoku/smaa)
 *
 * Copyright (C) 2013 Jorge Jimenez (jorge@iryoku.com)
 * Copyright (C) 2013 Jose I. Echevarria (joseignacioechevarria@gmail.com)
 * Copyright (C) 2013 Belen Masia (bmasia@unizar.es)
 * Copyright (C) 2013 Fernando Navarro (fernandn@microsoft.com)
 * Copyright (C) 2013 Diego Gutierrez (diegog@unizar.es)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software. As clarification, there is no
 * requirement that the copyright notice and permission be included in binary
 * distributions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
#include "common\common.hlsli"

#define SMAA_HLSL_4_1

#define SMAA_INCLUDE_VS 1
#define SMAA_RT_METRICS float4(scaled_screen_res.zw, 1.0f / scaled_screen_res.zw)

#include "postfx\smaa.hlsli"

// Struct
struct p_smaa
{
    float4 hpos : SV_POSITION;
    float2 texcoord : TEXCOORD0; // Texture coordinates         (for sampling maps)
    float4 offset : TEXCOORD1;
};

// Vertex
p_smaa vs_main(uint VertexId : SV_VertexID)
{
    VSOutputFullscreen I = FullscreenTriangle(VertexId);
    p_smaa O;

    O.hpos = I.hpos;
    O.texcoord = I.texcoord;

    SMAANeighborhoodBlendingVS(I.texcoord, O.offset);

    return O;
}

/*
------------------------------------------------------------------
SMAA neighbour blend pass
------------------------------------------------------------------
References:
https://github.com/iryoku/smaa
------------------------------------------------------------------
Back to the Roots
*/

#define SMAA_HLSL_4_1

#define SMAA_RT_METRICS float4(scaled_screen_res.zw, 1.0f / scaled_screen_res.zw)
#define SMAA_PRESET_ULTRA

Texture2D s_blendtex : register(t25);

float4 ps_main(p_smaa I) : SV_Target
{
    return SMAANeighborhoodBlendingPS(I.texcoord, I.offset, s_image, s_blendtex);
}

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
// What the upscalers NRI drives read beside the colour: how a pixel has moved, in pixels of the frame from this frame
// to the one before, and the depth as a plain texture. The frame keeps its velocity in clip space, old to new

#include "common\common.hlsli"

struct guides_output {
    float2 motion : SV_Target0;
    float depth : SV_Target1;
};

guides_output ps_main(PSInputFullscreen input)
{
    guides_output output;
    int3 pixel = int3(input.hpos.xy, 0);
    output.motion = s_velocity.Load(pixel).xy * float2(-0.5f, 0.5f) * scaled_screen_res.xy;
    output.depth = s_position.Load(pixel).x;
    return output;
}

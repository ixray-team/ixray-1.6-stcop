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
Texture2DMS<float> s_position : register(t37);

// returns the nearest sample of the multisampled depth
float ps_main(float4 position : SV_Position) : SV_Depth
{
    uint width, height, samples;
    s_position.GetDimensions(width, height, samples);
    float depth = 1.0f;
    for (uint sample_idx = 0; sample_idx < samples; sample_idx++) {
        depth = min(depth, s_position.Load(int2(position.xy), sample_idx));
    }
    return depth;
}

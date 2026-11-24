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
Texture2DMS<uint> s_thin : register(t39);

struct resolved {
    uint thin : SV_Target;
    float depth : SV_Depth;
};

// returns the nearest sample of the multisampled depth and the thin target's word of that sample: the words of
// two surfaces cannot be averaged
resolved ps_main(float4 position : SV_Position)
{
    uint width, height, samples;
    s_position.GetDimensions(width, height, samples);

    resolved output;
    output.depth = 1.0f;
    uint nearest_idx = 0u;

    for (uint sample_idx = 0; sample_idx < samples; sample_idx++) {
        float depth = s_position.Load(int2(position.xy), sample_idx);
        if (depth < output.depth) {
            output.depth = depth;
            nearest_idx = sample_idx;
        }
    }

    output.thin = s_thin.Load(int2(position.xy), nearest_idx);
    return output;
}

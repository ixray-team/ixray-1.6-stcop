#include "common\r1_common.hlsli"

Texture2D s_distort : register(t25);

// returns the picture read where the distortion target pushes the pixel to
float4 ps_main(PSInputFullscreen input) : SV_Target
{
    // The push is a share of the view, and stays inside it
    float2 push = (s_distort.SampleLevel(smp_nofilter, input.texcoord, 0).xy - 127.0f / 255.0f) * 0.05f * view_uv.xy;
    push = min(input.texcoord + push, view_uv.zw) - input.texcoord;
    // The HUD is not pulled into the scene behind it, as the frame that shades a pixel once has it
    float2 place = s_position.SampleLevel(smp_nofilter, input.texcoord + push, 0).x < 0.02f ? input.texcoord : input.texcoord + push;
    return float4(s_image.SampleLevel(smp_rtlinear, place, 0).rgb, 1.0f);
}

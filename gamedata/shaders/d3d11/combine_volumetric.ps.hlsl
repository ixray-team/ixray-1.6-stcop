#include "common.hlsli"
Texture2D s_vollight;

uniform float4 scaled_screen_res;

float4 main(PSInputFullscreen I) : SV_Target
{
    return s_vollight.SampleLevel(smp_rtlinear, I.texcoord.xy, 0);
}


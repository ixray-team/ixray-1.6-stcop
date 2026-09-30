#include "common.hlsli"
Texture2D s_vollight;


float4 main(PSInputFullscreen I) : SV_Target
{
    return s_vollight.SampleLevel(smp_rtlinear, I.texcoord.xy, 0);
}


#include "r1_common.hlsli"
Texture2D s_distort;
float4 main(PSInputFullscreen I) : SV_Target
{
    float2 offset = (s_distort.Sample(smp_rtlinear, I.texcoord).xy - 127.0f / 255.0f) * 0.05f;
    return float4(s_image.Sample(smp_rtlinear, I.texcoord + offset).rgb, 1.0f);
}

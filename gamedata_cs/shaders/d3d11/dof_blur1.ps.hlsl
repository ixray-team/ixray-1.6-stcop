#include "common.hlsli"

Texture2D<float> t_coc;
Texture2D<float4> t_image;
Texture2D<float4> t_bimage;

float4 main(PSInputFullscreen I) : SV_Target
{
    //Sample CoC
    float CoC = 10.f * t_coc.Sample(smp_nofilter, I.texcoord.xy).x;
    float4 color = t_image.Sample(smp_rtlinear, I.texcoord.xy);
    float4 preblurred = t_bimage.Sample(smp_rtlinear, I.texcoord.xy);
    float4 res = 1.f;
    res.rgb = max(0.f, lerp(color.rgb, preblurred.rgb, saturate(CoC.xxx)));
    return res;
}
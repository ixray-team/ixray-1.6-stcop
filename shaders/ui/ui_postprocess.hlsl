#include "common\common.hlsli"

#include "common\post_cb.hlsli"

// The icon target holds premultiplied linear light and coverage
float4 ps_main(PSInputFullscreen input) : SV_Target
{
    float4 color = s_image.SampleLevel(smp_rtlinear, input.texcoord, 0);
    color.w = saturate(color.w);
    color.xyz = ToneToDisplay(tonemap(color.xyz / max(color.w, EPS_S), 1.5f)) * color.w;
    color.xyz *= static_color.xyz * static_color.w;
    color.w *= static_color.w;
    return color;
}

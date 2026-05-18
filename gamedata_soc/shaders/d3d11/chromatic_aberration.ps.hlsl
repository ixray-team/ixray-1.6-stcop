#include "common.hlsli"
uniform float4 screen_res;

float4 main(p_shadow I) : SV_Target
{
    float3 col;
    float factor = saturate(distance(I.tc0, float2(0.5, 0.5)));
    col.r = s_image.Sample(smp_rtlinear, float2(I.tc0 + float2(screen_res.z * factor, 0))).r;
    col.g = s_image.Sample(smp_rtlinear, float2(I.tc0 + float2(-0.866, -0.5) * screen_res.zw * factor)).g;
    col.b = s_image.Sample(smp_rtlinear, float2(I.tc0 + float2(0.866, -0.5) * screen_res.zw * factor)).b;

    return float4(col, 1);
}

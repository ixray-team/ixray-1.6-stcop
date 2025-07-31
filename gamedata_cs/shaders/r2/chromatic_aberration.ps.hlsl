#include "common.hlsli"

uniform float4 screen_res;

float4 main(p_shadow I) : COLOR
{
    float3 col;
    float factor = saturate(distance(I.tc0, float2(0.5f, 0.5f)));

    col.r = tex2D(s_image, I.tc0 + float2(screen_res.z * factor, 0.0f)).r;
    col.g = tex2D(s_image, I.tc0 + float2(-0.866f, -0.5f) * screen_res.zw * factor).g;
    col.b = tex2D(s_image, I.tc0 + float2(0.866f, -0.5f) * screen_res.zw * factor).b;

    return float4(col, 1.0f);
}

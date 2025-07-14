#include "common.hlsli"

float4 main(p_shadow I) : COLOR
{
    float3 col = tex2D(s_image, I.tc0).xyz;
    col *= 1.0f - saturate(distance(I.tc0, float2(0.5f, 0.5f)));

    return float4(col, 1.0f);
}

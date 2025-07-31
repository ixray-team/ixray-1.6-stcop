#include "common.hlsli"

float3 pharse_saturation(float3 Color)
{
    float Luma = dot(Color.xyz, 0.33f) + 0.001f;
    return lerp(Luma.xxx, Color.xyz, 1.2f);
}

float4 main(p_shadow I) : COLOR
{
    float3 col = tex2D(s_image, I.tc0).xyz;
    col = 1.2f * pharse_saturation(col);

    return float4(col, 1.0f);
}

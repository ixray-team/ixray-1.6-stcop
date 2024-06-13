#include "common.hlsli"

float3 pharse_saturation (float3 Color)
 {
    float Luma = dot(Color.xyz, 0.33f) + 0.001f;
    return lerp(Luma.xxx, Color.xyz, 0.3f * 4.0f);
}

float4 main(p_shadow I) : COLOR
{
    float3 col = tex2D(s_image, I.tc0).xyz;
    col = pharse_saturation(col);
    col *= 1.4f - (1.4f * saturate(distance(I.tc0, float2(0.5f, 0.5f))));

    return float4(col, 1.0f);
}

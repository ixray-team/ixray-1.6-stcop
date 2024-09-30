#include "common.hlsli"

float3 pharse_saturation(float3 Color)
{
    float Luma = dot(Color.xyz, 0.33f) + 0.001f;
    return lerp(Luma.xxx, Color.xyz, 1.2f);
}

float4 main(p_shadow I) : SV_Target
{
    float3 col = s_image.Load(int3(I.hpos.xy, 0), 0).xyz;
    col = 1.2f * pharse_saturation(col);

    return float4(col, 1.0f);
}

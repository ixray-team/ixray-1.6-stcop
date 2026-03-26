#include "common.hlsli"

float3 pharse_saturation(float3 Color)
{
    float Luma = Luminance(Color);
    return lerp(Luma.xxx, Color.xyz, 1.0f);
}

float4 main(PSInputFullscreen I) : SV_Target
{
    float3 col = s_image.Load(int3(I.hpos.xy, 0), 0).xyz;
    col = pharse_saturation(col);

    return float4(col, 1.0f);
}

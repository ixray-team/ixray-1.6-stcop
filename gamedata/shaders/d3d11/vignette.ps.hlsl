#include "common.hlsli"

float4 main(p_shadow I) : SV_Target
{
    float3 col = s_image.Load(int3(I.hpos.xy, 0), 0).xyz;
    col *= 1.0f - saturate(distance(I.tc0, float2(0.5f, 0.5f)));

    return float4(col, 1.0f);
}

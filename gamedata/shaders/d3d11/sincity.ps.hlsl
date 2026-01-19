#include "common.hlsli"

// Uniform mapped from engine: sincity_params = sensitivity
uniform float sincity_params;

float4 main(p_shadow I) : SV_Target
{
    float3 col = s_image.Sample(smp_rtlinear, I.tc0).xyz;

    float lum = dot(col, float3(0.2126f, 0.7152f, 0.0722f));
    float3 gray = lum.xxx;

    float red_delta = col.r - max(col.g, col.b);
    float mask = saturate(red_delta * sincity_params);

    float3 result = lerp(gray, col, mask);
    return float4(result, 1.0f);
}

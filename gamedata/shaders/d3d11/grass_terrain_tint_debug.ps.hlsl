#include "common.hlsli"

uniform float4 grass_tint_bounds;
Texture2D s_grass_tint_color;

float4 main(VSOutputFullscreen I) : SV_Target
{
    float2 uv = I.texcoord;
    float aspect = grass_tint_bounds.w / max(grass_tint_bounds.z, 0.000001f);
    float screenAspect = screen_res.x / screen_res.y;
    float2 fit = float2(max(screenAspect / aspect, 1.0f), max(aspect / screenAspect, 1.0f));
    uv = (uv - 0.5f) * fit + 0.5f;
    if (any(uv < 0.0f) || any(uv > 1.0f))
    {
        return float4(0.025f, 0.025f, 0.025f, 1.0f);
    }
    float4 color = s_grass_tint_color.SampleLevel(smp_rtlinear, uv, 0.0f);
    float checker = fmod(floor(uv.x * 64.0f) + floor(uv.y * 64.0f), 2.0f);
    float3 background = lerp(0.08f, 0.14f, checker).xxx;
    float3 albedo = LinearToGamma(max(color.rgb / max(color.a, 0.0001f), 0.0f));
    return float4(lerp(background, albedo, saturate(color.a)), 1.0f);
}

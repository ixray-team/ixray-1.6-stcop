#ifndef GRASS_TERRAIN_TINT_INCLUDED
#define GRASS_TERRAIN_TINT_INCLUDED

uniform float4 grass_tint_bounds;
uniform float4 grass_tint_params;
uniform float4 grass_tint_options;

Texture2D s_grass_tint_color : register(t1);
Texture2D s_grass_tint_height : register(t2);

float4 GrassTerrainTintSample(float3 root, float localHeight)
{
    if (grass_tint_options.y < 0.5f)
    {
        return float4(1.0f, 1.0f, 1.0f, 0.0f);
    }
    float2 uv = (root.xz - grass_tint_bounds.xy) * grass_tint_bounds.zw;
    if (any(uv < 0.0f) || any(uv > 1.0f))
    {
        return float4(1.0f, 1.0f, 1.0f, 0.0f);
    }
    float coverage = s_grass_tint_color.SampleLevel(smp_nofilter, uv, 0.0f).a;
    float groundHeight = s_grass_tint_height.SampleLevel(smp_nofilter, uv, 0.0f).r;
    if (coverage < 0.5f || abs(root.y - groundHeight) > grass_tint_params.z)
    {
        return float4(1.0f, 1.0f, 1.0f, 0.0f);
    }
    float4 color = s_grass_tint_color.SampleLevel(smp_rtlinear, uv, grass_tint_params.y);
    float weight = lerp(1.0f, 0.55f, saturate(localHeight));
    return float4(color.rgb / max(color.a, 0.0001f), weight);
}

float3 GrassTerrainTintApply(float3 baseColor, float4 tint)
{
    if (tint.a <= 0.0f || grass_tint_options.y < 0.5f)
    {
        return baseColor;
    }
    if (grass_tint_params.w > 0.5f && grass_tint_params.w < 1.5f)
    {
        return LinearToGamma(max(tint.rgb, 0.0f));
    }
    float strength = saturate(grass_tint_params.x * tint.a);
    if (grass_tint_options.x > 0.5f)
    {
        // Preserve flowers, dry stems and neutral stones in mixed detail atlases.
        float green = (baseColor.g - max(baseColor.r, baseColor.b)) / max(baseColor.g, 0.02f);
        strength *= saturate(green * 5.0f);
    }
    const float3 luma = float3(0.2126f, 0.7152f, 0.0722f);
    float luminance = dot(tint.rgb, luma);
    if (luminance < 0.0001f || strength <= 0.0f)
    {
        return baseColor;
    }
    float3 chroma = clamp(tint.rgb / luminance, 0.45f, 1.8f);
    float3 original = GammaToLinear(max(baseColor, 0.0f));
    float3 colored = original * chroma;
    colored *= dot(original, luma) / max(dot(colored, luma), 0.0001f);
    return LinearToGamma(lerp(original, colored, strength));
}
#endif

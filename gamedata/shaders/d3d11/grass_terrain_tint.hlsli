#ifndef GRASS_TERRAIN_TINT_INCLUDED
#define GRASS_TERRAIN_TINT_INCLUDED

#include "grass_terrain_tint_color.hlsli"

uniform float4 grass_tint_bounds;
uniform float4 grass_tint_params;
uniform float4 grass_tint_options;
uniform float4 grass_tint_adjustment;

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
        return float4(1.0f, 1.0f, 1.0f, -1.0f);
    }
    float coverage = s_grass_tint_color.SampleLevel(smp_nofilter, uv, 0.0f).a;
    float groundHeight = s_grass_tint_height.SampleLevel(smp_nofilter, uv, 0.0f).r;
    if (coverage < 0.5f)
    {
        return float4(1.0f, 1.0f, 1.0f, -2.0f);
    }
    if (abs(root.y - groundHeight) > grass_tint_params.z)
    {
        return float4(1.0f, 1.0f, 1.0f, -3.0f);
    }
    float4 color = s_grass_tint_color.SampleLevel(smp_rtlinear, uv, grass_tint_params.y);
    float weight = lerp(1.0f, saturate(grass_tint_options.z), saturate(localHeight));
    return float4(color.rgb / max(color.a, 0.0001f), weight);
}

float3 GrassTerrainTintApply(float3 baseColor, float4 tint)
{
    // Test the detail shader independently from terrain coverage and height rejection.
    if (grass_tint_params.w > 2.5f && grass_tint_params.w < 3.5f)
    {
        return float3(1.0f, 0.0f, 1.0f);
    }
    if (grass_tint_params.w > 3.5f)
    {
        if (tint.a > 0.0f) return float3(0.0f, 1.0f, 0.0f);
        if (tint.a < -2.5f) return float3(1.0f, 1.0f, 0.0f);
        if (tint.a < -1.5f) return float3(1.0f, 0.0f, 0.0f);
        if (tint.a < -0.5f) return float3(0.0f, 0.0f, 1.0f);
        return float3(0.25f, 0.25f, 0.25f);
    }
    if (tint.a <= 0.0f || grass_tint_options.y < 0.5f)
    {
        return baseColor;
    }
    if (grass_tint_params.w > 0.5f && grass_tint_params.w < 1.5f)
    {
        return GrassTerrainTintToGamma(tint.rgb);
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
    float3 original = GrassTerrainTintToLinear(baseColor);
    float3 ground = max(lerp(luminance.xxx, tint.rgb, grass_tint_adjustment.y), 0.0f);
    float3 chroma = ground / max(dot(ground, luma), 0.0001f);
    float originalLuminance = dot(original, luma);
    float3 colored;
    if (grass_tint_options.w > 0.5f)
    {
        // Replace the source hue, retaining the blade's grayscale texture and relief.
        // A multiplicative tint cannot turn strongly green grass into yellow dry grass.
        float relief = originalLuminance * pow(max(originalLuminance / 0.18f, 0.0001f), grass_tint_adjustment.z - 1.0f);
        colored = chroma * relief;
    }
    else
    {
        // The original soft tint remains available for subtle color grading.
        colored = original * clamp(chroma, 0.45f, 1.8f);
        colored *= originalLuminance / max(dot(colored, luma), 0.0001f);
    }
    float brightness = lerp(1.0f, clamp(luminance / 0.18f, 0.25f, 3.0f), grass_tint_adjustment.x);
    colored *= brightness;
    return GrassTerrainTintToGamma(lerp(original, colored, strength));
}
#endif

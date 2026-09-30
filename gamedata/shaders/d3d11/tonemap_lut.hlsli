#ifndef TONEMAP_LUT_H
#define TONEMAP_LUT_H

Texture3D<float4> s_tonemap_lut;
// R32_FLOAT, 4x1: exposure multiplier, LUT maximum, exposed P05, exposed P95.
Texture2D<float> s_tonemap_state;

float3 TonemapLUT(float3 color)
{
    float exposure = s_tonemap_state.Load(int3(0, 0, 0));
    float range = s_tonemap_state.Load(int3(1, 0, 0));
    float3 grid = pow(saturate(color * exposure / range), 0.25f);
    uint width, height, depth;
    s_tonemap_lut.GetDimensions(width, height, depth);
    float3 size = float3(width, height, depth);
    float3 uvw = (grid * (size - 1.0f) + 0.5f) / size;
    return saturate(s_tonemap_lut.SampleLevel(smp_rtlinear, uvw, 0).rgb);
}

#ifdef DEBUG_TONEMAP_LUT
float3 DrawTonemapLUT(float3 Color, float2 position)
{
    uint screenWidth, screenHeight;
    s_image.GetDimensions(screenWidth, screenHeight);
    uint width, height, depth;
    s_tonemap_lut.GetDimensions(width, height, depth);

    // Top-left, 16 px inset. Blue slices increase left-to-right, then downward.
    uint columns = min(depth, 8u);
    uint rows = (depth + columns - 1u) / columns;
    float tileSize = max(3.0f, min(66.0f, floor(min(
        (float(screenWidth) - 32.0f) / columns,
        (float(screenHeight) - 32.0f) / rows))));
    float2 p = position - 16.0f;
    [branch]
    if (any(p < 0.0f) || any(p >= float2(columns, rows) * tileSize))
        return Color;

    uint2 tile = uint2(p / tileSize);
    uint slice = tile.y * columns + tile.x;
    float2 local = p - float2(tile) * tileSize;
    if (slice >= depth || any(local < 1.0f) || any(local >= tileSize - 1.0f))
        return 0.15f.xxx;

    // Show stored texels directly: R increases rightward, G upward.
    // No exposure or inverse LUT shaper is applied to this diagnostic view.
    float2 uv = (local - 1.0f) / (tileSize - 2.0f);
    uv.y = 1.0f - uv.y;
    uint2 cell = min(uint2(uv * float2(width, height)), uint2(width - 1u, height - 1u));
    float3 value = s_tonemap_lut.Load(int4(cell, slice, 0)).rgb;
    return LinearToGamma(saturate(value));
}
#endif

#endif

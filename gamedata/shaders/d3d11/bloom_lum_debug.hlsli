#include "bloom_lum_histogram.hlsli"

Texture2D<uint> s_histogram;
Texture2D<uint> s_output_histogram;

float2 HistogramDebugSize()
{
    uint width, height;
    s_image.GetDimensions(width, height);
    // Two panels with a 16 px gap and 16 px outer margins.
    return min(float2(512.0f, 160.0f), max(float2((float(width) - 48.0f) * 0.5f, float(height) - 32.0f), 1.0f));
}

float3 DrawHistogram(float3 Color, float2 position)
{
    uint width, height;
    s_image.GetDimensions(width, height);

    // Pixel coordinates: 16 px from the bottom-left, up to 512 x 160 px.
    float2 size = HistogramDebugSize();
    float2 p = position - float2(16.0f, float(height) - 16.0f - size.y);
    [branch]
    if (any(p < 0.0f) || any(p >= size))
        return Color;

    float2 uv = p / size;
    uint bin = min((uint)(uv.x * HISTOGRAM_BINS), HISTOGRAM_BINS - 1);
    uint count = s_histogram.Load(int3(bin, 0, 0));
    uint peak = 0, total = 0, prefix = 0;
    [loop]
    for (uint i = 0; i < HISTOGRAM_BINS; ++i)
    {
        uint value = s_histogram.Load(int3(i, 0, 0));
        peak = max(peak, value);
        total += value;
        if (i < bin)
            prefix += value;
    }

    Color = lerp(Color, float3(0.015f, 0.02f, 0.025f), 0.85f);
    float graphY = 1.0f - uv.y;
    // Horizontal grid at 25%, 50%, 75% of the largest bin.
    if (abs(graphY * 4.0f - round(graphY * 4.0f)) < 2.0f / size.y)
        Color = lerp(Color, 0.2f.xxx, 0.5f);

    float selected = max(0.0f, min(float(prefix + count), total * HistogramHighPercent)
        - max(float(prefix), total * HistogramLowPercent));
    float retained = selected / max(float(count), 1.0f);
    float barHeight = float(count) / max(float(peak), 1.0f);
    if (count > 0 && graphY <= barHeight)
        Color = lerp(float3(0.85f, 0.3f, 0.12f), float3(0.3f, 0.8f, 0.55f), retained);

    // Yellow line: temporally adapted log luminance from the compute path.
    float mean = s_tonemap_compute.Load(int3(0, 0, 0));
    float meanX = saturate((mean - HistogramMin) / (HistogramMax - HistogramMin));
    if (total > 0 && abs(p.x - meanX * (size.x - 1.0f)) < 1.0f)
        Color = float3(1.0f, 0.85f, 0.15f);

    if (p.x < 1.0f || p.y < 1.0f || p.x >= size.x - 1.0f || p.y >= size.y - 1.0f)
        Color = 0.5f.xxx;

    return Color;
}

float3 DrawOutputHistogram(float3 Color, float2 position)
{
    uint width, height;
    s_image.GetDimensions(width, height);
    float2 size = HistogramDebugSize();
    float2 p = position - float2(32.0f + size.x, float(height) - 16.0f - size.y);
    [branch]
    if (any(p < 0.0f) || any(p >= size))
        return Color;

    float2 uv = p / size;
    uint bin = min((uint)(uv.x * HISTOGRAM_BINS), HISTOGRAM_BINS - 1);
    uint count = s_output_histogram.Load(int3(bin, 0, 0));
    uint peak = 0;
    [loop]
    for (uint i = 0; i < HISTOGRAM_BINS; ++i)
        peak = max(peak, s_output_histogram.Load(int3(i, 0, 0)));

    Color = lerp(Color, float3(0.015f, 0.02f, 0.025f), 0.85f);
    float graphY = 1.0f - uv.y;
    if (abs(graphY * 4.0f - round(graphY * 4.0f)) < 2.0f / size.y)
        Color = lerp(Color, 0.2f.xxx, 0.5f);
    if (count > 0 && graphY <= float(count) / max(float(peak), 1.0f))
        Color = float3(0.3f, 0.65f, 1.0f);
    if (p.x < 1.0f || p.y < 1.0f || p.x >= size.x - 1.0f || p.y >= size.y - 1.0f)
        Color = 0.5f.xxx;
    return Color;
}

#include "bloom_lum_histogram.hlsli"

Texture2D<uint> s_histogram;
RWTexture2D<float> u_luminance : register(u0);
// R32_FLOAT avoids optional float4 typed UAV loads on D3D11.
RWTexture2D<float> u_tonemap_state : register(u1);
float4 adapt_params; // x - luminance blend, y - LUT range blend; both 1 on first frame
float4 autoexposure_params; // key, minimum EV, maximum EV, bias EV
static const float TonemapMinRange = 4.0f;

groupshared uint Histogram[HISTOGRAM_BINS];

[numthreads(HISTOGRAM_BINS, 1, 1)]
void main(uint index : SV_GroupIndex)
{
    Histogram[index] = s_histogram.Load(int3(index, 0, 0));
    GroupMemoryBarrierWithGroupSync();
    if (index != 0)
        return;

    uint count = 0;
    for (uint i = 0; i < HISTOGRAM_BINS; ++i)
        count += Histogram[i];

    float low = count * HistogramLowPercent;
    float high = count * HistogramHighPercent;
    float prefix = 0.0f, sum = 0.0f, weight = 0.0f;
    float logLow = HistogramMin, logHigh = HistogramMin;
    float binWidth = (HistogramMax - HistogramMin) / HISTOGRAM_BINS;
    for (uint bin = 0; bin < HISTOGRAM_BINS; ++bin)
    {
        float next = prefix + Histogram[bin];
        float selected = max(0.0f, min(next, high) - max(prefix, low));
        float logLuma = HistogramMin + (bin + 0.5f) * binWidth;
        sum += logLuma * selected;
        weight += selected;
        // Tonemap bounds use P05/P95 independently of the exposure trim settings.
        if (Histogram[bin] > 0)
        {
            float p05 = count * 0.05f, p95 = count * 0.95f;
            if (prefix < p05 && next >= p05)
                logLow = HistogramMin + (bin + (p05 - prefix) / Histogram[bin]) * binWidth;
            if (prefix < p95 && next >= p95)
                logHigh = HistogramMin + (bin + (p95 - prefix) / Histogram[bin]) * binWidth;
        }
        prefix = next;
    }

    float adapted = u_luminance[uint2(0, 0)];
    if (weight > 0.0f)
    {
        adapted = lerp(adapted, sum / weight, adapt_params.x);
        u_luminance[uint2(0, 0)] = adapted;
    }
    // The same multiplier is consumed by combine2; no second exposure calculation.
    float ev = clamp(log2(max(autoexposure_params.x, 1e-6f)) - adapted + autoexposure_params.w,
        autoexposure_params.y, autoexposure_params.z);
    float exposure = exp2(ev);
    float previousRange = u_tonemap_state[uint2(1, 0)];
    float range = max(previousRange, TonemapMinRange);
    if (count > 0)
    {
        float exposedLow = exp2(logLow + ev);
        float exposedHigh = exp2(logHigh + ev);
        float targetRange = max(exposedHigh, TonemapMinRange);
        range = previousRange > 0.0f ? exp2(lerp(log2(range), log2(targetRange), adapt_params.y)) : targetRange;
        u_tonemap_state[uint2(2, 0)] = exposedLow;
        u_tonemap_state[uint2(3, 0)] = exposedHigh;
    }
    else if (previousRange <= 0.0f)
    {
        // Defined first-frame state even when the source contains only NaN/Inf.
        range = TonemapMinRange;
        u_tonemap_state[uint2(2, 0)] = 0.0f;
        u_tonemap_state[uint2(3, 0)] = 1.0f;
    }
    u_tonemap_state[uint2(0, 0)] = exposure;
    u_tonemap_state[uint2(1, 0)] = range;
}

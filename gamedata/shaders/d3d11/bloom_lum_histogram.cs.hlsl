#include "common.hlsli"
#include "bloom_lum_histogram.hlsli"

RWTexture2D<uint> u_histogram : register(u0);
groupshared uint Histogram[HISTOGRAM_BINS];

[numthreads(16, 16, 1)]
void main(uint3 id : SV_DispatchThreadID, uint index : SV_GroupIndex)
{
    Histogram[index] = 0;
    GroupMemoryBarrierWithGroupSync();

    uint width, height;
    s_image.GetDimensions(width, height);
    if (id.x < width && id.y < height)
    {
        float3 Color = s_image.Load(int3(id.xy, 0)).rgb;
        if (all(isfinite(Color)))
        {
            #ifdef DEBUG_OUTPUT_HISTOGRAM
            float Bin = saturate(dot(max(Color, 0.0f), LUMINANCE_VECTOR));
            #else
            float Luma = dot(max(Color, 0.0f), LUMINANCE_VECTOR);
            float EV100 = SceneLuminanceToEV100(Luma);
            float Bin = saturate((EV100 - HistogramMinEV100) / (HistogramMaxEV100 - HistogramMinEV100));
            #endif
            uint bin = min((uint)(Bin * HISTOGRAM_BINS), HISTOGRAM_BINS - 1);
            InterlockedAdd(Histogram[bin], 1);
        }
    }

    GroupMemoryBarrierWithGroupSync();
    InterlockedAdd(u_histogram[uint2(index, 0)], Histogram[index]);
}

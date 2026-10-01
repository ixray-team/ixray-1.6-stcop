#include "common.hlsli"

#define DEBUG_HISTOGRAM
#define DEBUG_TONEMAP_LUT

Texture2D<float> s_tonemap_compute;
#include "bloom_lum_debug.hlsli"
#include "tonemap_lut.hlsli"

float3 main(PSInputFullscreen I) : SV_Target
{
    float3 Color = s_image.Load(int3(I.hpos.xy, 0)).rgb;
    #ifdef DEBUG_HISTOGRAM
        Color = DrawHistogram(Color, I.hpos.xy);
        Color = DrawOutputHistogram(Color, I.hpos.xy);
    #endif
    #ifdef DEBUG_TONEMAP_LUT
        Color = DrawTonemapLUT(Color, I.hpos.xy);
    #endif
    return Color;
}

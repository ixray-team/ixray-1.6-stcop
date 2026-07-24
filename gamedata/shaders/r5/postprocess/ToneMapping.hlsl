#ifndef TIRAMISU_TONE_MAPPING_HLSL
#define TIRAMISU_TONE_MAPPING_HLSL

float TiramisuLuminance(float3 Color)
{
    return dot(Color, float3(0.2126f, 0.7152f, 0.0722f));
}

// Adapted from the R4 commerce tone-mapping path. The function is kept
// resource-free so postprocess passes own all bindings and exposure policy.
float3 TiramisuCommerceToneMap(float3 Color, float StartCompression, float Desaturation)
{
    Color = max(Color, 0.0f);
    const float MinimumChannel = min(Color.r, min(Color.g, Color.b));
    const float Offset = MinimumChannel < 0.08f ?
        MinimumChannel - 6.25f * MinimumChannel * MinimumChannel : 0.04f;
    Color -= Offset;

    const float Peak = max(Color.r, max(Color.g, Color.b));
    if (Peak < StartCompression)
        return Color;

    const float DistanceToWhite = 1.0f - StartCompression;
    const float NewPeak = 1.0f - DistanceToWhite * DistanceToWhite /
        max(Peak + DistanceToWhite - StartCompression, 1.0e-6f);
    Color *= NewPeak / max(Peak, 1.0e-6f);
    const float DesaturationWeight = 1.0f - rcp(Desaturation * (Peak - NewPeak) + 1.0f);
    return lerp(Color, NewPeak.xxx, saturate(DesaturationWeight));
}

float3 TiramisuLinearToSrgb(float3 LinearColor)
{
    const float3 Low = LinearColor * 12.92f;
    const float3 High = 1.055f * pow(max(LinearColor, 0.0f), 1.0f / 2.4f) - 0.055f;
    return lerp(High, Low, LinearColor <= 0.0031308f);
}

#endif

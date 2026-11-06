#ifndef GRASS_TERRAIN_TINT_COLOR_INCLUDED
#define GRASS_TERRAIN_TINT_COLOR_INCLUDED

// Capture and vegetation must agree on the atlas encoding in both lighting modes.
// The engine's GammaToLinear macro is an identity in legacy lighting.
float3 GrassTerrainTintToLinear(float3 Color)
{
    return pow(max(Color, 0.0f), 2.2f);
}

float3 GrassTerrainTintToGamma(float3 Color)
{
    return pow(max(Color, 0.0f), 1.0f / 2.2f);
}

#endif

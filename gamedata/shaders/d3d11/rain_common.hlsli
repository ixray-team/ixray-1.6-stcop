#ifndef rain_common_hlsli_included
#define rain_common_hlsli_included


Texture2D s_rain_refraction;

float3 RainSkyTint()
{
    float3 SkyTint = GammaToLinear(L_hemi_color.rgb);
    return SkyTint / max(dot(SkyTint, LUMINANCE_VECTOR), 1e-4f);
}

float3 RainSunScatter(float3 ViewDirWorld)
{
    float Forward = saturate(dot(ViewDirWorld, -L_sun_dir_w.xyz));
    return L_sun_color.rgb * (0.02f + 0.3f * pow(Forward, 12.0f));
}

#endif

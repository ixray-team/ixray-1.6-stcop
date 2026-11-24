/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
#ifndef rain_common_hlsli_included
#define rain_common_hlsli_included


Texture2D s_rain_refraction : register(t25);

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

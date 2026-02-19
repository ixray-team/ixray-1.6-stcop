/*
Made for I-Xray engine by Papa Doenitz, based on original GSC code and Inigo Quilez's article (https://iquilezles.org/articles/fog/).
This shader calculates volumetric sun shafts (god rays) using ray marching in screen space.
The shader samples the shadow map to determine how much light is reaching each pixel, and applies a fog effect based on distance and view direction.
This software is protected by copyright Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License (CC BY-NC-SA 4.0). For more information, please visit https://creativecommons.org/licenses/by-nc-sa/4.0/.
If you want to use or modify this sofware, please credit the original author (Papa Doenitz) and provide a link to the original source and Inigo Quilez article. You may not use this shader for commercial purposes without permission from the author. If you create a derivative work, you must share it under the same license.
*/

#include "common.hlsli"

struct PSInput
{
	float4 hpos : SV_POSITION;
	float2 texcoord : TEXCOORD0;
};

#undef USE_ULTRA_SHADOWS

#define RAY_PATH 2.0h
#define JITTER_TEXTURE_SIZE 128.0f // blue noise 128x128 make sure to set up properly

#define NEW_SHAFTS // papa doenitz take on fog apply
#define SUN_SHAFTS_QUALITY 1

#ifdef SUN_SHAFTS_QUALITY
    #if SUN_SHAFTS_QUALITY == 1
        // #define FILTER_LOW
        #define RAY_SAMPLES 6 // what>>>!!!
    #elif SUN_SHAFTS_QUALITY == 2
        // #define FILTER_LOW
        #define RAY_SAMPLES 16
    #elif SUN_SHAFTS_QUALITY == 3
        // #define FILTER_LOW
        #define RAY_SAMPLES 24
    #endif
#endif

#include "shadow.hlsli"

float4 sun_shafts_intensity;

float4 main(PSInput I) : SV_Target
{
#ifndef SUN_SHAFTS_QUALITY
    return float4(0, 0, 0, 0);
#else //	SUN_SHAFTS_QUALITY
    IXrayGbuffer O;
    GbufferUnpack(I.texcoord.xy, I.hpos.xy, O);

    float3 P = O.Point;
    // swaped old GSC jitter for bluenoise. N = tex * 2 - 1 (so noise jumps both ways)
    float4 J0 = blue_noise.Sample(smp_nofilter, float3(frac(I.hpos.xy / JITTER_TEXTURE_SIZE), 0.5)) * 2.f - 1.f;
	//float coeff = (RAY_SAMPLES - J0.x) / (RAY_SAMPLES * RAY_SAMPLES);
    float3 delta = P / (RAY_SAMPLES);
    float3 start = delta * (J0.x * 0.5);
    // add some bias to prevent self-shadowing and artifacts when depth is very small
    float depth = P.z - start.z;
    float deltaDepth = max(delta.z, 1e-3);
    // transform to world space
    float3 PW = mul(m_invV, float4(P - start, 1.0f));
    float3 deltaW = mul(m_invV, delta);
    // transform to shadow map space
    float4 current = mul(m_shadow_sun[2], float4(PW, 1.0f));
    float4 deltaS = mul(m_shadow_sun[2], float4(deltaW, 0.0f));
    // to do - connect max_density with fog params
    float res = 0.0f;
    float max_density = sun_shafts_intensity.x;
    float density = max_density / RAY_SAMPLES;

    if(O.Depth > 0.9999f) {
		depth = 0.0f;
        res = max_density;
    }
    // to do - make proper density sampling and scatter formulas in loop
    [unroll] for(int i = 0; i < RAY_SAMPLES; ++i)
    {
        if (depth > 0.3)
        {
			res += density * s_smap_sun.SampleCmpLevelZero(smp_smap, float3(current.xy, 2), current.z).x;
        }

        depth -= deltaDepth;
        current -= deltaS;
    }

    float fSturation = 0.5 * dot(O.View, -Ldynamic_dir.xyz) + 0.5;
    //	Normalize dot product to
    fSturation = 0.4f * fSturation + 0.6f;
    // calc 2 fog coeff, x - exp fog, y - sun shafts, both with some distance-based attenuation. To do - connect attenuation with fog params
    float2 fog = float2(0.92, 1.0) - exp(-length(P.xyz) * float2(0.001, 0.001)); // fog param.xy
    fog = saturate(fog);
    // gamma correct color, temp
    float4 g_fog_color = PushGamma(fog_color);
    float4 g_Ldynamic_color = PushGamma(Ldynamic_color);
    // mix fog color with sun color based on view direction,
    float4 superfogcolor = lerp(g_fog_color, g_Ldynamic_color, pow(fSturation, 8.0));
    // summ all together, add some blue noise to mixing
    float4 final_color = lerp(0.f, superfogcolor, fog.x * (1.0 + 0.0078125 * J0.y)) + res * g_Ldynamic_color * (fog.y * (1.0 + 0.0078125 * J0.z));
    return 0.5 * final_color;

#endif // SUN_SHAFTS_QUALITY
}


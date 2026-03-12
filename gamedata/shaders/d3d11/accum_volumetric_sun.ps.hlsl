/*
Made for I-Xray engine by Papa Doenitz, based on original GSC code and Inigo Quilez's article (https://iquilezles.org/articles/fog/).
This shader calculates volumetric sun shafts (god rays) using ray marching in screen space.
The shader samples the shadow map to determine how much light is reaching each pixel, and applies a fog effect based on distance and view direction.
This software is protected by copyright Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License (CC BY-NC-SA 4.0). For more information, please visit https://creativecommons.org/licenses/by-nc-sa/4.0/.
If you want to use or modify this sofware, please credit the original author (Papa Doenitz) and provide a link to the original source and Inigo Quilez article. You may not use this shader for commercial purposes without permission from the author. If you create a derivative work, you must share it under the same license.
*/

#include "common.hlsli"
#include "shadow.hlsli"

struct PSInput
{
    float4 hpos : SV_POSITION;
    float2 texcoord : TEXCOORD0;
};

//#define JITTER_TEXTURE_SIZE 128.0f


#if SUN_SHAFTS_QUALITY == 1
#define RAY_SAMPLES 6
#elif SUN_SHAFTS_QUALITY == 2
#define RAY_SAMPLES 12
#elif SUN_SHAFTS_QUALITY == 3
#define RAY_SAMPLES 18
#endif

// --- Medium/scattering tuning knobs ---
static const float PHASE_G = 0.5f; // Schlick g (0 isotropic, higher = forward)
static const float SCATTER_RATIO = 0.5f; // fraction of extinction that goes to scattering
static const float ABSORB_TINT_STRENGTH = 0.5f; // 0 = gray absorption, 1 = tinted by fog_color

// Sun shafts intensity param (x used as density scale)
float4 sun_shafts_intensity;

// Schlick phase function (normalized form)
float PhaseFunction_Schlick(float g, float cos_theta)
{
    float gg = g * g;
    float nom = 1.0f - gg;
    float denom = 4.0f * PI * (1.0f + g * cos_theta) * (1.0f + g * cos_theta);
    return nom / denom;
}

float4 main(PSInput I) : SV_Target
{
#ifndef SUN_SHAFTS_QUALITY
    return float4(0,0,0,0);
#else //SUN_SHAFTS_QUALITY

    IXrayGbuffer O;
    GbufferUnpack(I.texcoord.xy, I.hpos.xy, O);
    
    float3 Pview = O.Point * 0.996f;
    bool isSky = (O.Depth > 0.9999f);
    if (isSky)
    {
        float zFar = fog_params.z; 
        float3 viewDir = normalize(Pview);
        Pview = viewDir * zFar;
    }

    float4 J0 = blue_noise[uint3(uint2(I.hpos.xy) % 128, uint(m_taa_jitter.w) % 32)];


    float3 deltaView = Pview / float(RAY_SAMPLES);
    float3 startView = deltaView * J0.x;
    float3 P0view = Pview - startView;

    float depth = P0view.z; 
    float deltaDepth = deltaView.z; 

    float3 PW = mul(m_invV, float4(P0view, 1.f)).xyz;
    float3 deltaW = mul(m_invV, float4(deltaView, 0.f)).xyz;

    float4 current = mul(m_shadow_sun[2], float4(PW, 1.f));
    float4 deltaS = mul(m_shadow_sun[2], float4(deltaW, 0.f));

    float3 fogTint = PushGamma(fog_color.rgb);
    fogTint /= max(max(fogTint.r, fogTint.g), fogTint.b + 1e-6f);

    float density = 0.1f * max(sun_shafts_intensity.x, 0.f);
    float sigma_t = density;
    float3 sigma_s = sigma_t * SCATTER_RATIO * fogTint;
    float3 absorbTint = lerp(1.f.xxx, saturate(1.f.xxx - fogTint), ABSORB_TINT_STRENGTH);
    float3 sigma_a = sigma_t * (1.f - SCATTER_RATIO) * absorbTint;
    float3 sigma_t_rgb = sigma_s + sigma_a;
    
    float3 sunCol = (Ldynamic_color.rgb); // i wont use gamma here

    float3 Ldir = normalize(Ldynamic_dir.xyz);

    float3 radiance = 0.f;
    float3 T = 1.f.xxx;

    float stepLen = max(length(deltaW), 1e-5f);
    
    float distToCam = length(PW - eye_position);
    float FarFadeStart = 0.8f * fog_params.z;
    float tFar = saturate((distToCam - FarFadeStart) / max(fog_params.z - FarFadeStart, 1e-4));
    tFar = tFar * tFar * (3.f - 2.f * tFar);
    float cosTheta = dot(Ldir, O.View);
    // since density is isomorphic and light dir constant - we can take phase out of loop
    float phase = PhaseFunction_Schlick(PHASE_G, cosTheta);
    [unroll]
    for (int i = 0; i < RAY_SAMPLES; ++i)
    {
        if (depth > 0.3f)
        {   
            float shadowValid = (current.x >= 0.f && current.x <= 1.f && current.y >= 0.f && current.y <= 1.f) ? 1.f : 0.f;
            float vis = s_smap_sun.SampleCmpLevelZero(smp_smap, float3(current.xy, 2), current.z).x;
            float visSafe = lerp(1.f, vis, shadowValid);
            vis = lerp(visSafe, 1.f, tFar);

            float3 Lin = sunCol * vis;
            radiance += T * (Lin * sigma_s * phase) * stepLen;
            T *= exp(-sigma_t_rgb * stepLen);
        }

        depth -= deltaDepth;
        current -= deltaS;
    }
    
    float3 fogNeutral = lerp(fogTint, Luminance(fogTint), 0.6f);
    radiance = lerp(radiance, fogNeutral * Luminance(radiance), tFar);
    
    return float4(radiance, 1.0f);

#endif //SUN_SHAFTS_QUALITY
}


#include "common.hlsli"

struct PSInput
{
	float4 hpos : SV_POSITION;
	float2 texcoord : TEXCOORD0;
};

#undef USE_ULTRA_SHADOWS

#define RAY_PATH 2.0h
#define JITTER_TEXTURE_SIZE 64.0f

#include "shadow.hlsli"

#if !Volumetric_Alternative

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

float4 main(PSInput I) : SV_Target
{
#ifndef SUN_SHAFTS_QUALITY
    return float4(0, 0, 0, 0);
#else //	SUN_SHAFTS_QUALITY
	IXRayGbuffer O = (IXRayGbuffer)NULL;
	GbufferUnpack((uint2)I.hpos.xy, O);
	
    float3 P = O.PointReal;

    float4 J0 = s_blue_noise[uint3(uint2(I.hpos.xy) % 128, uint(m_taa_jitter.w) % 32)];
	float coeff = 1.0f / RAY_SAMPLES;
	float3 direction = P * coeff;
	P -= direction * J0.x;

    float depth = P.z;
    float dt_depth = direction.z;

    P = mul(m_invV, float4(P, 1.0f)).xyz;
    direction = mul((float3x3)m_invV, direction);

    float3 current[3];
    float3 delta[3];
    [unroll] for (int cascade_idx = 0; cascade_idx < 3; cascade_idx++) {
        current[cascade_idx] = mul(m_shadow_sun[cascade_idx], float4(P, 1.0f)).xyz;
        delta[cascade_idx] = mul(m_shadow_sun[cascade_idx], float4(direction, 0.0f)).xyz;
    }

    float res = 0.0f;
    float max_density = sun_shafts_intensity.x;
    float lit = 0.0f;
    float weight = 0.0f;

    if (O.Depth > 0.9999f) {
		depth = 0.0f;
        res = max_density;
    }

    [loop] for (int i = 0; i < RAY_SAMPLES; i++) {
        float sample = -sun_shafts_intensity.w * O.ViewDist * coeff * (RAY_SAMPLES - 1 - i);
        
		if (depth > 0.3) {
			float visibility = 0.0f;
			float blend = 0.0f;

			if (all(abs(current[0] - 0.5f) < 0.5f)) {
				visibility = SampleSunShaft(current[0], 0);
				float2 edge = 0.5f - abs(current[0].xy - 0.5f);
				blend = saturate(min(edge.x, edge.y) * 10.0f);
			}

			[branch]
			if (blend < 1.0f) {
				float outer = SampleSunShaft(current[2], 2);
				if (all(abs(current[1] - 0.5f) < 0.5f)) {
					outer = min(outer, SampleSunShaft(current[1], 1));
				}
				visibility = lerp(outer, visibility, blend);
			}

			lit += sample * visibility;
        }

        weight += sample;
        depth -= dt_depth;

        [unroll] for (int cascade_idx = 0; cascade_idx < 3; cascade_idx++) {
            current[cascade_idx] -= delta[cascade_idx];
        }
    }

    res += max_density * lit / weight;

    float fog = saturate(O.ViewDist * fog_params.w + fog_params.x);
    res = lerp(res, max_density, fog * lit / weight);
    float ScatteringAmount = 1.0f - exp(-max(sun_shafts_intensity.w, 0.0f) * O.ViewDist * 0.95f);
    res *= ScatteringAmount;
    float saturation = 0.4f * dot(O.View, -Ldynamic_dir.xyz) + 0.6f;
    res *= saturation;

    return GammaToLinear(res * Ldynamic_color);
#endif // SUN_SHAFTS_QUALITY
}

#else // Volumetric_Alternative

#define SUN_SHAFTS_QUALITY 3

#if SUN_SHAFTS_QUALITY == 1
#define RAY_SAMPLES 8
#elif SUN_SHAFTS_QUALITY == 2
#define RAY_SAMPLES 16
#elif SUN_SHAFTS_QUALITY == 3
#define RAY_SAMPLES 24
#endif

// --- Medium/scattering tuning knobs ---
static const float PHASE_G1 = 0.8f; // Schlick g (0 isotropic, higher = forward)
static const float PHASE_G2 = 0.09f; // Schlick g (0 isotropic, higher = forward)
static const float SCATTER_RATIO = 0.5f; // fraction of extinction that goes to scattering
static const float ABSORB_TINT_STRENGTH = 0.5f; // 0 = gray absorption, 1 = tinted by fog_color

 /*
 Made for I-Xray engine by Papa Doenitz, based on original GSC code and Inigo Quilez's article (https://iquilezles.org/articles/fog/).
 This shader calculates volumetric sun shafts (god rays) using ray marching in screen space.
 The shader samples the shadow map to determine how much light is reaching each pixel, and applies a fog effect based on distance and view direction.
 This software is protected by copyright Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License (CC BY-NC-SA 4.0). For more information, please visit https://creativecommons.org/licenses/by-nc-sa/4.0/.
 If you want to use or modify this sofware, please credit the original author (Papa Doenitz) and provide a link to the original source and Inigo Quilez article. You may not use this shader for commercial purposes without permission from the author. If you create a derivative work, you must share it under the same license.
 */

 // Schlick phase function (normalized form)
 float PhaseFunction_Schlick(float g, float cos_theta)
 {
   float gg = g * g;
   float nom = 1.0f - gg;
   float denom = 4.0f * PI * (1.0f + g * cos_theta) * (1.0f + g * cos_theta);
   return nom / denom;
 }

 float4 main(PSInputFullscreen I) : SV_Target
 {
 #ifndef SUN_SHAFTS_QUALITY
    return float4(0,0,0,0);
 #else //SUN_SHAFTS_QUALITY

    IXRayGbuffer O = (IXRayGbuffer)NULL;
    GbufferUnpack((uint2)I.hpos.xy, O);
    
    float3 Pview = O.PointReal * 0.996f;
	
	float PviewSqr = dot(Pview, Pview);
	float PviewSqrDiv = PviewSqr > 0.0f ? rsqrt(PviewSqr) : 0.0f;
	
	float zFar = min(PviewSqr * PviewSqrDiv, fog_params.z); 
	float3 viewDir = Pview * PviewSqrDiv;
	
	Pview = viewDir * zFar;

    float4 J0 = s_blue_noise[uint3(uint2(I.hpos.xy) % 128, uint(m_taa_jitter.w) % 32)];

    float3 deltaView = Pview / float(RAY_SAMPLES);
    float3 startView = deltaView * J0.x;
    float3 P0view = Pview - startView;

    float depth = P0view.z; 
    float dt_depth = deltaView.z; 
    
    float3 RayOrigin = mul(m_invV, float4(0.0f, 0.0f, 0.0f, 1.0f)).xyz;
    float3 RayDirection = mul((float3x3) m_invV, viewDir);

    float4 ShadowOrigin = mul(m_shadow_sun[2], float4(RayOrigin, 1.0f));
    float4 ShadowDirection = mul(m_shadow_sun[2], float4(RayDirection, 0.0f));
    
 #ifndef USE_LEGACY_LIGHT
    float3 fogTint = (fog_color.rgb); // remove gamma converstion later
 #else
    float3 fogTint = fog_color.xyz * fog_color.xyz;
 #endif

    fogTint /= max(max(fogTint.r, fogTint.g), fogTint.b + EPS_S);

    float density = max(sun_shafts_intensity.w, 0.f);
    float sigma_t = density;
    float3 sigma_s = sigma_t * SCATTER_RATIO;
    float3 absorbTint = lerp(1.f.xxx, saturate(1.f.xxx - fogTint), ABSORB_TINT_STRENGTH);
    float3 sigma_a = sigma_t * (1.f - SCATTER_RATIO) * absorbTint;
    float3 sigma_t_rgb = sigma_s + sigma_a;
  
    float3 sunCol = (Ldynamic_color.rgb); // i wont use gamma here, remove it later

    float3 Ldir = normalize(Ldynamic_dir.xyz);

    float3 radiance = 0.f;
     
    float GuideSigma = min(sigma_t_rgb.r, min(sigma_t_rgb.g, sigma_t_rgb.b));
    float GuideOpticalDepth = GuideSigma * zFar;
    float GuideAmount = 1.0f - exp(-GuideOpticalDepth);
    
    //float3 T = 1.f.xxx;

    //float stepLen = max(length(deltaW), 1e-5f);
  
    //float distToCam = length(PW - eye_position);
    float distToCam = zFar * (1.0f - J0.x / float(RAY_SAMPLES));
    float FarFadeStart = 0.8f * fog_params.z;
    float FarFadeInvRange = rcp(max(fog_params.z - FarFadeStart, 1e-4f));

    float tFar = saturate((zFar - FarFadeStart) * FarFadeInvRange);
    tFar = tFar * tFar * (3.0f - 2.0f * tFar);
    float cosTheta = dot(Ldir, O.View);
    // since density is isomorphic and light dir constant - we can take phase out of loop
    float phase = PhaseFunction_Schlick(PHASE_G1, cosTheta) + PhaseFunction_Schlick(PHASE_G2, cosTheta);
    phase *= 0.5f;
	
    [loop]
    for (int i = 0; i < RAY_SAMPLES; ++i)
    {
        float U = (float(i) + J0.x) / float(RAY_SAMPLES);

        float Distance;
        float3 IntegrationWeight;

    [branch]
        if (GuideOpticalDepth > 0.001f)
        {
            float GuideTransmittance = max(1.0f - U * GuideAmount, 1e-7f);
            Distance = -log(GuideTransmittance) / GuideSigma;

            IntegrationWeight = (GuideAmount / GuideSigma) * exp((GuideSigma - sigma_t_rgb) * Distance);
        }
        else
        {
        // Uniform sampling avoids cancellation for optically thin media.
            Distance = U * zFar;
            IntegrationWeight = zFar * exp(-sigma_t_rgb * Distance);
        }
        //Distance = clamp(Distance, 0.0f, zFar);
        float depth = viewDir.z * Distance;

        if (depth > 0.3f)
        {
            float4 current = ShadowOrigin + ShadowDirection * Distance;

            float shadowValid = all(current.xy >= 0.0f) && all(current.xy <= 1.0f) ? 1.0f : 0.0f;

            float vis = SampleSunShaft(current.xyz, 2);
            float visSafe = lerp(1.0f, vis, shadowValid);

            float SampleFarFade = saturate((Distance - FarFadeStart) * FarFadeInvRange);
            SampleFarFade = SampleFarFade * SampleFarFade * (3.0f - 2.0f * SampleFarFade);

            vis = lerp(visSafe, 0.1f, SampleFarFade);
            radiance += vis * IntegrationWeight / float(RAY_SAMPLES);
        }
    }
    radiance *= max(sun_shafts_intensity.x, 0.0f) * sunCol * sigma_s * phase;
    float3 fogNeutral = lerp(fogTint, Luminance(fogTint), 0.6f);
    radiance = lerp(radiance, fogNeutral * Luminance(radiance), tFar);
    
 #ifdef USE_LEGACY_LIGHT
    radiance = sqrt(radiance);
 #endif

     return GammaToLinear(float4(radiance, 1.0f));

#endif //SUN_SHAFTS_QUALITY
}

#endif // Volumetric_Alternative

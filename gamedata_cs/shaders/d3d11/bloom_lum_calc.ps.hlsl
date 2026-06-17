/*
Made by Papa Doenitz for IX-ray engine 2026-03-05
CC BY-NC-SA 4.0 Lisence https://creativecommons.org/licenses/by-nc-sa/4.0/

Credits goes to:
Bruno Opsenica https://bruop.github.io/exposure/
Krzysztof Narkowicz https://knarkowicz.wordpress.com/2016/01/09/automatic-exposure/
The Real MJP https://mynameismjp.wordpress.com/2011/08/10/average-luminance-compute-shader/
Epic Games, "How Epic Games is handling Auto Exposure in 4.25"
Unreal Engine Documentation, "Auto Exposure / Eye Adaptation"
*/

#include "common.hlsli"

uniform Texture2D p_image;

float4 adapt_params; // x - ps_r2_autoexposure_min_weight, y - ps_r2_autoexposure_gaussian, z - ps_r2_autoexposure_speed
float4 adapt_params2; // x - ps_r2_autoexposure_soft_log_k, y - ps_r2_autoexposure_soft_limiter, z- ps_r2_autoexposure_sensitivity

float4 MiddleGray;
/*
constants buffer descr:
    autoexposure_min_weight - minimum weight for farthest pixels, can be tweaked, higher value means more even weight distribution, lower value means more center weighted distribution
    autoexposure_gaussian - gaussian weight distribution, higher - more center weighted, lower - more flat distribution, can be tweaked
    autoexposure_speed - how fast exposure adapts to changes, can be tweaked
    autoexposure_soft_log_k - strength of soft log, higher value means less aggressive log
    autoexposure_soft_limiter - limit soft log in f-stops, higher value means stronger limiter, can be tweaked
    autoexposure_sensitivity - how much to blend between log and soft-log exposure, can be tweaked
*/


//#define USE_CENTER_WEIGHTED_LUMA
//#define USE_SOFT_LOG

#if defined(USE_CLASSIQUE_TONEMAP) && defined(USE_SOFT_LOG)
	#undef USE_SOFT_LOG
#endif

float4 main(PSInputFullscreen I) : SV_Target
{
    float2 uv = I.texcoord.xy;
    float4 temp;
    float LumaCurr = 0.f, tempCurr = 0.f, weight = 1.f, weightsumm = 0.f, sumExp = 0.f;  
    // here we perform weighed average summ
    [unroll]
    for (int y = 0; y < 16; y++)
    {
        for (int x = 0; x < 16; x++)
        {
            // sample location of 16x16 tex
            uv = (float2(x,y) + 0.5) / 16.f;
            tempCurr = s_image.Sample(smp_rtlinear, uv).r;
            #ifndef USE_CENTER_WEIGHTED_LUMA
                LumaCurr += tempCurr; 
            #else   // USE_CENTER_WEIGHTED_LUMA
                uv = (uv - 0.5f) * 2.f;
                temp.x = dot(uv, uv);
                temp.y = exp2(-adapt_params.y * temp.x); // gaussian weight distribution, higher - more center weighted, lower - more flat distribution
                weight = lerp(adapt_params.x, 1.f, temp.y); // minimum weight for farthest pixels
                weight *= weight;
                weight = lerp(0.1f, 1.0f, weight);
                LumaCurr += tempCurr * weight;  
            #endif  // USE_CENTER_WEIGHTED_LUMA
            #ifdef USE_SOFT_LOG
                temp.z = exp2(adapt_params2.x * clamp(tempCurr, -16.f, +16.f)); // exp decay for soft log
                sumExp += temp.z * weight;
            #endif  
            weightsumm += weight;
        }

    }
    #ifndef USE_CENTER_WEIGHTED_LUMA
        LumaCurr *= rcp (256.f);
    #else //USE_CENTER_WEIGHTED_LUMA
        LumaCurr *= rcp(max(weightsumm, 1e-6));
    #endif
    
    #ifdef USE_SOFT_LOG
        float logSoft = (1.0f / adapt_params2) * log2(max(sumExp * rcp(max(weightsumm, 1e-6)), 1e-12f));
        logSoft = min(logSoft, LumaCurr + adapt_params2.y);
        LumaCurr = lerp(LumaCurr, logSoft, adapt_params2.z);
    #endif
   
#ifndef USE_CLASSIQUE_TONEMAP
	return float2(LumaCurr, adapt_params.z).xxxy;
#else
	LumaCurr = LinearToGamma(LumaCurr);
    LumaCurr = MiddleGray.x * rcp(LumaCurr * MiddleGray.y + MiddleGray.z);
    LumaCurr = clamp(LumaCurr, 1.f / 128.f, 20.0f);
	LumaCurr = GammaToLinear(LumaCurr);
	
    return float2(LumaCurr, MiddleGray.w).xxxy;
#endif
}


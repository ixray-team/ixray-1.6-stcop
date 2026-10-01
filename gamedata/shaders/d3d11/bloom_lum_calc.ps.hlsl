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
#include "autoexposure.hlsli"

float4 adapt_params; // minimum spatial weight, Gaussian coefficient, temporal blend alpha
float4 adapt_params2; // soft-log coefficient (1/EV), limiter (stops), blend amount

/*
constants buffer descr:
    autoexposure_min_weight - minimum weight for farthest pixels, can be tweaked, higher value means more even weight distribution, lower value means more center weighted distribution
    autoexposure_gaussian - gaussian weight distribution, higher - more center weighted, lower - more flat distribution, can be tweaked
    autoexposure_time - exponential time constant in seconds, converted to blend alpha by CPU
    autoexposure_soft_log_k - soft-log exponent coefficient in inverse stops; zero selects the mean
    autoexposure_soft_limiter - maximum soft-log uplift relative to the mean, in stops
    autoexposure_sensitivity - how much to blend between log and soft-log exposure, can be tweaked
*/


//#define USE_CENTER_WEIGHTED_LUMA
//#define USE_SOFT_LOG


float4 main(PSInputFullscreen I) : SV_Target
{
    float2 uv = I.texcoord.xy;
    float4 temp;
    float LumaCurr = 0.f, tempCurr = 0.f, weight = 1.f, weightsumm = 0.f, sumExp = 0.f;
    float softMaxEV100 = autoexposure_metering.y;
    // here we perform weighed average summ
    [loop]
    for (int y = 0; y < 16; y++)
    {
        for (int x = 0; x < 16; x++)
        {
            // sample location of 16x16 tex
            uv = (float2(x,y) + 0.5) / 16.f;
            tempCurr = s_image.SampleLevel(smp_rtlinear, uv, 0).r;
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
                // Stable weighted log-sum-exp in EV100, independent of the absolute EV offset.
                float nextMax = max(softMaxEV100, tempCurr);
                sumExp = sumExp * exp2(adapt_params2.x * (softMaxEV100 - nextMax))
                    + weight * exp2(adapt_params2.x * (tempCurr - nextMax));
                softMaxEV100 = nextMax;
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
        float logSoft = adapt_params2.x > 1e-4f
            ? softMaxEV100 + log2(max(sumExp / max(weightsumm, 1e-6f), 1e-30f)) / adapt_params2.x
            : LumaCurr;
        logSoft = min(logSoft, LumaCurr + adapt_params2.y);
        LumaCurr = lerp(LumaCurr, logSoft, saturate(adapt_params2.z));
    #endif
   
	return float2(LumaCurr, adapt_params.z).xxxy;
}


#ifndef SLOAD_H
#define SLOAD_H
#include "common.hlsli"

static const float fParallaxStartFade = 8.0f;
static const float fParallaxStopFade = 12.0f;

struct XrayMaterial
{
    float Metalness;
    float Roughness;

    float3 Normal;
    float3 Point;

    float4 Color;
    float Hemi;
    float Sun;

    float SSS;
    float AO;
};

#ifdef USE_STEEPPARALLAX
    #if defined(ALLOW_STEEPPARALLAX)

void UpdateTC(inout p_bumped_new I)
{
    if (I.position.z < fParallaxStopFade)
    {
        const float maxSamples = 25;
        const float minSamples = 5;
        const float fParallaxOffset = -0.013f;

        float3x3 TBN = float3x3(I.M1, I.M2, I.M3);
        float3 viewDir = mul(transpose(TBN), -I.position);
        viewDir = normalize(viewDir);

        // Calculate number of steps
        float nNumSteps = lerp(maxSamples, minSamples, viewDir.z);

        float fStepSize = 1.0f / nNumSteps;
        float2 vDelta = viewDir.xy * fParallaxOffset * 1.2f;
        float2 vTexOffsetPerStep = fStepSize * vDelta;

        // Prepare start data for cycle
        float2 vTexCurrentOffset = I.tcdh.xy;
        float fCurrHeight = 0.0f;
        float fCurrentBound = 1.0f;

        for (int i = 0; i < nNumSteps; ++i)
        {
            if (fCurrHeight < fCurrentBound)
            {
                vTexCurrentOffset += vTexOffsetPerStep;
                fCurrHeight = tex2Dlod(s_bumpX, float4(vTexCurrentOffset.xy, 0.0f, 0.0f)).w;

                fCurrentBound -= fStepSize;
            }
        }

        // Reconstruct previouse step's data
        vTexCurrentOffset -= vTexOffsetPerStep;
        float fPrevHeight = tex2D(s_bumpX, vTexCurrentOffset.xy).w;

        // Smooth tc position between current and previouse step
        float fDelta2 = ((fCurrentBound + fStepSize) - fPrevHeight);
        float fDelta1 = (fCurrentBound - fCurrHeight);

        float fParallaxAmount = (fCurrentBound * fDelta2 - (fCurrentBound + fStepSize) * fDelta1) / (fDelta2 - fDelta1);
        float fParallaxFade = smoothstep(fParallaxStopFade, fParallaxStartFade, I.position.z);

        float2 vParallaxOffset = vDelta * fParallaxFade * (1.0f - fParallaxAmount);
        float2 vTexCoord = I.tcdh.xy + vParallaxOffset;

        // Output the result
        I.tcdh.xy = vTexCoord;
    }
}

    #else

void UpdateTC(inout p_bumped_new I)
{
    float3x3 TBN = float3x3(I.M1, I.M2, I.M3);
    float3 viewDir = mul(transpose(TBN), -I.position);
    viewDir = normalize(viewDir);

    float height = tex2D(s_bumpX, I.tcdh.xy).w;
    height = height * parallax.x + parallax.y;

    // Output the result
    I.tcdh.xy += height * viewDir.xy;
}

    #endif
#endif

void SloadNew(inout p_bumped_new I, inout XrayMaterial M)
{
    // Enable parallax only on near objects
#if defined(USE_STEEPPARALLAX) && defined(USE_HIGH_QUALITY)
    UpdateTC(I);
#endif

    M.Color = tex2D(s_base, I.tcdh.xy);

#ifdef USE_BUMP
    float4 Bump = tex2D(s_bump, I.tcdh.xy);
    float4 BumpX = tex2D(s_bumpX, I.tcdh.xy);

    //	#ifndef USE_PBR
    M.Normal = Bump.wzy + BumpX.xyz - 1.0f;
    M.Normal.z *= 0.5f;

    M.Roughness = Bump.x * Bump.x;
    M.Metalness = 0.0f;

    M.SSS = 0.0;
    M.AO = 1.0;
//	#endif
#else
    M.Normal = float3(0.0f, 0.0f, 1.0f);

    M.Roughness = def_gloss;
    M.Metalness = 0.0f;

    M.SSS = 0.0f;
    M.AO = 1.0f;
#endif

#ifdef USE_TDETAIL
    float2 tcdbump = I.tcdh.xy * dt_params.xy;
    float4 Detail = tex2D(s_detail, tcdbump);
    #ifndef USE_PBR
    M.Color.xyz *= Detail.xyz * 2.0f;

        #ifdef USE_TDETAIL_BUMP
    float4 DetailBump = tex2D(s_detailBump, tcdbump);
    float4 DetailBumpX = tex2D(s_detailBumpX, tcdbump);
    M.Normal += DetailBump.wzy + DetailBumpX.xyz - 1.0f;
    M.Roughness *= DetailBump.x * 2.0f;
        #else
    M.Roughness *= Detail.w * 2.0f;
        #endif
    #endif
#endif

#ifndef USE_PBR
    M.Metalness = M.Roughness * 0.1f;
    M.Roughness = saturate(1.0f - M.Roughness);
#endif
}

#endif

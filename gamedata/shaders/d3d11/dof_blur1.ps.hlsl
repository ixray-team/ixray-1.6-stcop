#include "common.hlsli"

Texture2D<float>  t_coc;
Texture2D<float4> t_image;
Texture2D<float4> t_bimage;

// xy = (W,H), zw = (1/W, 1/H)
float4 dof_rt_size;

#define MaxBlurPx 50.0
#define SampleCount 40
#define PreblurStrength 0.12

float3 SampleChromaticAberration(float2 uv, float coc01)
{
    float angle = 0.5;
    float2 dir = float2(cos(angle), sin(angle));
    float maxShiftPx = 5.0;
    float shiftPx = coc01 * maxShiftPx;
    float2 pxSize = dof_rt_size.zw; // (1/W, 1/H)
    float2 shiftUV = dir * (shiftPx * pxSize);

    float r = t_image.Sample(smp_rtlinear, uv + shiftUV).r;
    float g = t_image.Sample(smp_rtlinear, uv).g;
    float b = t_image.Sample(smp_rtlinear, uv - shiftUV).b;

    return float3(r, g, b);
}

static const float GOLDEN_ANGLE = 2.39996322972865332; // rad
static const float PI_2 = 6.28318530718;

float4 main(PSInputFullscreen I) : SV_Target
{   
    float4 res = 0.0;
    float2 uv = I.texcoord.xy;
    float coc01 = saturate(t_coc.Sample(smp_nofilter, uv).x);
    float blurRadiusPx = coc01 * MaxBlurPx;
    if (blurRadiusPx < 0.5)
    {
        res = t_image.Sample(smp_rtlinear, uv);
        res.a = 1.0;
        return res;
    }

    float2 pxSize = dof_rt_size.zw;              // (1/W, 1/H)
    float2 blurRadiusUV = blurRadiusPx * pxSize; // anisotropic in UV

    float4 center = t_image.Sample(smp_rtlinear, uv);
    float4 preblur = t_bimage.Sample(smp_rtlinear, uv);

    float3 sumRGB = 0.0;
    float  sumW   = 0.0;

    float rnd = Hash(uv * dof_rt_size.xy);
    float angle0 = rnd * PI_2; 

    int N = max(SampleCount, 4);

    [unroll]
    for (int i = 0; i < N; i++)
    {
        float t = (i + 0.5) / (float)N;
        float r = sqrt(t);

        float a = angle0 + (float)i * GOLDEN_ANGLE;
        float2 dir = float2(cos(a), sin(a));

        float2 suv = uv + dir * (blurRadiusUV * r);

        float3 s = SampleChromaticAberration(suv, coc01);
        float w = lerp(0.75, 1.25, r);

        sumRGB += s * w;
        sumW   += w;
    }

    float3 blurred = sumRGB / max(sumW, 1e-5);
    float fill = saturate(PreblurStrength) * saturate((blurRadiusPx - 4.0) / 12.0);
    blurred = lerp(blurred, preblur.rgb, fill);



    res.rgb = max(0.0, blurred);
    res.a = 1.0;
    return res;
}
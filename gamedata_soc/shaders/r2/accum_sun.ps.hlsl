#include "common.hlsli"
#include "lmodel.hlsli"
#include "shadow.hlsli"

float4 main(float4 tc : TEXCOORD0, float4 tcJ : TEXCOORD1) : COLOR
{
    float4 _P = tex2Dproj(s_position, tc);
    float4 _N = tex2Dproj(s_normal, tc);
    _P.xyz += normalize(_N.xyz) * 0.015f;

    // ----- light-model
    float m = xmaterial;
#ifndef USE_R2_STATIC_SUN
    m = _P.w;
#endif
    float4 light = plight_infinity(m, _P, _N, Ldynamic_dir);

    // ----- shadow
    float4 P4 = float4(_P.x, _P.y, _P.z, 1.0f);
    float4 PS = mul(m_shadow, P4);
    float s = 1.0f;
#if SUN_QUALITY == 2
    #ifndef USE_FAR_ATTENTION
    s *= shadow_high(PS);
    #else
    s *= shadowtest_sun(PS, float4(0, 0, 0, 0));
    #endif
#else
    s *= shadow(PS);
#endif

#ifdef USE_FAR_ATTENTION
    // Far edge fading code
    float3 tc_f = abs(PS.xyz / PS.w - float3(0.5f, 0.5f, 0.5f));

    float3 border = 0.4f;
    float3 fac = 1.0f - saturate((tc_f - border) / (0.5f - border));

    s = lerp(s, 1.0f, 1.0f - fac.x * fac.y * fac.z);
#endif

    s *= sunmask(P4);
    return blend(Ldynamic_color * light * s, tc);
}

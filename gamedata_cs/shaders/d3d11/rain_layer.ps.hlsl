#include "common.hlsli"
#include "shadow.hlsli"

#ifndef USE_SUNMASK
float3x4 m_sunmask;
#endif

Texture2D s_water;

float4 main(float2 tc : TEXCOORD0, float2 tcJ : TEXCOORD1, float4 pos2d : SV_POSITION) : SV_Target
{
    float4 P = GbufferGetPoint(pos2d.xy);
    float4 PS = mul(m_shadow, P);

    float s = shadow(PS);
    float2 tc1 = mul(m_sunmask, P).xy;

    float4 water = s_water.SampleLevel(smp_linear, frac(tc1 * 0.5f), 0);

    water.xyz = (water.xzy - 0.5f) * 2.0f;
    water.xyz = mul((float3x3)m_V, water.xyz);
    water *= s;

    return float4(water.xyz, s * 0.5f);
}


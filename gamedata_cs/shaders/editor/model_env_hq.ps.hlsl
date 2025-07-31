#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float3 tc1 : TEXCOORD1; // environment
};

// Pixel
float4 main_ps_1_1(v2p I) : COLOR
{
    float4 t_base = tex2D(s_base, I.tc0);
    float4 t_env = texCUBE(s_env, I.tc1);

    // lighting

    // final-color
    float3 base = lerp(t_env, t_base, t_base.a);
    float3 final = base;

    // out
    return float4(final.r, final.g, final.b, t_base.a);
}

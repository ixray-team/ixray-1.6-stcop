#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float2 tc1 : TEXCOORD1; // lmap
    float2 tc2 : TEXCOORD2; // detail
};
uniform sampler2D s_mask; //

uniform sampler2D s_dt_r; //
uniform sampler2D s_dt_g; //
uniform sampler2D s_dt_b; //
uniform sampler2D s_dt_a; //

// Pixel
float4 main(v2p I) : COLOR
{
    float4 t_base = tex2D(s_base, I.tc0);

    float4 mask = tex2D(s_mask, I.tc0);
    float mag = dot(mask, 1);
    mask = mask / mag;

    float3 d_R = tex2D(s_dt_r, I.tc2) * mask.r;
    float3 d_G = tex2D(s_dt_g, I.tc2) * mask.g;
    float3 d_B = tex2D(s_dt_b, I.tc2) * mask.b;
    float3 d_A = tex2D(s_dt_a, I.tc2) * mask.a;
    float3 dt = d_R + d_G + d_B + d_A;
    t_base.xyz *= 2.0f * dt;

    // out
    return float4(t_base.xyz, 1);
}


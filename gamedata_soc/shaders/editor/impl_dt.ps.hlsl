#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float2 tc1 : TEXCOORD1; // lmap
    float2 tc2 : TEXCOORD2; // detail
};
Texture2D s_mask; //

Texture2D s_dt_r; //
Texture2D s_dt_g; //
Texture2D s_dt_b; //
Texture2D s_dt_a; //

// Pixel
float4 main(v2p I) : SV_Target
{
    float4 t_base = s_base.Sample(smp_base, I.tc0);

    float4 mask = s_mask.Sample(smp_base, I.tc0);
    float mag = dot(mask, 1);
    mask = mask / mag;

    float3 d_R = s_dt_r.Sample(smp_base, I.tc2) * mask.r;
    float3 d_G = s_dt_g.Sample(smp_base, I.tc2) * mask.g;
    float3 d_B = s_dt_b.Sample(smp_base, I.tc2) * mask.b;
    float3 d_A = s_dt_a.Sample(smp_base, I.tc2) * mask.a;
    float3 dt = d_R + d_G + d_B + d_A;
    t_base.xyz *= 2.0f * dt;

    // out
    return float4(t_base.xyz, 1);
}


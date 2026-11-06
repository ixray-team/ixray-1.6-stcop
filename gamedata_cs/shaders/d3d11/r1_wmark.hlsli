#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf_wmark
{
    float2 tc0 : TEXCOORD0;
    float4 c0 : COLOR0;
    float4 hpos : SV_POSITION;
};

float4 wmark_shift(float3 P, float3 N)
{
    float3 sd = eye_position - P;
    float d = length(sd);
    P += N * 0.007f;
    P -= normalize(eye_direction - sd * rcp(d)) * lerp(0.003f, 0.011f, d);
    return float4(P, 1.0f);
}

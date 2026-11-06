#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; 
    float3 tc1 : TEXCOORD1; 
    float2 tc2 : TEXCOORD2; 
    float3 c0 : COLOR0; 
    float4 c1 : COLOR1; 
};

float4 main(v2p I) : SV_Target
{
    float4 t_base = s_base.Sample(smp_base, I.tc0);
    float4 t_env = s_env.Sample(smp_rtlinear, I.tc1);

    float3 final = lerp(t_env, t_base, t_base.a);

    return float4(final.r, final.g, final.b, t_base.a);
}

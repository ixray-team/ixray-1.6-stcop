#include "r1_common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0;
    float4 c0 : COLOR0;
};

float4 main(v2p I) : SV_Target
{
    float4 t_base = s_base.Sample(smp_base, I.tc0);
    return float4(t_base.rgb, t_base.a * I.c0.a);
}

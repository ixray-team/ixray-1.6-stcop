#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0;
    float3 c0 : COLOR0;
    float fog : TEXCOORD7;
};

float4 main(v2p I) : SV_Target
{
    float4 t_base = r1_sample_base(I.tc0);

    float3 light = I.c0;
    float3 final = light * t_base.rgb * 2.0f;
    final = lerp(fog_color.xyz, final, I.fog);

    return float4(final.r, final.g, final.b, t_base.a);
}

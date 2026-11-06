#include "r1_common.hlsli"
#include "r1_static.hlsli"

float4 main(vf_spot I) : SV_Target
{
    float2 uv = I.tc1.xy / I.tc1.w;
    if (I.tc1.w <= 0.0f || any(uv != saturate(uv)) || I.tc2.x != saturate(I.tc2.x))
        discard;
    float4 t_base = r1_sample_base(I.tc0);
    float4 t_lmap = s_lmap.Sample(smp_rtlinear, uv);
    float4 t_att = s_att.Sample(smp_rtlinear, I.tc2);

    float4 final_color = t_base * t_lmap * t_att * I.color;
    final_color.rgb *= t_base.a;

    return final_color * 2;
}

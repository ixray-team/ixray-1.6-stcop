#include "r1_common.hlsli"
#include "r1_static.hlsli"

float4 main(vf_point I) : SV_Target
{
    if (any(I.tc1 != saturate(I.tc1)) || any(I.tc2 != saturate(I.tc2)))
        discard;
    float4 t_base = r1_sample_base(I.tc0);
    float4 final_color = t_base * I.color;
    final_color.rgb *= t_base.a;

    return final_color * 2.0f;
}

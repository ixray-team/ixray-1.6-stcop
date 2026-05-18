#include "common.hlsli"

// Pixel
float4 main(vf_point I) : COLOR
{
    float4 t_base = tex2D(s_base, I.tc0);
    float4 t_att1 = tex2D(s_lmap, I.tc1); // point-att
    float4 t_att2 = tex2D(s_att, I.tc2); // point-att
    float4 t_att = t_att1 * t_att2;

    float4 final_color = t_base * t_att * I.color;
    final_color.rgb *= t_base.a;
    float3 final_rgb = (final_color + final_color) * 2.0f;
    float final_a = final_color.w;

    // out
    return final_color * 2.0f; // float4	(final_rgb,final_a);
}

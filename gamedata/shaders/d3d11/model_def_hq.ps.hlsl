#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0;
    float4 tc1 : TEXCOORD1;
    float3 c0 : COLOR0;
    float4 c1 : COLOR1;
    float fog : TEXCOORD7;
};

float4 main(v2p I) : SV_Target
{
    float4 t_base = r1_sample_base(I.tc0);
    float2 uv = I.tc1.xy / I.tc1.w;
    float inside = I.tc1.w > 0.0f && all(uv == saturate(uv));
    float4 t_lmap = s_lmap.Sample(smp_rtlinear, saturate(uv));

    float3 l_base = t_lmap.rgb;
    float3 l_sun = I.c0 * t_lmap.a;
    // Outside the projector slot the clamped sample is some other object's
    // light. Fall back to the vertex light instead of that bright edge.
    float3 light = lerp(l_base + l_sun, I.c1.rgb, inside ? I.c1.w : 1.0f);

    float3 final = light * t_base.rgb * 2.0f;
    final = lerp(fog_color.xyz, final, I.fog);

    return float4(final.r, final.g, final.b, t_base.a);
}

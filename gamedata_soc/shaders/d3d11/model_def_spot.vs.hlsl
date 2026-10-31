#include "r1_common.hlsli"
#include "r1_static.hlsli"
#include "skin.hlsli"

vf_spot _main(v_model v)
{
    vf_spot o;

    float4 pos = v.P;
    float3 pos_w = mul(m_W, pos);
    float4 pos_w4 = float4(pos_w, 1);
    float3 norm_w = normalize(mul((float3x3)m_W, v.N));

    o.hpos = mul(m_WVP, pos);
    o.tc0 = v.tc.xy;
    o.color = calc_spot(o.tc1, o.tc2, pos_w4, norm_w);

    return o;
}

#define R1_SKIN_OUTPUT vf_spot
#include "r1_skin_main.hlsli"

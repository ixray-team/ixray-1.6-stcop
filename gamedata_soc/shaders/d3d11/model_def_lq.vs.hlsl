#include "r1_common.hlsli"
#include "r1_static.hlsli"
#include "skin.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float3 c0 : COLOR0;
    float fog : TEXCOORD7;
    float4 hpos : SV_POSITION;
};

vf _main(v_model v)
{
    vf o;

    float4 pos = v.P;
    float3 pos_w = mul(m_W, pos);
    float3 norm_w = normalize(mul((float3x3)m_W, v.N));

    o.hpos = mul(m_WVP, pos);
    o.tc0 = v.tc.xy;
    o.c0 = calc_model_lq_lighting(norm_w);
    o.fog = calc_fogging(float4(pos_w, 1));

#ifdef SKIN_COLOR
    o.c0.rgb *= v.rgb_tint;
#endif

    return o;
}

#define R1_SKIN_OUTPUT vf
#include "r1_skin_main.hlsli"

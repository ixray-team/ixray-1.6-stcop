#include "r1_common.hlsli"
#include "r1_static.hlsli"
#include "skin.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float4 c0 : COLOR0;
    float4 hpos : SV_POSITION;
};

vf _main(v_model v)
{
    vf o;
    o.hpos = mul(m_WVP, v.P);
    o.tc0 = v.tc.xy;
    float3 dir_v = normalize(mul(m_WV, v.P));
    float3 norm_v = normalize(mul((float3x3)m_WV, v.N));
    o.c0 = abs(dot(dir_v, norm_v));
    return o;
}

#define R1_SKIN_OUTPUT vf
#include "r1_skin_main.hlsli"

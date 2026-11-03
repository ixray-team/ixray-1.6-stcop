#include "r1_common.hlsli"
#include "r1_static.hlsli"
#include "skin.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float fog : TEXCOORD1;
    float4 hpos : SV_POSITION;
};

vf _main(v_model v)
{
    vf o;
    o.hpos = mul(m_WVP, v.P);
    o.tc0 = v.tc.xy;
    o.fog = r1_fog(mul(m_W, v.P));
    return o;
}

#define R1_SKIN_OUTPUT vf
#include "r1_skin_main.hlsli"

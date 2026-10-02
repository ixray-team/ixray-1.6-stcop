#include "r1_selflight.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float fog : TEXCOORD1;
    float4 hpos : SV_POSITION;
};

vf main(v_selflight v)
{
    vf o;
    o.hpos = mul(m_WVP, v.P);
    o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w);
    o.fog = r1_fog(mul(m_W, v.P));
    return o;
}

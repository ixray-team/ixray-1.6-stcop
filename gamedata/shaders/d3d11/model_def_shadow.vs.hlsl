#include "r1_common.hlsli"
#include "r1_static.hlsli"
#include "skin.hlsli"

struct vf
{
    float4 c0 : COLOR0;
    float4 hpos : SV_POSITION;
};

vf _main(v_model v)
{
    vf o;

    o.hpos = mul(m_WVP, v.P);
    o.c0 = 0;
    return o;
}

#define R1_SKIN_OUTPUT vf
#include "r1_skin_main.hlsli"

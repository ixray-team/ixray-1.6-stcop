#include "common.hlsli"
#include "skin.hlsli"

struct vf
{
    float4 hpos : POSITION;
    float2 tc0 : TEXCOORD0; // base
    float3 normal : TEXCOORD1; // view-space normal
};

vf _main(v_model v)
{
    vf o;

    o.hpos = mul(m_WVP, v.pos);
    o.tc0 = v.tc.xy;
    o.normal = mul((float3x3)m_WV, v.norm);

    return o;
}

#define SKIN_VF vf
#include "skin_main.hlsli"

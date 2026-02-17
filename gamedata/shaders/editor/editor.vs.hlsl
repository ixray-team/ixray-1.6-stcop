#include "common.hlsli"

struct vf
{
    float4 P : POSITION;
    float4 C : COLOR0;
};

struct v2p
{
    float4 C : COLOR0;
    float4 P : SV_POSITION;
};

uniform float4 tfactor;

v2p main(vf i)
{
    v2p o;

    o.P = mul(m_WVP, i.P); // xform, input in world coords
    o.C = tfactor * i.C.zyxw;

    return o;
}

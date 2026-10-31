#include "common.hlsli"

struct v2p
{
    float4 C : COLOR0;
    float4 P : SV_POSITION;
};

v2p main(PULL_INPUT)
{
    v2p o;
    RingVertex V = PullRing(VertexId);

    o.P = mul(m_WVP, float4(V.P, 1.0f)); // xform, input in world coords
    o.C = tfactor * V.C;

    return o;
}

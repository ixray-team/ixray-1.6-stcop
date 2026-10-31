#include "common.hlsli"

struct v2p
{
	float4 hpos : SV_POSITION;
	float4 color : COLOR0;
	float4 wp : TEXCOORD0;
};

v2p main(PULL_INPUT)
{
    v2p O;
    RingVertex V = PullRing(VertexId);
    O.wp = float4(V.P, 1.0f);
    O.hpos = mul(m_WVP, O.wp);
    O.color = V.C.zyxw;
    return O;
}

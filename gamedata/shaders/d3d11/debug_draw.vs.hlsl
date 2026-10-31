#include "common.hlsli"

// output
struct v2p_L
{
	float4 pos : SV_POSITION;
	float4 viewpos : TEXCOORD0;
	float4 color : COLOR0;
};

// Vertex
v2p_L main(PULL_INPUT)
{
    v2p_L O;
    RingVertex V = PullRing(VertexId);
	
    O.pos = mul(m_WVP, float4(V.P, 1.0f));
    O.viewpos = O.pos;
	
    O.color = V.C;
    return O;
}

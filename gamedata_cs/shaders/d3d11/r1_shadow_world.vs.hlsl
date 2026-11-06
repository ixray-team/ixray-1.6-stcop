#include "r1_common.hlsli"
struct vf { float4 color : COLOR0; float2 tc : TEXCOORD0; float4 hpos : SV_POSITION; };
vf main(PULL_INPUT) { vf O; RingVertex V = PullRing(VertexId); O.hpos = mul(m_WVP, float4(V.P, 1)); O.color = V.C; O.tc = V.UV; return O; }

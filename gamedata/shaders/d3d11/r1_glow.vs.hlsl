#include "r1_common.hlsli"
#ifndef R1_COLOR_SCALE
#define R1_COLOR_SCALE 1
#endif
struct vf { float4 hpos : SV_POSITION; float4 color : COLOR0; float2 tc : TEXCOORD0; };
vf main(PULL_INPUT) { vf O; RingVertex V = PullRing(VertexId); O.hpos = mul(m_WVP, float4(V.P, 1)); O.color = V.C; O.color.rgb *= R1_COLOR_SCALE; O.tc = V.UV; return O; }

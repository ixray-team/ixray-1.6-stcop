#include "r1_common.hlsli"
#include "r1_static.hlsli"
struct vf {
    float2 tc : TEXCOORD0; float4 color : COLOR0; float fog : TEXCOORD7; float4 hpos : SV_POSITION;
};
vf main(PULL_INPUT) { vf O; RingVertex V = PullRing(VertexId); O.hpos = mul(m_WVP, float4(V.P, 1)); O.tc = V.UV; O.color = V.C; O.fog = r1_fog(V.P); O.color.a *= O.fog; return O; }

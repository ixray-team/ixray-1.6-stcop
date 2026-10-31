#include "r1_common.hlsli"
#include "r1_static.hlsli"
struct vi { float4 pos : POSITION; float2 tc : TEXCOORD0; float4 color : COLOR0; };
struct vf {
    float2 tc : TEXCOORD0; float4 color : COLOR0; float fog : TEXCOORD7; float4 hpos : SV_POSITION;
};
vf main(vi I) { vf O; O.hpos = mul(m_WVP, I.pos); O.tc = I.tc; O.color = I.color.bgra; O.fog = r1_fog(I.pos.xyz); O.color.a *= O.fog; return O; }

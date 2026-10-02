#include "r1_common.hlsli"
struct vi { float3 pos : POSITION; float4 color : COLOR0; float2 tc : TEXCOORD0; };
struct vf { float4 color : COLOR0; float2 tc : TEXCOORD0; float4 hpos : SV_POSITION; };
vf main(vi I) { vf O; O.hpos = mul(m_WVP, float4(I.pos, 1)); O.color = I.color.bgra; O.tc = I.tc; return O; }

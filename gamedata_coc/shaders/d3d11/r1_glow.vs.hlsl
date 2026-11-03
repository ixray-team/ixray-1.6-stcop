#include "r1_common.hlsli"
#ifndef R1_COLOR_SCALE
#define R1_COLOR_SCALE 1
#endif
struct vi { float3 pos : POSITION; float4 color : COLOR0; float2 tc : TEXCOORD0; };
struct vf { float4 hpos : SV_POSITION; float4 color : COLOR0; float2 tc : TEXCOORD0; };
vf main(vi I) { vf O; O.hpos = mul(m_WVP, float4(I.pos, 1)); O.color = I.color.bgra; O.color.rgb *= R1_COLOR_SCALE; O.tc = I.tc; return O; }

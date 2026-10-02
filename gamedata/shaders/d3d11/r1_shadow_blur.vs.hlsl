#include "r1_common.hlsli"
struct vi { float4 pos : POSITIONT; float4 color : COLOR0; float2 tc0 : TEXCOORD0; float2 tc1 : TEXCOORD1; };
struct vf { float2 tc0 : TEXCOORD0; float2 tc1 : TEXCOORD1; float4 hpos : SV_POSITION; };
vf main(vi I) { vf O; O.hpos = float4((I.pos.xy + .5f) / 512.f * float2(2, -2) + float2(-1, 1), I.pos.zw); O.tc0 = I.tc0; O.tc1 = I.tc1; return O; }

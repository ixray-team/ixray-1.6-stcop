#include "r1_common.hlsli"
struct vf { float4 hpos : SV_POSITION; float4 color : COLOR0; float2 tc : TEXCOORD0; };
float4 main(vf I) : SV_Target { return saturate(s_base.Sample(smp_base, I.tc) + I.color); }

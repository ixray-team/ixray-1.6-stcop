#include "r1_common.hlsli"
struct vf { float4 hpos : SV_POSITION; float2 tc0 : TEXCOORD0; float2 tc1 : TEXCOORD1; };
float4 main(vf I) : SV_Target { return (s_base.Sample(smp_rtlinear, I.tc0) + s_base.Sample(smp_rtlinear, I.tc1)) * (127.f / 255.f); }

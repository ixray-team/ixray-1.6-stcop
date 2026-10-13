#include "r1_common.hlsli"
#include "r1_static.hlsli"
struct vf { float2 tc : TEXCOORD0; float4 color : COLOR0; };
float4 main(vf I) : SV_Target { float4 color = I.color * r1_sample_base(I.tc); clip(color.a - .01f / 255.f); return color; }

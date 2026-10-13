#include "r1_common.hlsli"
#include "r1_static.hlsli"
struct vf { float4 hpos : SV_POSITION; float4 color : COLOR0; float2 tc : TEXCOORD0; };
float4 main(vf I) : SV_Target { return I.color * r1_sample_base(I.tc); }

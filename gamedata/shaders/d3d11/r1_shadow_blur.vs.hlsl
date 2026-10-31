#include "r1_common.hlsli"
struct vf { float2 tc0 : TEXCOORD0; float2 tc1 : TEXCOORD1; float4 hpos : SV_POSITION; };
vf main(uint VertexId : SV_VertexID) { vf O; float2 UV = float2((VertexId << 1u) & 2u, VertexId & 2u); O.hpos = float4(UV * float2(2, -2) + float2(-1, 1), 0, 1); O.tc0 = UV - .75f / 512.f; O.tc1 = UV + .75f / 512.f; return O; }

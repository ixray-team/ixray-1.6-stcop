#include "common.hlsli"

struct v2p
{
    float4 tc0 : TEXCOORD0; // tc.xy, tc.w = tonemap scale
    float2 tcJ : TEXCOORD1; // jitter coords
    float4 hpos : SV_POSITION;
};

// Vertex
v2p main(uint VertexId : SV_VertexID)
{
    v2p O;
    float2 UV = float2((VertexId << 1u) & 2u, VertexId & 2u);
    O.hpos = float4(UV * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f), 0.0f, 1.0f);
    O.tc0 = float4(UV, 1.0f, 1.0f);
    O.tcJ = UV * scaled_screen_res.xy / 64.0f; // TEX_jitter
    return O;
}

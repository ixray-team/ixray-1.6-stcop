#include "common.hlsli"

// Material preview of the Thumbnail View: procedural mesh in world space
struct v2p_material_preview
{
    float4 hpos : SV_POSITION;
    float2 tc : TEXCOORD0;
    float3 wpos : TEXCOORD1;
    float3 N : TEXCOORD2;
    float3 T : TEXCOORD3;
    float3 B : TEXCOORD4;
};

v2p_material_preview main(PULL_INPUT)
{
    PulledVertex I = PullVertex(VertexId);
    v2p_material_preview O;
    O.hpos = mul(m_WVP, float4(I.P, 1.0f));
    O.wpos = I.P;
    O.tc = I.UV;
    O.N = I.N;
    O.T = I.T;
    O.B = I.B;
    return O;
}

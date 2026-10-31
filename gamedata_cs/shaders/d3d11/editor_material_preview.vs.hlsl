#include "common.hlsli"

// Material preview of the Thumbnail View: procedural mesh in world space
struct v_material_preview
{
    float3 P : POSITION;
    float3 N : NORMAL;
    float3 T : TANGENT;
    float3 B : BINORMAL;
    float2 tc : TEXCOORD0;
};

struct v2p_material_preview
{
    float4 hpos : SV_POSITION;
    float2 tc : TEXCOORD0;
    float3 wpos : TEXCOORD1;
    float3 N : TEXCOORD2;
    float3 T : TEXCOORD3;
    float3 B : TEXCOORD4;
};

v2p_material_preview main(v_material_preview I)
{
    v2p_material_preview O;
    O.hpos = mul(m_WVP, float4(I.P, 1.0f));
    O.wpos = I.P;
    O.tc = I.tc;
    O.N = I.N;
    O.T = I.T;
    O.B = I.B;
    return O;
}

#include "common.hlsli"

struct VSOutput
{
    float4 hpos : SV_Position;
    float4 hpos2d : TEXCOORD0;
};


VSOutput main(PULL_INPUT)
{
    VSOutput O;

    O.hpos = mul(m_WVP, float4(PullVertex(VertexId).P, 1.0));
    O.hpos2d = O.hpos; //No transformation

    return O;
}

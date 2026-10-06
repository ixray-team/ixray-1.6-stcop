#include "common.hlsli"

struct v2p
{
    float2 Tex0 : TEXCOORD0;
    float3 World : TEXCOORD1;
    float4 Color : COLOR;
    float4 HPos : SV_POSITION;
};

void main(in v_TL I, out v2p O)
{
    O.HPos = mul(m_WVP, I.P);
    O.World = mul(m_W, I.P);
    O.Tex0 = I.Tex0;
    O.Color = I.Color.bgra;
}

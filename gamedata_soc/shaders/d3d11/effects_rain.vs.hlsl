#include "common.hlsli"

struct v2p
{
    float2 Tex0 : TEXCOORD0;
    float3 Point : TEXCOORD1;
	
    float4 Color : COLOR;
    float4 HPos : SV_POSITION;
};

// Vertex
void main(in v_TL I, out v2p O)
{
	O.Point = mul(m_WV, I.P);
    O.HPos = mul(m_WVP, I.P);
	
    O.HPos.xy += m_taa_jitter.xy * O.HPos.w;
	
    O.Tex0 = I.Tex0;
    O.Color = I.Color.bgra;
}


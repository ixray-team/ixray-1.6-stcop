#include "common.hlsli"

struct v_vert
{
    float4 pos : POSITION;
    float4 color : COLOR0;
};

struct v2p
{
	float4 hpos : SV_POSITION;
	float4 color : COLOR0;
	float4 wp : TEXCOORD0;
};

v2p main(v_vert I)
{
    v2p O;
    O.hpos = mul(m_WVP, I.pos);
    O.wp = I.pos;
    O.color = I.color;
    return O;
}
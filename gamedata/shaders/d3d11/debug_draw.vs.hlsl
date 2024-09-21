#include "common.hlsli"

// input
struct v_vert
{
    float4 pos : POSITION; // (float,float,float,1)
    float4 color : COLOR0; // (r,g,b,dir-occlusion)
};

// output
struct v2p_L
{
	float4 pos : SV_POSITION;
	float4 viewpos : TEXCOORD0;
	float4 color : COLOR0;
};

// Vertex
v2p_L main(v_vert I)
{
    v2p_L O;
    O.pos = mul(m_WVP, I.pos);
    O.viewpos = float4(mul(m_WV, I.pos), 1.0f);
    O.color = I.color.bgra; //	swizzle vertex colour
    return O;
}

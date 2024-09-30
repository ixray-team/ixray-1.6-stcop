#include "common.hlsli"

struct vv
{
    float4 P : POSITION;
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;
};

struct v2p_particle
{
    float4 color : COLOR0;
    float4 hpos : SV_POSITION;
};

void main(in vv I, out v2p_particle pp)
{
    pp.color = I.c;
	
    pp.hpos = mul(m_WVP, I.P);
    pp.hpos.xy += m_taa_jitter.xy * pp.hpos.w;
}

// THIS SHADER SHOD BE DELEATED OR FIXED
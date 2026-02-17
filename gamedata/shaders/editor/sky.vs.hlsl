#include "common.hlsli"

struct vi
{
    float4 p : POSITION;
    float4 c : COLOR0;
	
    float3 tc0 : TEXCOORD0;
    float3 tc1 : TEXCOORD1;
};

struct v2p
{
    float4 factor : COLOR0;
    float3 p : TEXCOORD1;

    float4 hpos : SV_POSITION;
};

void main(in vi v, out v2p o)
{
    o.hpos = mul(m_WVP, v.p);
	
    o.factor = v.c;
    o.p = v.p.xyz;
}


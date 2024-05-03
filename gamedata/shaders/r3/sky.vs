#include "common.h"

struct vi
{
	float4	p : POSITION;
	float4	c : COLOR0;
	float3	tc0 : TEXCOORD0;
	float3	tc1 : TEXCOORD1;
};

struct v2p
{
	float4	factor : COLOR0;
	
	float3	tc0 : TEXCOORD0;
	float3	tc1 : TEXCOORD1;
	
	float4	hpos_curr : TEXCOORD2;
	float4	hpos_old : TEXCOORD3;
	
	float4	hpos : SV_Position;
};

v2p main (vi v)
{
	v2p o;

	//float4 tpos = mul (1000, v.p);
	o.hpos = mul (m_WVP, v.p);
//	o.hpos.z = o.hpos.w;
	o.tc0 = v.tc0;
	o.tc1 = v.tc1;

	o.factor = v.c;
	o.hpos.xy += m_taa_jitter.xy * o.hpos.w;
	
	o.hpos_curr = mul(m_WVP, v.p);
	o.hpos_old = mul(m_WVP_old, v.p);

	return o;
}
#include "common\common.hlsli"

// output
struct v2p_L
{
	float4 pos : SV_POSITION;
	float4 viewpos : TEXCOORD0;
	float4 color : COLOR0;
};

// Vertex
v2p_L vs_main(PULL_INPUT)
{
    v2p_L O;
    RingVertex V = PullRing(VertexId);
	
    O.pos = mul(m_WVP, float4(V.P, 1.0f));
    O.viewpos = O.pos;
	
    O.color = V.C;
    return O;
}

// vertex output

float4 ps_main(v2p_L I) : SV_TARGET
{
	float depth = s_position.SampleLevel(smp_nofilter, I.pos.xy * screen_res.zw, 0).x;
	clip(depth - I.viewpos.z / I.viewpos.w);
	
	return I.color;
}

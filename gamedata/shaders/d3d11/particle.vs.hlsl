#include "common.hlsli"

struct v2p
{
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;

    float3 tctexgen : TEXCOORD1;
    float4 hpos : SV_POSITION;
    float fog : FOG;
};


v2p main(PULL_INPUT)
{
    v2p o;
    RingVertex V = PullRing(VertexId);
    float4 P = float4(V.P, 1.0f);

    o.hpos = mul(m_WVP, P); // xform, input in world coords
    o.tc = V.UV;
    o.c = V.C;

    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;
    o.tctexgen.xyz = mul(m_WV, P).xyz;
	
    o.fog = 1.0f - calc_fogging(V.P); // fog, input in world coords
    return o;
}

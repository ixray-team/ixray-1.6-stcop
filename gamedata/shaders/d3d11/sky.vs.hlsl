#include "common.hlsli"

struct v2p
{
    float4 factor : COLOR0;
    float3 p : TEXCOORD1;

#ifndef DISABLE_MOTION_VECTORS
    float4 hpos_curr : TEXCOORD2;
    float4 hpos_old : TEXCOORD3;
#endif

    float4 hpos : SV_POSITION;
};

void main(PULL_INPUT, out v2p o)
{
    RingVertex V = PullRing(VertexId);
    float4 P = float4(V.P, 1.0f);

    o.hpos = mul(m_WVP, P);
	
    o.factor = V.C.zyxw;
    o.p = V.P;

#ifndef DISABLE_MOTION_VECTORS
    o.hpos_curr = o.hpos;
    o.hpos_old = mul(m_WVP_old, P);
#endif
	
    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;
}

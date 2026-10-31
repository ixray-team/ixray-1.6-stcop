#include "r1_wmark.hlsli"

vf_wmark main(PULL_INPUT)
{
    vf_wmark o;
    PulledVertex V = PullVertex(VertexId);
    float3 N = V.N;
    float4 C = float4(DecodeU8x4(V.X).xyz, DecodeU8x4(V.C).y);
    float4 P = wmark_shift(mul(m_W, float4(V.P, 1.0f)), N);
    o.hpos = mul(m_VP, P);
    o.tc0 = V.UV;
    o.c0.rgb = C.rgb + r1_v_hemi(N) * DecodeU8x4(V.C).x + r1_v_sun(N) * C.w + L_ambient.xyz;
    o.c0.a = r1_fog(P.xyz);
    return o;
}

#include "r1_water.hlsli"

vf_waterd main(PULL_INPUT)
{
    vf_waterd o;
    PulledVertex V = PullVertex(VertexId);
    float4 P = watermove(float4(V.P, 1.0f));
    o.tbase = V.UV;
    o.tdist0 = watermove_tc(o.tbase * W_DISTORT_BASE_TILE_0, P.xz, W_DISTORT_AMP_0);
    o.tdist1 = watermove_tc(o.tbase * W_DISTORT_BASE_TILE_1, P.xz, W_DISTORT_AMP_1);
    o.hpos = mul(m_VP, P);
    return o;
}

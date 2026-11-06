#include "r1_water.hlsli"

vf_water main(PULL_INPUT)
{
    vf_water o;
    PulledVertex V = PullVertex(VertexId);
    float4 P = watermove(float4(V.P, 1.0f));
    float3 N = V.N;
    float3 T = V.T;
    float3 B = V.B;
    float4 C = float4(DecodeU8x4(V.X).xyz, DecodeU8x4(V.C).y);

    o.v2point = P.xyz - eye_position;
    o.tbase = V.UV;
    o.tnorm0 = watermove_tc(o.tbase * W_DISTORT_BASE_TILE_0, P.xz, W_DISTORT_AMP_0);
    o.tnorm1 = watermove_tc(o.tbase * W_DISTORT_BASE_TILE_1, P.xz, W_DISTORT_AMP_1);

    float3x3 xform = mul((float3x3)m_W, float3x3(T.x, B.x, N.x, T.y, B.y, N.y, T.z, B.z, N.z));
    o.M1 = xform[0];
    o.M2 = xform[1];
    o.M3 = xform[2];

    float3 L = C.rgb + r1_v_hemi(N) * DecodeU8x4(V.C).x + r1_v_sun(N) * C.w + L_ambient.xyz;
    o.hpos = mul(m_VP, P);
    o.c0 = float4(L, r1_fog(V.P));
    return o;
}

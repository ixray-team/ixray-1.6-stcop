#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
    float4 c0 : COLOR0;
    float4 hpos : SV_POSITION;
};

vf main(PULL_INPUT)
{
    vf o;
    PulledVertex V = PullVertex(VertexId);

    float3 N = V.N;
    float3 L_rgb = DecodeU8x4(V.X).xyz;
    float3 L_hemi = r1_v_hemi(N) * DecodeU8x4(V.C).x;
    float L_sun = DecodeU8x4(V.C).y;
    float3 L_final = L_rgb + L_hemi + L_ambient.xyz;

    o.hpos = mul(m_WVP, float4(V.P, 1.0f));
    o.c0 = float4(L_final.x, L_final.y, L_final.z, L_sun);
    return o;
}

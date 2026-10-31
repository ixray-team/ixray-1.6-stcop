#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float4 c0 : COLOR0;
    float4 c1 : COLOR1;
    float fog : TEXCOORD7;
    float4 hpos : SV_POSITION;
};

vf main(PULL_INPUT)
{
    vf o;
    PulledVertex V = PullVertex(VertexId);

    float3 N = V.N;
    o.hpos = mul(m_WVP, float4(V.P, 1.0f));
    o.tc0 = V.UV;

    o.tc1 = o.tc0 * dt_params.xy;

    float3 L_rgb = DecodeU8x4(V.X).xyz;
    float3 L_hemi = r1_v_hemi(N) * DecodeU8x4(V.C).x;
    float3 L_sun = r1_v_sun(N) * DecodeU8x4(V.C).y;
    float3 L_final = L_rgb + L_hemi + L_sun + L_ambient.xyz;

    float2 dt = calc_detail(mul(m_W, float4(V.P, 1.0f)));

    o.c0 = float4(L_final.x, L_final.y, L_final.z, dt.x);
    o.c1 = dt.y;
    o.fog = r1_fog(mul(m_W, float4(V.P, 1.0f)));

    return o;
}

#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float2 tc2 : TEXCOORD2;
    float4 c0 : COLOR0; 
    float4 c1 : COLOR1; 
    float fog : TEXCOORD7;
    float4 hpos : SV_POSITION;
};

vf main(PULL_INPUT)
{
    vf o;
    PulledVertex V = PullVertex(VertexId);

    float2 dt = calc_detail(V.P);
    float3 N = V.N;
    o.hpos = mul(m_VP, float4(V.P, 1.0f)); 
    o.tc0 = V.UV; 
    o.tc1 = o.tc0; 
    o.tc2 = o.tc0 * dt_params.xy; 
    o.c0 = float4(r1_v_hemi(N), dt.x); 
    o.c1 = float4(r1_v_sun(N), dt.y); 
    o.fog = calc_fogging(float4(V.P, 1.0f)); 

    return o;
}

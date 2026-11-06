#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float2 tch : TEXCOORD2;
    float3 tc2 : TEXCOORD3;
    float3 c0 : COLOR0; 
    float3 c1 : COLOR1; 
    float fog : TEXCOORD7;
    float4 hpos : SV_POSITION;
};

vf main(PULL_INPUT)
{
    vf o;
    PulledVertex V = PullVertex(VertexId);

    float3 pos_w = V.P;
    float3 norm_w = V.N;

    o.hpos = mul(m_VP, float4(V.P, 1.0f)); 
    o.tc0 = V.UV; 
    
    o.tc1 = unpack_tc_lmap(DecodeS16x2(V.X)); 
    o.tch = o.tc1;
    o.tc2 = calc_reflection(pos_w, norm_w);
    o.c0 = r1_v_hemi(norm_w); 
    o.c1 = r1_v_sun(norm_w); 
    o.fog = calc_fogging(float4(V.P, 1.0f)); 

    return o;
}

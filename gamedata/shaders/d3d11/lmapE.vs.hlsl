#define USE_LM_HEMI
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

vf main(v_static v)
{
    vf o;

    float3 pos_w = v.P;
    float3 norm_w = normalize(unpack_normal(v.Nh.xyz));

    o.hpos = mul(m_VP, v.P); 
    o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w); 
    
    o.tc1 = unpack_tc_lmap(v.lmh); 
    o.tch = o.tc1;
    o.tc2 = calc_reflection(pos_w, norm_w);
    o.c0 = r1_v_hemi(norm_w); 
    o.c1 = r1_v_sun(norm_w); 
    o.fog = calc_fogging(v.P); 

    return o;
}

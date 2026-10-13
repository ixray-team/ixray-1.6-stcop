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

vf main(v_static v)
{
    vf o;

    float2 dt = calc_detail(v.P.xyz);
    float3 N = unpack_normal(v.Nh.xyz);
    o.hpos = mul(m_VP, v.P); 
    o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w); 
    o.tc1 = o.tc0; 
    o.tc2 = o.tc0 * dt_params.xy; 
    o.c0 = float4(r1_v_hemi(N), dt.x); 
    o.c1 = float4(r1_v_sun(N), dt.y); 
    o.fog = calc_fogging(v.P); 

    return o;
}

#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float3 c0 : COLOR0;
    float4 hpos : SV_POSITION;
};

vf main(v_static v)
{
    vf o;

    o.hpos = mul(m_VP, v.P); 
    o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w); 
    o.tc1 = o.tc0; 
    o.c0 = r1_v_hemi(unpack_normal(v.Nh.xyz)); 

    return o;
}

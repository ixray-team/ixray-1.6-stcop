#include "common.hlsli"
#include "skin.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0; // base
    float3 tc1 : TEXCOORD1; // view-space position
    float4 c0 : COLOR0; // color
    float4 hpos : SV_POSITION;
    float fog : FOG;
};

vf _main(v_model v)
{
    vf o;

    o.hpos = mul(m_WVP, v.P); // xform, input in world coords
    o.tc0 = v.tc.xy; // copy tc
    o.tc1 = mul(m_WV, v.P).xyz;

    // calculate fade
    float3 dir_v = normalize(o.tc1);
    float3 norm_v = normalize(mul((float3x3)m_WV, v.N.xyz));
    float fade = 1 - abs(dot(dir_v, norm_v));
    o.c0 = fade;
    o.fog = 1.0f - calc_fogging(float4(mul(m_W, v.P), 1.0f).xyz); // fog, input in world coords

    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;
	
    return o;
}

vf main(PULL_INPUT)
{
    return _main(PullModel(VertexId));
}

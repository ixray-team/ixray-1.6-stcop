#include "common.hlsli"

struct vv
{
    float4 P : POSITION;
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;
};

struct v2p
{
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;

    float3 tctexgen : TEXCOORD1;
    float4 hpos : SV_POSITION;
    float fog : FOG;
};


v2p main(vv v)
{
    v2p o;

    o.hpos = mul(m_WVP, v.P); // xform, input in world coords
    //	o.hpos 		= mul	(m_VP, v.P);		// xform, input in world coords
    o.tc = v.tc; // copy tc
    o.c = unpack_D3DCOLOR(v.c); // copy color

    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;
    o.tctexgen.xyz = mul(m_WV, v.P).xyz;
	
    o.fog = 1.0f - calc_fogging(v.P.xyz); // fog, input in world coords
    return o;
}


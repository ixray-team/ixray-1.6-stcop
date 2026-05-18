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

//	Igor: for additional depth dest
#ifdef USE_SOFT_PARTICLES
    float4 tctexgen : TEXCOORD1;
#endif //	USE_SOFT_PARTICLES

    float4 hpos : SV_POSITION;
    float fog : FOG;
};

uniform float4x4 mVPTexgen;

v2p main(vv v)
{
    v2p o;

    o.hpos = mul(m_WVP, v.P); // xform, input in world coords
    //	o.hpos 		= mul	(m_VP, v.P);		// xform, input in world coords
    o.tc = v.tc; // copy tc
    o.c = unpack_D3DCOLOR(v.c); // copy color

    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;
//	Igor: for additional depth dest
#ifdef USE_SOFT_PARTICLES
    o.tctexgen = mul(mVPTexgen, v.P);
    o.tctexgen.z = mul(m_WV, v.P).z;
#endif //	USE_SOFT_PARTICLES

    o.fog = 1.0f - calc_fogging(v.P.xyz); // fog, input in world coords
    return o;
}


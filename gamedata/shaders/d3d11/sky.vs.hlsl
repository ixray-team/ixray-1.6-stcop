#include "common.hlsli"

struct vi
{
    float4 p : POSITION;
    float4 c : COLOR0;
	
    float3 tc0 : TEXCOORD0;
    float3 tc1 : TEXCOORD1;
};

struct v2p
{
    float4 factor : COLOR0;
    float3 p : TEXCOORD1;

    float4 hpos_curr : TEXCOORD2;
    float4 hpos_old : TEXCOORD3;

    float4 hpos : SV_POSITION;
#ifdef USE_PROCEDURAL_SKY_VIEW

    // World-space view direction associated with the skybox vertex.
    float3 world_direction : TEXCOORD4;

#endif
};

void main(in vi v, out v2p o)
{
    o.hpos = mul(m_WVP, v.p);
	
    o.factor = v.c;
    o.p = v.p.xyz;

    o.hpos_curr = o.hpos;
    o.hpos_old = mul(m_WVP_old, v.p);
    #ifdef USE_PROCEDURAL_SKY_VIEW

    // m_W contains sky_rotation and camera translation.
    // w=0 removes translation and leaves only the direction rotation.
    o.world_direction = mul(m_W, float4(v.p.xyz, 0.0f));

#endif
	
    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;
}


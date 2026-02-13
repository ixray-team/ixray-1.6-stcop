#include "common.hlsli"

cbuffer VolumetricLights
{
    float3 vMinBounds;
    float3 vMaxBounds;
    float4 FrustumClipPlane[6];
}

struct v2p
{
    float3 lightToPos : TEXCOORD0; // light center to plane vector
    float3 vPos : TEXCOORD1; // position in camera space
    float fDensity : TEXCOORD2; // plane density alon Z axis
    float4 hpos : SV_POSITION;
};

v2p main(float3 P : POSITION)
{
    v2p o;
    float4 vPos = 1.0f;
    vPos.xyz = lerp(vMinBounds, vMaxBounds, P);
	
    o.hpos = mul(m_P, vPos);

    o.lightToPos = vPos.xyz - Ldynamic_pos.xyz;
    o.vPos = vPos.xyz;

    o.fDensity = 0.025f;

    return o;
}


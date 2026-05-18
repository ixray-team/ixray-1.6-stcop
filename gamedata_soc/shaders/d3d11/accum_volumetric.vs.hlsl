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
    float3 clip0 : SV_ClipDistance0;
    float3 clip1 : SV_ClipDistance1;
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

	o.clip0.x = dot(o.hpos, FrustumClipPlane[0]);
	o.clip0.y = dot(o.hpos, FrustumClipPlane[1]);
	o.clip0.z = dot(o.hpos, FrustumClipPlane[2]);
	
	o.clip1.x = dot(o.hpos, FrustumClipPlane[3]);
	o.clip1.y = dot(o.hpos, FrustumClipPlane[4]);
	o.clip1.z = dot(o.hpos, FrustumClipPlane[5]);

    return o;
}


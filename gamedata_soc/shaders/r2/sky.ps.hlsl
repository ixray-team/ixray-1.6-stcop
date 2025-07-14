#include "common.hlsli"

struct v2p
{
    float4 factor : COLOR0;
    float3 tc0 : TEXCOORD0;
    float3 tc1 : TEXCOORD1;
};

uniform samplerCUBE s_sky0 : register(s0);
uniform samplerCUBE s_sky1 : register(s1);

float4 main(v2p I) : COLOR0
{
    float3 s0 = texCUBE(s_sky0, I.tc0);
    float3 s1 = texCUBE(s_sky1, I.tc1);
    float3 sky = L_sky_color.xyz * lerp(s0, s1, I.factor.w);

    return sky.xyzz;
}

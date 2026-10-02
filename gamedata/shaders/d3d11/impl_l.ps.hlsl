#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; 
    float2 tc1 : TEXCOORD1; 
    float3 c0 : COLOR0; 
};

float4 main(v2p I) : SV_Target
{
    float4 t_base = s_base.Sample(smp_base, I.tc0);
    float4 t_lmap = s_lmap.Sample(smp_base, I.tc1);

    float3 l_base = t_lmap.rgb; 
    float3 l_hemi = I.c0 * t_base.a; 
    float l_sun = t_lmap.a; 
    float3 light = L_ambient.xyz + l_base + l_hemi;
    return float4(light, l_sun);
}

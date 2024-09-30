#include "common.hlsli"

Texture2D s_patched_normal;

float4 main(float2 tc : TEXCOORD0) : SV_Target
{
    float3 Normal = s_patched_normal.Sample(smp_nofilter, tc).xyz;
    Normal.z = -Normal.z;
    Normal = saturate(Normal.xyz * 0.5 + 0.5);

    return float4(Normal, 1.0f);
}


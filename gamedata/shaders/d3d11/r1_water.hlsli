#include "r1_common.hlsli"
#include "r1_static.hlsli"
#include "shared\waterconfig.hlsli"
#include "shared\watermove.hlsli"

struct vf_water
{
    float2 tbase : TEXCOORD0;
    float2 tnorm0 : TEXCOORD1;
    float2 tnorm1 : TEXCOORD2;
    float3 M1 : TEXCOORD3;
    float3 M2 : TEXCOORD4;
    float3 M3 : TEXCOORD5;
    float3 v2point : TEXCOORD6;
    float4 c0 : COLOR0;
    float4 hpos : SV_POSITION;
};

struct vf_waterd
{
    float2 tbase : TEXCOORD0;
    float2 tdist0 : TEXCOORD1;
    float2 tdist1 : TEXCOORD2;
    float4 hpos : SV_POSITION;
};

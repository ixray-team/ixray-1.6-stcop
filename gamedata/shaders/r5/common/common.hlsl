
#ifndef COMMON_HLSL
#define COMMON_HLSL

struct InputUI
{
    float3 Position : POSITION0;
    float4 Color    : COLOR0;
    float2 UV       : TEXCOORD0;
};

struct OutputUI
{
    float4 Position     : SV_Position;
    float4 Color        : COLOR0;
    float2 UV           : TEXCOORD0;
    uint   InstanceID   : ID0;
};

#endif

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


struct InputLegacySceneVertex
{
    float3 Position : POSITION0;
    float4 Normal   : TEXCOORD0;
    float4 Tangent  : TEXCOORD1;
    float4 Binormal : TEXCOORD2;
    float4 Color    : COLOR0;
    int2 UV         : TEXCOORD3;
};

struct OutputLegacySceneVertex
{
    float4 Position : SV_Position;
    float3 Normal   : TEXCOORD0;
    float3 Tangent  : TEXCOORD1;
    float3 Binormal : TEXCOORD2;
    float4 Color    : COLOR0;
    float2 UV       : TEXCOORD3;
    uint   InstanceID   : ID0;
};


struct InputLegacySceneLMap
{
    float3 Position : POSITION0;
    float4 Normal   : TEXCOORD0;
    float4 Tangent  : TEXCOORD1;
    float4 Binormal : TEXCOORD2;
    int2 UV0        : TEXCOORD3;
    int2 UV1        : TEXCOORD4;
};

struct OutputLegacySceneLMap
{
    float4 Position : SV_Position;
    float3 Normal   : TEXCOORD0;
    float3 Tangent  : TEXCOORD1;
    float3 Binormal : TEXCOORD2;
    float2 UV0      : TEXCOORD3;
    float2 UV1       : TEXCOORD4;
    uint   InstanceID   : ID0;
};


float2 unpack_tc_base(float2 tc, float du, float dv)
{
	return (tc.xy + float2(du, dv)) * (32.f / 32768.f); //!Increase from 32bit to 64bit floating point
}
float3 	unpack_normal(float3 v) { return 2 * v - 1; }
float2 	unpack_tc_lmap(float2 tc) { return tc * (1.f / 32768.f); } // [-1  .. +1 ]


#endif
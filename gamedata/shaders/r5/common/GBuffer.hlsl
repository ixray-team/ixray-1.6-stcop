#ifndef TIRAMISU_GBUFFER_HLSL
#define TIRAMISU_GBUFFER_HLSL

#define TIRAMISU_GBUFFER_VERSION 1u

struct TiramisuGBufferData
{
    float3 BaseColor;
    float AmbientOcclusion;
    float3 Normal;
    float Roughness;
    float Metallic;
    float3 Emissive;
    uint MaterialFlags;
    float2 Velocity;
};

struct TiramisuGBufferTargets
{
    float4 BaseColorAO;
    float4 NormalRoughnessMetallic;
    float4 EmissiveMaterialFlags;
    float2 Velocity;
};

float2 EncodeOctahedralNormal(float3 Normal)
{
    Normal *= rcp(max(abs(Normal.x) + abs(Normal.y) + abs(Normal.z), 1.0e-6f));
    if (Normal.z < 0.0f)
    {
        const float2 SignNotZero = float2(Normal.x >= 0.0f ? 1.0f : -1.0f,
            Normal.y >= 0.0f ? 1.0f : -1.0f);
        Normal.xy = (1.0f - abs(Normal.yx)) * SignNotZero;
    }
    return Normal.xy * 0.5f + 0.5f;
}

float3 DecodeOctahedralNormal(float2 Encoded)
{
    const float2 Octahedral = Encoded * 2.0f - 1.0f;
    float3 Normal = float3(Octahedral, 1.0f - abs(Octahedral.x) - abs(Octahedral.y));
    if (Normal.z < 0.0f)
    {
        const float2 SignNotZero = float2(Normal.x >= 0.0f ? 1.0f : -1.0f,
            Normal.y >= 0.0f ? 1.0f : -1.0f);
        Normal.xy = (1.0f - abs(Normal.yx)) * SignNotZero;
    }
    return normalize(Normal);
}

float EncodeMaterialFlags(uint Flags)
{
    return float(Flags & 0xffu) * (1.0f / 255.0f);
}

uint DecodeMaterialFlags(float Encoded)
{
    return uint(round(saturate(Encoded) * 255.0f));
}

TiramisuGBufferTargets PackTiramisuGBuffer(TiramisuGBufferData Data)
{
    TiramisuGBufferTargets Result;
    Result.BaseColorAO = float4(saturate(Data.BaseColor), saturate(Data.AmbientOcclusion));
    Result.NormalRoughnessMetallic = float4(EncodeOctahedralNormal(normalize(Data.Normal)),
        saturate(Data.Roughness), saturate(Data.Metallic));
    Result.EmissiveMaterialFlags = float4(max(Data.Emissive, 0.0f), EncodeMaterialFlags(Data.MaterialFlags));
    Result.Velocity = Data.Velocity;
    return Result;
}

TiramisuGBufferData UnpackTiramisuGBuffer(float4 BaseColorAO,
    float4 NormalRoughnessMetallic, float4 EmissiveMaterialFlags, float2 Velocity)
{
    TiramisuGBufferData Result;
    Result.BaseColor = BaseColorAO.rgb;
    Result.AmbientOcclusion = BaseColorAO.a;
    Result.Normal = DecodeOctahedralNormal(NormalRoughnessMetallic.xy);
    Result.Roughness = NormalRoughnessMetallic.z;
    Result.Metallic = NormalRoughnessMetallic.w;
    Result.Emissive = EmissiveMaterialFlags.rgb;
    Result.MaterialFlags = DecodeMaterialFlags(EmissiveMaterialFlags.a);
    Result.Velocity = Velocity;
    return Result;
}

#endif

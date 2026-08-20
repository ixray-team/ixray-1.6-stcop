#ifndef TIRAMISU_DEFERRED_LIGHTING_COMMON_HLSL
#define TIRAMISU_DEFERRED_LIGHTING_COMMON_HLSL

#include "common/NRI.hlsl"
#include "common/GBuffer.hlsl"
#include "lighting/PbrLighting.hlsl"

NRI_RESOURCE(cbuffer, DeferredLightingConstants, b, 0, 2)
{
    float4x4 DeferredInverseViewProjection;
    float4 DeferredCameraPosition;
    float4 DeferredLightDirectionAndIntensity;
    float4 DeferredLightColorAndAmbientIntensity;
    float4 DeferredPointLightPositionAndInverseRadiusSquared;
    uint DeferredBaseColorAOIndex;
    uint DeferredNormalRoughnessMetallicIndex;
    uint DeferredEmissiveMaterialFlagsIndex;
    uint DeferredVelocityIndex;
    uint DeferredDepthIndex;
    uint DeferredSamplerIndex;
	uint DeferredGBufferVersion;
	uint DeferredPadding;
};

struct DeferredFullscreenInput
{
    float4 Position : SV_Position;
    float2 TexCoord : TEXCOORD0;
};

float3 ReconstructDeferredWorldPosition(float2 TexCoord, float DeviceDepth)
{
    const float4 ClipPosition = float4(TexCoord * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f),
        DeviceDepth, 1.0f);
    const float4 WorldPosition = mul(DeferredInverseViewProjection, ClipPosition);
    return WorldPosition.xyz / max(abs(WorldPosition.w), 1.0e-6f);
}

TiramisuGBufferData LoadDeferredGBuffer(int2 PixelPosition)
{
    Texture2D<float4> BaseColorAO =
        ResourceDescriptorHeap[NonUniformResourceIndex(DeferredBaseColorAOIndex)];
    Texture2D<float4> NormalRoughnessMetallic =
        ResourceDescriptorHeap[NonUniformResourceIndex(DeferredNormalRoughnessMetallicIndex)];
    Texture2D<float4> EmissiveMaterialFlags =
        ResourceDescriptorHeap[NonUniformResourceIndex(DeferredEmissiveMaterialFlagsIndex)];
    Texture2D<float2> Velocity =
        ResourceDescriptorHeap[NonUniformResourceIndex(DeferredVelocityIndex)];
    return UnpackTiramisuGBuffer(BaseColorAO.Load(int3(PixelPosition, 0)),
        NormalRoughnessMetallic.Load(int3(PixelPosition, 0)),
        EmissiveMaterialFlags.Load(int3(PixelPosition, 0)), Velocity.Load(int3(PixelPosition, 0)));
}

float LoadDeferredDepth(int2 PixelPosition)
{
    Texture2D<float> Depth = ResourceDescriptorHeap[NonUniformResourceIndex(DeferredDepthIndex)];
    return Depth.Load(int3(PixelPosition, 0));
}

TiramisuPbrSurface MakeDeferredPbrSurface(TiramisuGBufferData GBuffer)
{
    TiramisuPbrSurface Result;
    Result.BaseColor = GBuffer.BaseColor;
    Result.Normal = GBuffer.Normal;
    Result.Roughness = GBuffer.Roughness;
    Result.Metallic = GBuffer.Metallic;
    Result.AmbientOcclusion = GBuffer.AmbientOcclusion;
    Result.Emissive = GBuffer.Emissive;
    return Result;
}

#endif

// Common Tiramisu material contract. Pass-specific entry points include this file
// after generating MaterialParameters.generated.hlsl.

#include "common/MaterialGpuAbi.hlsl"

struct MaterialContext
{
    float2 TexCoord0;
    float2 TexCoord1;
    float4 VertexColor;
    float3 WorldNormal;
    float3 WorldPosition;
    float3 CameraPosition;
    float3 CameraVector;
    float Time;
};

struct MaterialInputs
{
    float3 BaseColor;
    float3 Normal;
    float Roughness;
    float Metallic;
    float AmbientOcclusion;
    float3 Emissive;
    float Opacity;
    float OpacityMask;
    float3 WorldPositionOffset;
};

// MaterialParameters is generated from the master material schema. Texture and
// sampler fields are uint indices, not shader resources or backend handles.
#include "MaterialParameters.generated.hlsl"

MaterialParameters LoadMaterialParametersForInstance(uint MaterialInstanceIndex)
{
    const MaterialInstanceGpuData Instance = LoadMaterialInstanceGpuData(MaterialInstanceIndex);
    ByteAddressBuffer MaterialData = ResourceDescriptorHeap[MaterialParameterBufferIndex];
    return LoadMaterialParameters(
        MaterialData, Instance.ParameterDataOffset, DefaultMaterialSamplerIndex);
}

float4 SampleMaterialTexture2D(uint ResourceIndex, uint SamplerIndex, float2 UV)
{
    Texture2D<float4> Texture = ResourceDescriptorHeap[NonUniformResourceIndex(ResourceIndex)];
    SamplerState Sampler = SamplerDescriptorHeap[NonUniformResourceIndex(SamplerIndex)];
#if MATERIAL_VERTEX_SHADER
    return Texture.SampleLevel(Sampler, UV, 0.0f);
#else
    return Texture.Sample(Sampler, UV);
#endif
}

float4 SampleMaterialTextureCube(uint ResourceIndex, uint SamplerIndex, float3 Direction)
{
    TextureCube<float4> Texture = ResourceDescriptorHeap[NonUniformResourceIndex(ResourceIndex)];
    SamplerState Sampler = SamplerDescriptorHeap[NonUniformResourceIndex(SamplerIndex)];
#if MATERIAL_VERTEX_SHADER
    return Texture.SampleLevel(Sampler, Direction, 0.0f);
#else
    return Texture.Sample(Sampler, Direction);
#endif
}

// The compiler substitutes this include with hand-written or graph-generated HLSL.
#include "MaterialImplementation.generated.hlsl"

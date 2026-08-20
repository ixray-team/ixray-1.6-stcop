// © 2021 NVIDIA Corporation

#include "NRI.hlsl"
#include "common.hlsl"
#include "GBuffer.hlsl"
#include "MaterialGpuAbi.hlsl"

struct LegacySceneGBufferOutput
{
    float4 BaseColorAO : SV_Target0;
    float4 NormalRoughnessMetallic : SV_Target1;
    float4 EmissiveMaterialFlags : SV_Target2;
    float2 Velocity : SV_Target3;
};

LegacySceneGBufferOutput Main(in OutputLegacySceneLMap input)
{
    const MaterialInstanceGpuData Instance = LoadMaterialInstanceGpuData(input.InstanceID);
    ByteAddressBuffer MaterialData = ResourceDescriptorHeap[MaterialParameterBufferIndex];
    const uint TextureIndex = MaterialData.Load(Instance.ParameterDataOffset);

    Texture2D<float4> Texture =
        ResourceDescriptorHeap[NonUniformResourceIndex(TextureIndex)];
    SamplerState Sampler = SamplerDescriptorHeap[DefaultMaterialSamplerIndex];
    const float4 BaseColor = Texture.Sample(Sampler, input.UV0);

    TiramisuGBufferData Data;
    Data.BaseColor = BaseColor.rgb;
    Data.AmbientOcclusion = 1.0f;
    Data.Normal = normalize(input.Normal);
    Data.Roughness = 0.7f;
    Data.Metallic = 0.0f;
    Data.Emissive = 0.0f;
    Data.MaterialFlags = 0u;
    Data.Velocity = 0.0f;

    const TiramisuGBufferTargets Packed = PackTiramisuGBuffer(Data);
    LegacySceneGBufferOutput Output;
    Output.BaseColorAO = Packed.BaseColorAO;
    Output.NormalRoughnessMetallic = Packed.NormalRoughnessMetallic;
    Output.EmissiveMaterialFlags = Packed.EmissiveMaterialFlags;
    Output.Velocity = Packed.Velocity;
    return Output;
}

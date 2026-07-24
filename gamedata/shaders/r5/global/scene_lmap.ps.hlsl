// © 2021 NVIDIA Corporation

#include "NRI.hlsl"
#include "common.hlsl"
#include "MaterialGpuAbi.hlsl"

float4 Main( in OutputLegacySceneLMap input  ) : SV_Target
{
    const MaterialInstanceGpuData Instance = LoadMaterialInstanceGpuData(input.InstanceID);
    ByteAddressBuffer MaterialData = ResourceDescriptorHeap[MaterialParameterBufferIndex];
    const uint TextureIndex = MaterialData.Load(Instance.ParameterDataOffset);

    Texture2D<float4> Texture =
        ResourceDescriptorHeap[NonUniformResourceIndex(TextureIndex)];
    SamplerState Sampler = SamplerDescriptorHeap[DefaultMaterialSamplerIndex];
    return Texture.Sample( Sampler, input.UV0 ).rgba;
}

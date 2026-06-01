// © 2021 NVIDIA Corporation

#include "NRI.hlsl"
#include "common.hlsl"

float4 Main( in OutputUI input  ) : SV_Target
{
    Texture2D<float4> Texture = ResourceDescriptorHeap[input.InstanceID];
    SamplerState Sampler = SamplerDescriptorHeap[0];
    return Texture.Sample( Sampler, input.UV ).rgba;
}

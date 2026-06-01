// © 2021 NVIDIA Corporation

#include "NRI.hlsl"
#include "common.hlsl"

float4 Main( in OutputUI input  ) : SV_Target
{
    Texture2D<float4> Texture = ResourceDescriptorHeap[input.InstanceID];
    SamplerState Sampler = SamplerDescriptorHeap[0];
    float4 output = float4(1,1,1,1);
    output.rgb = Texture.Sample( Sampler, input.UV ).rgb;
    return output;
}

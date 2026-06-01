// © 2021 NVIDIA Corporation

#include "NRI.hlsl"

NRI_RESOURCE( cbuffer, CommonConstants, b, 0, 2 )
{
    float3 color;
    float scale;
};

NRI_RESOURCE( Texture2D, g_DiffuseTexture, t, 0, 1 );

struct PushConstants
{
    float transparency;
};

NRI_ROOT_CONSTANTS( PushConstants, g_PushConstants, 1, 2 );
NRI_RESOURCE( SamplerState, g_Sampler, s, 0, 2 );

struct outputVS
{
    float4 position : SV_Position;
    float2 texCoord : TEXCOORD0;
    uint Instance: INSTANCE0;
};

float4 Main( in outputVS input  ) : SV_Target
{
    Texture2D<float4> src0 = ResourceDescriptorHeap[input.Instance];
    SamplerState sampler0 = SamplerDescriptorHeap[0];
    float4 output = float4(1,1,1,1);

    if(input.Instance != 0)
    {
        output.xyz *= src0.Sample( sampler0, input.texCoord ).a;
    }
    else
    {
         output.rgb = src0.Sample( sampler0, input.texCoord ).rgb;
    }
    return output;
}

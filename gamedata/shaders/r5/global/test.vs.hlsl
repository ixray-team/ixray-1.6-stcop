// © 2021 NVIDIA Corporation

#include "NRI.hlsl"
NRI_ENABLE_DRAW_PARAMETERS;

NRI_RESOURCE( cbuffer, CommonConstants, b, 0, 2 )
{
    float3 color;
    float scale;
};

struct outputVS
{
    float4 position : SV_Position;
    float2 texCoord : TEXCOORD0;
    uint Instance : INSTANCE0;
};

outputVS Main
(
    float2 inPos : POSITION0,
    float2 inTexCoord : TEXCOORD0,
    NRI_DECLARE_DRAW_PARAMETERS
)
{
    outputVS output;
    output.Instance = NRI_BASE_INSTANCE;
    output.position.xy = inPos;
    output.position.zw = float2( 0.0, 1.0 );
    output.texCoord = inTexCoord;

    return output;
}

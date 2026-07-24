#include "common/NRI.hlsl"

struct FullscreenTriangleOutput
{
    float4 Position : SV_Position;
    float2 TexCoord : TEXCOORD0;
};

FullscreenTriangleOutput Main(uint VertexId : SV_VertexID)
{
    FullscreenTriangleOutput Result;
    const float2 TexCoord = float2((VertexId << 1u) & 2u, VertexId & 2u);
    Result.TexCoord = TexCoord;
    Result.Position = float4(TexCoord * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f), 0.0f, 1.0f);
    return Result;
}

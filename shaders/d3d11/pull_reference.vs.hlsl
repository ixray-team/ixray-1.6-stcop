#include "shared\vertex.hlsli"

float4 main(PULL_INPUT) : SV_Position
{
    return float4(float(DrawId), float(VertexId), 0.0f, 1.0f);
}

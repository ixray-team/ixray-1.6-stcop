#include "NRI.hlsl"
#include "MaterialPassCommon.hlsl"

NRI_ENABLE_DRAW_PARAMETERS;

static const float3 DecalProjectorCorners[8] =
{
    float3(-0.5f, -0.5f, -0.5f),
    float3( 0.5f, -0.5f, -0.5f),
    float3( 0.5f,  0.5f, -0.5f),
    float3(-0.5f,  0.5f, -0.5f),
    float3(-0.5f, -0.5f,  0.5f),
    float3( 0.5f, -0.5f,  0.5f),
    float3( 0.5f,  0.5f,  0.5f),
    float3(-0.5f,  0.5f,  0.5f)
};

// Шесть наружных граней канонического projector box. Pipeline отбрасывает
// front faces и растеризует дальнюю границу объёма один раз на пиксель.
static const uint DecalProjectorIndices[36] =
{
    0u, 3u, 2u, 0u, 2u, 1u,
    4u, 5u, 6u, 4u, 6u, 7u,
    0u, 4u, 7u, 0u, 7u, 3u,
    1u, 2u, 6u, 1u, 6u, 5u,
    0u, 1u, 5u, 0u, 5u, 4u,
    3u, 7u, 6u, 3u, 6u, 2u
};

// Геометрия box генерируется по SV_VertexID и не требует отдельного GPU buffer.
MaterialPassPixelInput Main(
    NRI_DECLARE_DRAW_PARAMETERS)
{
    const MaterialDrawGpuData DrawData =
        LoadMaterialDrawGpuData(NRI_INSTANCE_ID_OFFSET);
    const float3 LocalPosition =
        DecalProjectorCorners[DecalProjectorIndices[NRI_VERTEX_ID]];
    const float4 WorldPosition = mul(
        DrawData.LocalToWorld,
        float4(LocalPosition, 1.0f));

    MaterialPassPixelInput Output = (MaterialPassPixelInput)0;
    Output.Position = mul(ViewProjectionWorldMatrix, WorldPosition);
    Output.MaterialInstanceIndex = DrawData.MaterialInstanceIndex;
    Output.MaterialDrawFlags = DrawData.Flags;
    Output.MaterialDrawIndex = NRI_INSTANCE_ID_OFFSET;
    return Output;
}

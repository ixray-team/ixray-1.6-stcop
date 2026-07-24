#include "NRI.hlsl"
#include "MaterialPassCommon.hlsl"

NRI_ENABLE_DRAW_PARAMETERS;

struct MaterialLevelStaticVertexInput
{
    float3 Position : POSITION0;
    float3 Normal : NORMAL0;
    float4 Tangent : TANGENT0;
    float2 TexCoord0 : TEXCOORD0;
    float2 TexCoord1 : TEXCOORD1;
    float4 Color : COLOR0;
};

MaterialPassPixelInput Main(
    MaterialLevelStaticVertexInput Input,
    NRI_DECLARE_DRAW_PARAMETERS)
{
    const MaterialDrawGpuData DrawData =
        LoadMaterialDrawGpuData(NRI_INSTANCE_ID_OFFSET);

    float4 WorldPosition = mul(DrawData.LocalToWorld, float4(Input.Position, 1.0f));
    float4 PreviousWorldPosition =
        mul(DrawData.PreviousLocalToWorld, float4(Input.Position, 1.0f));
    const float3 WorldNormal = normalize(
        mul((float3x3)DrawData.LocalToWorld, Input.Normal));

    MaterialContext Context;
    Context.TexCoord0 = Input.TexCoord0;
    Context.TexCoord1 = Input.TexCoord1;
    Context.VertexColor = Input.Color;
    Context.WorldNormal = WorldNormal;
    Context.WorldPosition = WorldPosition.xyz;
    Context.CameraPosition = CameraPositionAndTime.xyz;
    const float3 CameraDelta = Context.CameraPosition - Context.WorldPosition;
    Context.CameraVector = CameraDelta *
        rsqrt(max(dot(CameraDelta, CameraDelta), 1.0e-8f));
    Context.Time = CameraPositionAndTime.w;

    MaterialInputs Material;
    const MaterialParameters Parameters =
        LoadMaterialParametersForInstance(DrawData.MaterialInstanceIndex);
    EvaluateMaterial(Context, Parameters, Material);
    WorldPosition.xyz += Material.WorldPositionOffset;
    PreviousWorldPosition.xyz += Material.WorldPositionOffset;

    MaterialPassPixelInput Output;
    Output.CurrentClipPosition = mul(ViewProjectionWorldMatrix, WorldPosition);
    Output.PreviousClipPosition =
        mul(ViewProjectionWorldMatrix, PreviousWorldPosition);
    Output.Position = Output.CurrentClipPosition;
    Output.TexCoord0 = Input.TexCoord0;
    Output.TexCoord1 = Input.TexCoord1;
    Output.VertexColor = Input.Color;
    Output.WorldNormal = WorldNormal;
    Output.WorldPosition = WorldPosition.xyz;
    Output.MaterialInstanceIndex = DrawData.MaterialInstanceIndex;
    Output.MaterialDrawFlags = DrawData.Flags;
    return Output;
}

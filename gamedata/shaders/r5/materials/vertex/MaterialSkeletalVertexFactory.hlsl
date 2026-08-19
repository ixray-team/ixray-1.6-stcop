#include "NRI.hlsl"
#include "MaterialPassCommon.hlsl"

NRI_ENABLE_DRAW_PARAMETERS;

struct MaterialSkeletalVertexInput
{
    float3 Position : POSITION0;
    float3 Normal : NORMAL0;
    float4 Tangent : TANGENT0;
    float2 TexCoord0 : TEXCOORD0;
    float2 TexCoord1 : TEXCOORD1;
    float4 Color : COLOR0;
    uint4 BoneIndices : BLENDINDICES0;
    float4 BoneWeights : BLENDWEIGHT0;
};

void EvaluateSkinning(
    MaterialSkeletalVertexInput Input,
    MaterialDrawGpuData DrawData,
    out float4 CurrentPosition,
    out float4 PreviousPosition,
    out float3 CurrentNormal)
{
    CurrentPosition = 0.0f;
    PreviousPosition = 0.0f;
    CurrentNormal = 0.0f;
    const float4 LocalPosition = float4(Input.Position, 1.0f);

    [unroll]
    for (uint Influence = 0u; Influence < 4u; ++Influence)
    {
        const float Weight = Input.BoneWeights[Influence];
        if (Weight <= 0.0f)
            continue;

        const uint BoneIndex = min(
            Input.BoneIndices[Influence],
            DrawData.SkinningBoneCount - 1u);
        const float4x4 CurrentBone = LoadMaterialSkinningMatrix(
            DrawData.SkinningPaletteOffset + BoneIndex);
        const float4x4 PreviousBone = LoadMaterialSkinningMatrix(
            DrawData.PreviousSkinningPaletteOffset + BoneIndex);
        CurrentPosition += mul(CurrentBone, LocalPosition) * Weight;
        PreviousPosition += mul(PreviousBone, LocalPosition) * Weight;
        CurrentNormal += mul((float3x3)CurrentBone, Input.Normal) * Weight;
    }
}

MaterialPassPixelInput Main(
    MaterialSkeletalVertexInput Input,
    NRI_DECLARE_DRAW_PARAMETERS)
{
    const MaterialDrawGpuData DrawData =
        LoadMaterialDrawGpuData(NRI_INSTANCE_ID_OFFSET);

    float4 SkinnedPosition;
    float4 PreviousSkinnedPosition;
    float3 SkinnedNormal;
    EvaluateSkinning(
        Input,
        DrawData,
        SkinnedPosition,
        PreviousSkinnedPosition,
        SkinnedNormal);

    float4 WorldPosition = mul(
        DrawData.LocalToWorld,
        SkinnedPosition);
    float4 PreviousWorldPosition = mul(
        DrawData.PreviousLocalToWorld,
        PreviousSkinnedPosition);
    const float3 WorldNormal = normalize(mul(
        (float3x3)DrawData.LocalToWorld,
        normalize(SkinnedNormal)));

    MaterialContext Context;
    Context.TexCoord0 = Input.TexCoord0;
    Context.TexCoord1 = Input.TexCoord1;
    Context.VertexColor = Input.Color;
    Context.WorldNormal = WorldNormal;
    Context.WorldPosition = WorldPosition.xyz;
    Context.CameraPosition = CameraPositionAndTime.xyz;
    const float3 CameraDelta =
        Context.CameraPosition - Context.WorldPosition;
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
    Output.CurrentClipPosition = mul(
        ViewProjectionWorldMatrix,
        WorldPosition);
    Output.PreviousClipPosition = mul(
        ViewProjectionWorldMatrix,
        PreviousWorldPosition);
    Output.Position = Output.CurrentClipPosition;
    Output.TexCoord0 = Input.TexCoord0;
    Output.TexCoord1 = Input.TexCoord1;
    Output.VertexColor = Input.Color;
    Output.WorldNormal = WorldNormal;
    Output.WorldPosition = WorldPosition.xyz;
    Output.MaterialInstanceIndex = DrawData.MaterialInstanceIndex;
    Output.MaterialDrawFlags = DrawData.Flags;
    Output.MaterialDrawIndex = NRI_INSTANCE_ID_OFFSET;
    return Output;
}

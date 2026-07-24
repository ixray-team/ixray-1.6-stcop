#include "NRI.hlsl"
#include "common.hlsl"
#include "MaterialGpuAbi.hlsl"

NRI_ENABLE_DRAW_PARAMETERS;

OutputLegacySceneVertex Main
(
    in InputLegacySceneVertex input,
    NRI_DECLARE_DRAW_PARAMETERS
)
{
    OutputLegacySceneVertex output;
    const MaterialDrawGpuData DrawData = LoadMaterialDrawGpuData(NRI_INSTANCE_ID_OFFSET);
    output.InstanceID = DrawData.MaterialInstanceIndex;
    
    const float4 WorldPosition = mul(DrawData.LocalToWorld, float4(input.Position, 1.0f));
    output.Position = mul(ViewProjectionWorldMatrix, WorldPosition);

    const float3x3 LocalToWorld3x3 = (float3x3)DrawData.LocalToWorld;
    output.Normal = mul(LocalToWorld3x3, input.Normal.xyz);
    output.Tangent = mul(LocalToWorld3x3, input.Tangent.xyz);
    output.Binormal = mul(LocalToWorld3x3, input.Binormal.xyz);

	output.UV = unpack_tc_base(input.UV, (input.Tangent.w + 1) / 2.0, (input.Binormal.w + 1) / 2.0);
    output.Color = output.Color;

    return output;
}


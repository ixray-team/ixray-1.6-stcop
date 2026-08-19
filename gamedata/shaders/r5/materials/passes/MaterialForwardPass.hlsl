#include "MaterialPassCommon.hlsl"
#include "MaterialLightingCommon.hlsl"

float4 Main(MaterialPassPixelInput Input) : SV_Target0
{
    const MaterialInputs Inputs = EvaluateMaterialPass(Input);
    ApplyMaterialOpacityMask(Inputs);
#if MATERIAL_SHADING_UNLIT
    const float3 Color = max(Inputs.BaseColor + Inputs.Emissive, 0.0f);
#else
    const float3 View = CameraPositionAndTime.xyz - Input.WorldPosition;
    const float3 Direct = LightCount == 0u
        ? EvaluateMaterialDirectLighting(Inputs, View,
            float3(-0.45f, 0.75f, -0.5f),
            float3(4.0f, 3.8f, 3.5f))
        : EvaluateMaterialSceneDirectLighting(
            Inputs, View, Input.WorldPosition);
    float3 Ambient = max(Inputs.BaseColor, 0.0f) *
        (0.08f + 0.16f * saturate(Inputs.AmbientOcclusion));
    if (EnvironmentTextureIndex != TIRAMISU_INVALID_DESCRIPTOR_INDEX)
    {
        const float3 Normal = normalize(Inputs.Normal);
        const float3 ViewDirection = normalize(View);
        const float Roughness = saturate(Inputs.Roughness);
        const float Metallic = saturate(Inputs.Metallic);
        const float3 BaseColor = max(Inputs.BaseColor, 0.0f);
        const float3 F0 = lerp(0.04f.xxx, BaseColor, Metallic);
        const float3 DiffuseWeight = (1.0f - F0) * (1.0f - Metallic);
        TextureCube<float4> Environment =
            ResourceDescriptorHeap[EnvironmentTextureIndex];
        SamplerState EnvironmentSampler =
            SamplerDescriptorHeap[DefaultMaterialSamplerIndex];
        const float3 Reflection = reflect(-ViewDirection, Normal);
        const float3 EnvironmentDiffuse = Environment.SampleLevel(
            EnvironmentSampler, Normal, 5.0f).rgb;
        const float3 EnvironmentSpecular = Environment.SampleLevel(
            EnvironmentSampler, Reflection, Roughness * 7.0f).rgb;
        Ambient = (EnvironmentDiffuse * DiffuseWeight * BaseColor +
            EnvironmentSpecular * F0) *
            (0.18f + 0.32f * saturate(Inputs.AmbientOcclusion));
    }
    const float3 Color = Direct + Ambient + max(Inputs.Emissive, 0.0f);
#endif
    float3 FinalColor = Color;
    if ((Input.MaterialDrawFlags & 1u) != 0u)
        FinalColor = lerp(FinalColor, float3(1.0f, 0.55f, 0.08f), 0.24f);
    return float4(FinalColor, saturate(Inputs.Opacity));
}

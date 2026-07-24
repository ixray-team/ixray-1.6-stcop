#include "MaterialPassCommon.hlsl"
#include "MaterialLightingCommon.hlsl"

float4 Main(MaterialPassPixelInput Input) : SV_Target0
{
    const MaterialInputs Material = EvaluateMaterialPass(Input);
    ApplyMaterialOpacityMask(Material);

    const float3 N = normalize(Material.Normal);
    const float3 V = normalize(CameraPositionAndTime.xyz - Input.WorldPosition);
    const float Roughness = saturate(Material.Roughness);
    const float Metallic = saturate(Material.Metallic);
    const float3 BaseColor = max(Material.BaseColor, 0.0f);
    const float3 F0 = lerp(0.04f.xxx, BaseColor, Metallic);
    const float3 DiffuseWeight = (1.0f - F0) * (1.0f - Metallic);
    TextureCube<float4> Environment =
        ResourceDescriptorHeap[Input.MaterialDrawFlags];
    SamplerState EnvironmentSampler =
        SamplerDescriptorHeap[DefaultMaterialSamplerIndex];
    const float3 Reflection = reflect(-V, N);
    const float3 EnvironmentDiffuse = Environment.SampleLevel(
        EnvironmentSampler, N, 5.0f).rgb;
    const float3 EnvironmentSpecular = Environment.SampleLevel(
        EnvironmentSampler, Reflection, Roughness * 7.0f).rgb;
    const float3 Ambient = (EnvironmentDiffuse * DiffuseWeight * BaseColor +
        EnvironmentSpecular * F0) *
        (0.18f + 0.32f * saturate(Material.AmbientOcclusion));
    const float3 Direct = EvaluateMaterialDirectLighting(Material, V,
        float3(-0.45f, 0.75f, -0.5f), float3(5.0f, 4.7f, 4.2f));
#if MATERIAL_SHADING_UNLIT
    const float3 Color = BaseColor + max(Material.Emissive, 0.0f);
#else
    const float3 Color = Direct + Ambient + max(Material.Emissive, 0.0f);
#endif
    return float4(Color, saturate(Material.Opacity));
}

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
    const float3 Ambient = max(Inputs.BaseColor, 0.0f) *
        (0.08f + 0.16f * saturate(Inputs.AmbientOcclusion));
    const float3 Color = Direct + Ambient + max(Inputs.Emissive, 0.0f);
#endif
    float3 FinalColor = Color;
    if ((Input.MaterialDrawFlags & 1u) != 0u)
        FinalColor = lerp(FinalColor, float3(1.0f, 0.55f, 0.08f), 0.24f);
    return float4(FinalColor, saturate(Inputs.Opacity));
}

#include "MaterialPassCommon.hlsl"
#include "common/GBuffer.hlsl"

struct MaterialGBufferOutput
{
    float4 BaseColorAO : SV_Target0;
    float4 NormalRoughnessMetallic : SV_Target1;
    float4 EmissiveMaterialFlags : SV_Target2;
    float2 Velocity : SV_Target3;
};

MaterialGBufferOutput Main(MaterialPassPixelInput Input)
{
    const MaterialInputs Inputs = EvaluateMaterialPass(Input);
    ApplyMaterialOpacityMask(Inputs);

    TiramisuGBufferData Data;
    Data.BaseColor = Inputs.BaseColor;
    Data.AmbientOcclusion = Inputs.AmbientOcclusion;
    Data.Normal = Inputs.Normal;
    Data.Roughness = Inputs.Roughness;
    Data.Metallic = Inputs.Metallic;
    Data.Emissive = Inputs.Emissive;
    Data.MaterialFlags = 0u;
    const float2 CurrentNdc = Input.CurrentClipPosition.xy / max(Input.CurrentClipPosition.w, 1.0e-6f);
    const float2 PreviousNdc = Input.PreviousClipPosition.xy / max(Input.PreviousClipPosition.w, 1.0e-6f);
    Data.Velocity = (CurrentNdc - PreviousNdc) * float2(0.5f, -0.5f);

    const TiramisuGBufferTargets Packed = PackTiramisuGBuffer(Data);
    MaterialGBufferOutput Output;
    Output.BaseColorAO = Packed.BaseColorAO;
    Output.NormalRoughnessMetallic = Packed.NormalRoughnessMetallic;
    Output.EmissiveMaterialFlags = Packed.EmissiveMaterialFlags;
    Output.Velocity = Packed.Velocity;
    return Output;
}

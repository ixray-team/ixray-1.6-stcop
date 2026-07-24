#include "MaterialPassCommon.hlsl"

float4 Main(MaterialPassPixelInput Input) : SV_Target0
{
    const MaterialInputs Inputs = EvaluateMaterialPass(Input);
    return float4(Inputs.BaseColor + Inputs.Emissive, Inputs.Opacity);
}

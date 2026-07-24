#include "MaterialPassCommon.hlsl"

float4 Main(MaterialPassPixelInput Input) : SV_Target0
{
    const MaterialInputs Inputs = EvaluateMaterialPass(Input);
    return float4(max(Inputs.BaseColor * Input.VertexColor.rgb + Inputs.Emissive, 0.0f),
        saturate(Inputs.Opacity * Input.VertexColor.a));
}

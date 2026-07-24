#include "MaterialPassCommon.hlsl"

void Main(MaterialPassPixelInput Input)
{
    const MaterialInputs Inputs = EvaluateMaterialPass(Input);
    ApplyMaterialOpacityMask(Inputs);
}

#include "NRI.hlsl"
#include "common.hlsl"
#include "MaterialGpuAbi.hlsl"

NRI_ENABLE_DRAW_PARAMETERS;

OutputUI Main
(
    in InputUI input,
    NRI_DECLARE_DRAW_PARAMETERS
)
{
    OutputUI output;

    output.InstanceID = NRI_BASE_INSTANCE;
    output.Position.xy = input.Position.xy * SceneView.zw * 2.f - 1.f;
    output.Position.y = -output.Position.y;
    output.Position.zw = float2( 0.0, 1.0 );
    output.UV = input.UV;
    output.Color = output.Color;

    return output;
}

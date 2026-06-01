#include "NRI.hlsl"
#include "common.hlsl"

NRI_ENABLE_DRAW_PARAMETERS;

NRI_RESOURCE( cbuffer, GlobalConstants, b, 0, 2 )
{
    float4 ScreenSize;
};

OutputUI Main
(
    in InputUI input,
    NRI_DECLARE_DRAW_PARAMETERS
)
{
    OutputUI output;

    output.InstanceID = NRI_BASE_INSTANCE;
    output.Position.xy = input.Position.xy* ScreenSize.zw*2.f - 1.f;
    output.Position.zw = float2( 0.0, 1.0 );
    output.UV = input.UV;
    output.Color = output.Color;

    return output;
}

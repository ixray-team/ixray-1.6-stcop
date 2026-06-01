#include "NRI.hlsl"
#include "common.hlsl"

NRI_ENABLE_DRAW_PARAMETERS;

NRI_RESOURCE( cbuffer, GlobalConstants, b, 0, 2 )
{
    float4 ScreenSize;
	float4x4 ViewProjectionMatrix;
};

OutputLegacySceneLMap Main
(
    in InputLegacySceneLMap input,
    NRI_DECLARE_DRAW_PARAMETERS
)
{
    OutputLegacySceneLMap output;
    output.InstanceID = NRI_BASE_INSTANCE;
    
    output.Position = float4(input.Position,1);
	output.Position = mul(ViewProjectionMatrix, output.Position);
    output.Normal = input.Normal.xyz;
    output.Tangent = input.Tangent.xyz;
    output.Binormal = input.Binormal.xyz;

	output.UV0 = unpack_tc_base(input.UV0, (input.Tangent.w + 1) / 2.0, (input.Binormal.w + 1) / 2.0);
    output.UV1 = unpack_tc_lmap(input.UV1);


    return output;
}


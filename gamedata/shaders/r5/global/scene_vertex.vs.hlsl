#include "NRI.hlsl"
#include "common.hlsl"

NRI_ENABLE_DRAW_PARAMETERS;

NRI_RESOURCE( cbuffer, GlobalConstants, b, 0, 2 )
{
    float4 ScreenSize;
	matrix ViewProjectionWorldMatrix;
};

OutputLegacySceneVertex Main
(
    in InputLegacySceneVertex input,
    NRI_DECLARE_DRAW_PARAMETERS
)
{
    OutputLegacySceneVertex output;
    output.InstanceID = NRI_BASE_INSTANCE;
    
    output.Position = float4(input.Position,1);
	output.Position = mul(ViewProjectionWorldMatrix, output.Position);

    output.Normal = input.Normal.xyz;
    output.Tangent = input.Tangent.xyz;
    output.Binormal = input.Binormal.xyz;

	output.UV = unpack_tc_base(input.UV, (input.Tangent.w + 1) / 2.0, (input.Binormal.w + 1) / 2.0);
    output.Color = output.Color;

    return output;
}


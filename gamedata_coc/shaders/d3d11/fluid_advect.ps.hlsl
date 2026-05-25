#include "fluid_common.hlsli"

//	Pixel
float4 main(p_fluidsim input) : SV_Target
{
    if (IsNonEmptyCell(input.texcoords.xyz))
    {
        return 0;
    }

    float3 npos = GetAdvectedPosTexCoords(input);

    return Texture_color.SampleLevel(samLinear, npos, 0) * modulate;
}

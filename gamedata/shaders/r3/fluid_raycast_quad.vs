#include "fluid_common_render.h"

//////////////////////////////////////////////////////////////////////////////////////////
//	Vertex
//	TODO: DX10: replace WorldViewProjection with m_WVP
PS_INPUT_RAYCAST main_vs_4_0 (VS_INPUT input)
{
    PS_INPUT_RAYCAST output = (PS_INPUT_RAYCAST)0;
    output.pos = float4(input.pos,1);
//    output.posInGrid = mul( float4( input.pos.xy*ZNear, 0, ZNear ), InvWorldViewProjection );
	output.posInGrid = mul( InvWorldViewProjection, float4( input.pos.xy*ZNear, 0, ZNear ));
    return output;
}
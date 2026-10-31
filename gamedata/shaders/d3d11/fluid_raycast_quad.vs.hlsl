#include "fluid_common_render.hlsli"

//	Vertex
//	TODO: DX10: replace WorldViewProjection with m_WVP
PS_INPUT_RAYCAST main(uint VertexId : SV_VertexID)
{
    float3 Pos = FluidQuadVertex(VertexId);
    PS_INPUT_RAYCAST output = (PS_INPUT_RAYCAST)0;
    output.pos = float4(Pos, 1);
    //    output.posInGrid = mul( float4( Pos.xy*ZNear, 0, ZNear ), InvWorldViewProjection );
    output.posInGrid = mul(InvWorldViewProjection, float4(Pos.xy * ZNear, 0, ZNear)).xyz;
    return output;
}


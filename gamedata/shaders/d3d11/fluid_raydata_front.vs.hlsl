#include "fluid_common_render.hlsli"

//	Vertex
//	TODO: DX10: replace WorldViewProjection with m_WVP
PS_INPUT_RAYDATA_FRONT main(uint VertexId : SV_VertexID)
{
    float3 Pos = FluidBoxVertex(VertexId);
    PS_INPUT_RAYDATA_FRONT output = (PS_INPUT_RAYDATA_FRONT)0;
    //    output.pos = mul(float4(Pos,1), WorldViewProjection);
    output.pos = mul(WorldViewProjection, float4(Pos, 1));
    output.posInGrid = Pos;
    output.depth = output.pos.w;
    return output;
}

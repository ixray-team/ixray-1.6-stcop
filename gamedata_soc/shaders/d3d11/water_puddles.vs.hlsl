#include "common.hlsli"

static const float2 quad_geometry[6] =
{
    float2(-1.0, -1.0), 
	float2(-1.0, 1.0), 
	float2(1.0, -1.0), 
	float2(-1.0, 1.0), 
	float2(1.0, 1.0), 
	float2(1.0, -1.0)
};

struct VSOutput
{
    float4 hpos	: SV_POSITION;
	float3 world_position : TEXCOORD0;	
};

float puddle_constants;

VSOutput main(uint vertex_id : SV_VertexID)
{
	VSOutput O;

	float3 vertex_position;
	vertex_position.xz = quad_geometry[vertex_id];
	vertex_position.y = puddle_constants;
	O.world_position = mul(m_W, float4(vertex_position, 1.0));

	O.hpos = mul(m_VP, float4(O.world_position, 1.0));
    O.hpos.xy += m_taa_jitter.xy * O.hpos.w;

    return O; 
}
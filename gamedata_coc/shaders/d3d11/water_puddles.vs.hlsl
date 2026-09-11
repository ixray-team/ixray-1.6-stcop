#include "common.hlsli"
#include "shared\waterconfig.hlsli"
#include "shared\watermove.hlsli"

static const float2 quad_geometry[6] =
{
    float2(-1.0, -1.0), 
	float2(-1.0, 1.0), 
	float2(1.0, -1.0), 
	float2(-1.0, 1.0), 
	float2(1.0, 1.0), 
	float2(1.0, -1.0)
};

struct vf
{
    float2 tbase : TEXCOORD0;
    float2 tnorm0 : TEXCOORD1;
    float2 tnorm1 : TEXCOORD2;
    float3 M1 : TEXCOORD3;
    float3 M2 : TEXCOORD4;
    float3 M3 : TEXCOORD5;
    float3 v2point : TEXCOORD6;
    float3 tctexgen : TEXCOORD7;
    float3 pos : TEXCOORD8;
    float4 c0 : COLOR0;
    float4 hpos : SV_POSITION;
};

float puddle_constants;

void main(in uint vertex_id : SV_VertexID, out vf O)
{
	float3 vertex_position;
	
	vertex_position.xz = quad_geometry[vertex_id];
	vertex_position.y = puddle_constants;
	
	O.pos = mul(m_W, float4(vertex_position, 1.0));
    O.v2point = O.pos.xyz - eye_position;
	
    O.tbase = O.pos.xz * 0.3;
    O.tnorm0 = watermove_tc(O.tbase * W_DISTORT_BASE_TILE_0, O.pos.xz, W_DISTORT_AMP_0);
    O.tnorm1 = watermove_tc(O.tbase * W_DISTORT_BASE_TILE_1, O.pos.xz, W_DISTORT_AMP_1);

	O.hpos = mul(m_VP, float4(O.pos, 1.0));
    O.hpos.xy += m_taa_jitter.xy * O.hpos.w;
	
    O.M1 = float3(-1, 0, 0);
    O.M2 = float3(0, 0, +1);
    O.M3 = float3(0, -1, 0);
	
	O.c0 = float2(0.0f, 0.7f).xxxy;
    O.tctexgen = mul(m_V, float4(O.pos, 1.0f)).xyz;
}
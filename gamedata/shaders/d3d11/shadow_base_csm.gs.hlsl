#include "common.hlsli"
float4x4 m_shadow_vp[3]; //view-projection (without texcoord bias and scale)

struct GS_out
{
#ifdef USE_AREF
	float2 tc0		: TEXCOORD0;
#endif
	float4 hpos 	: SV_Position;
	uint rt_idx 	: SV_RenderTargetArrayIndex;
};

[maxvertexcount(9)] //3 * cascade
void main(triangle p_shadow_csm VS_in[3], inout TriangleStream<GS_out> GeometryStream)
{
	[unroll]
    for(int cascade = 0; cascade < 3; cascade++)
    {
		GS_out output;

		output.rt_idx = cascade; //output to texarray

		[unroll]
		for(uint verts = 0; verts < 3; verts++)
		{
			#ifdef USE_AREF
				output.tc0 = VS_in[verts].tc0;
			#endif
			
			output.hpos = mul(m_shadow_vp[cascade], VS_in[verts].w_pos); //world(set m_W in shader)-vp(per cascade)

			GeometryStream.Append(output);
		}
		GeometryStream.RestartStrip();
	}
}

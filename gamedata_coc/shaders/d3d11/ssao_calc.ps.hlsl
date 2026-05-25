#include "common.hlsli"

uniform Texture2D s_half_depth;
#include "ssao.ps.hlsl"

struct _input
{
    float4 tc0 : TEXCOORD0; // tc.xy, tc.w = tonemap scale
    float2 tcJ : TEXCOORD1; // jitter coords
    float4 pos2d : SV_POSITION;
};

float4 main(_input I) : SV_Target0
{
    IXrayGbuffer O;
    GbufferUnpack(I.tc0.xy, I.pos2d.xy, O);
	
	if(O.Depth > 0.9999f) {
		return 1.0f;
	}

	float occ = calc_ssao(O.Depth, O.Normal, I.tc0.xy);
    return float4(occ, occ, occ, occ);
}


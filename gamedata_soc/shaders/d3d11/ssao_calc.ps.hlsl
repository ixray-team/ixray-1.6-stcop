#include "common.hlsli"

uniform Texture2D s_half_depth;
#include "ssao.ps.hlsl"

float main(PSInputFullscreen I) : SV_Target0
{
    IXrayGbuffer O;
    GbufferUnpack(I.texcoord.xy, I.hpos.xy, O);
	
	if(O.Depth > 0.9999f) {
		return 1.0f;
	}

	float occ = calc_ssao(O.Depth, O.Normal, I.texcoord.xy);
    return occ;
}


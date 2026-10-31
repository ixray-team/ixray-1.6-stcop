#include "common.hlsli"
#include "skin.hlsli"

void skinned_main(in v_model I, out VSOutputFullscreen O)
{
    O.hpos = mul(m_WVP, I.P);
    O.hpos.xy += m_taa_jitter.xy * O.hpos.w;
	
    O.texcoord = I.tc.xy;
}

void main(PULL_INPUT, out VSOutputFullscreen O)
{
    skinned_main(PullModel(VertexId), O);
}

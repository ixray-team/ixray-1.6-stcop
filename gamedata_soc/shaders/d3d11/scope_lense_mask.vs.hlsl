#include "common.hlsli"
#include "skin.hlsli"

void skinned_main(in v_model I, out VSOutputFullscreen O)
{
    O.hpos = mul(m_WVP, I.P);
    O.hpos.xy += m_taa_jitter.xy * O.hpos.w;
	
    O.texcoord = I.tc.xy;
}

#if defined(SKIN_0)
void main(in v_model_skinned_0 I, out VSOutputFullscreen O)
{
    skinned_main(skinning_0(I), O);
}
#elif defined(SKIN_1)
void main(in v_model_skinned_1 I, out VSOutputFullscreen O)
{
    skinned_main(skinning_1(I), O);
}
#elif defined(SKIN_2)
void main(in v_model_skinned_2 I, out VSOutputFullscreen O)
{
    skinned_main(skinning_2(I), O);
}
#elif defined(SKIN_3)
void main(in v_model_skinned_3 I, out VSOutputFullscreen O)
{
    skinned_main(skinning_3(I), O);
}
#elif defined(SKIN_4)
void main(in v_model_skinned_4 I, out VSOutputFullscreen O)
{
    skinned_main(skinning_4(I), O);
}
#else
void main(in v_model I, out VSOutputFullscreen O)
{
    skinned_main(I, O);
}
#endif


#include "common.hlsli"
#include "skin.hlsli"

void skinned_main(in v_model I, out float4 HPos)
{
    HPos = mul(m_WVP, I.P);
    HPos.xy += m_taa_jitter.xy * HPos.w;
}

#if defined(SKIN_0)
void main(in v_model_skinned_0 I, out float4 HPos : SV_POSITION)
{
    skinned_main(skinning_0(I), HPos);
}
#elif defined(SKIN_1)
void main(in v_model_skinned_1 I, out float4 HPos : SV_POSITION)
{
    skinned_main(skinning_1(I), HPos);
}
#elif defined(SKIN_2)
void main(in v_model_skinned_2 I, out float4 HPos : SV_POSITION)
{
    skinned_main(skinning_2(I), HPos);
}
#elif defined(SKIN_3)
void main(in v_model_skinned_3 I, out float4 HPos : SV_POSITION)
{
    skinned_main(skinning_3(I), HPos);
}
#elif defined(SKIN_4)
void main(in v_model_skinned_4 I, out float4 HPos : SV_POSITION)
{
    skinned_main(skinning_4(I), HPos);
}
#else
void main(in v_model I, out float4 HPos : SV_POSITION)
{
    skinned_main(I, HPos);
}
#endif


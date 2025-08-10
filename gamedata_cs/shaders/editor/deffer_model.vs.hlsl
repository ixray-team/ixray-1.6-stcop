#include "common.hlsli"
#include "skin.hlsli"

void skinned_main(in v_model I, out p_bumped_new O)
{
    float3 Nw = normalize(mul((float3x3)m_W, (float3)I.N));
    float hemi_val = Nw.y * 0.3f + 0.5f;
    float3 Pe = mul(m_WV, I.P);

    O.tcdh = float4(I.tc.xy, hemi_val, 0.5f);
    O.position = float4(Pe, 1.0f);

    O.M1 = mul(m_WV, I.N.xyz * 2.0f);
    O.M2 = mul(m_WV, I.T.xyz * 2.0f);
    O.M3 = mul(m_WV, I.B.xyz * 2.0f);

    O.hpos = mul(m_WVP, I.P);
}

#if defined(SKIN_0)
void main(in v_model_skinned_0 I, out p_bumped_new O)
{
    skinned_main(skinning_0(I), O);
}
#elif defined(SKIN_1)
void main(in v_model_skinned_1 I, out p_bumped_new O)
{
    skinned_main(skinning_1(I), O);
}
#elif defined(SKIN_2)
void main(in v_model_skinned_2 I, out p_bumped_new O)
{
    skinned_main(skinning_2(I), O);
}
#elif defined(SKIN_3)
void main(in v_model_skinned_3 I, out p_bumped_new O)
{
    skinned_main(skinning_3(I), O);
}
#elif defined(SKIN_4)
void main(in v_model_skinned_4 I, out p_bumped_new O)
{
    skinned_main(skinning_4(I), O);
}
#else
void main(in v_model I, out p_bumped_new O)
{
    skinned_main(I, O);
}
#endif

#include "common.hlsli"
#include "skin.hlsli"

p_shadow_csm _main(v_model I)
{
    p_shadow_csm O;

    O.hpos = mul(m_WVP, I.P);
	O.w_pos = float4(mul(m_W, I.P),1.);
    O.tc0 = I.tc;

    return O;
}

#ifdef SKIN_NONE
p_shadow_csm main(v_model v)
{
    return _main(v);
}
#endif

#ifdef SKIN_0
p_shadow_csm main(v_model_skinned_0 v)
{
    return _main(skinning_0(v));
}
#endif

#ifdef SKIN_1
p_shadow_csm main(v_model_skinned_1 v)
{
    return _main(skinning_1(v));
}
#endif

#ifdef SKIN_2
p_shadow_csm main(v_model_skinned_2 v)
{
    return _main(skinning_2(v));
}
#endif

#ifdef SKIN_3
p_shadow_csm main(v_model_skinned_3 v)
{
    return _main(skinning_3(v));
}
#endif

#ifdef SKIN_4
p_shadow_csm main(v_model_skinned_4 v)
{
    return _main(skinning_4(v));
}
#endif

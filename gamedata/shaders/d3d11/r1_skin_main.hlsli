#include "skin.hlsli"

#if defined(SKIN_0)
R1_SKIN_OUTPUT main(v_model_skinned_0 v)
{
    return _main(skinning_0(v));
}
#elif defined(SKIN_1)
R1_SKIN_OUTPUT main(v_model_skinned_1 v)
{
    return _main(skinning_1(v));
}
#elif defined(SKIN_2)
R1_SKIN_OUTPUT main(v_model_skinned_2 v)
{
    return _main(skinning_2(v));
}
#elif defined(SKIN_3)
R1_SKIN_OUTPUT main(v_model_skinned_3 v)
{
    return _main(skinning_3(v));
}
#elif defined(SKIN_4)
R1_SKIN_OUTPUT main(v_model_skinned_4 v)
{
    return _main(skinning_4(v));
}
#else
R1_SKIN_OUTPUT main(v_model v)
{
    return _main(v);
}
#endif

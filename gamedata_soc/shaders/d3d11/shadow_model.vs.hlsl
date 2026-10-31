#include "common.hlsli"
#include "skin.hlsli"
#include "editor_instancing.hlsli"

p_shadow _main(v_model I, object_xforms X)
{
    p_shadow O;

    O.hpos = mul(X.WVP, I.P);
    O.tc0 = I.tc;

    return O;
}

#ifdef SKIN_NONE
p_shadow main(v_model v, uint InstanceID : SV_InstanceID)
{
    return _main(v, object_get_xforms(InstanceID));
}
#endif

#ifdef SKIN_0
p_shadow main(v_model_skinned_0 v)
{
    return _main(skinning_0(v), object_default_xforms());
}
#endif

#ifdef SKIN_1
p_shadow main(v_model_skinned_1 v)
{
    return _main(skinning_1(v), object_default_xforms());
}
#endif

#ifdef SKIN_2
p_shadow main(v_model_skinned_2 v)
{
    return _main(skinning_2(v), object_default_xforms());
}
#endif

#ifdef SKIN_3
p_shadow main(v_model_skinned_3 v)
{
    return _main(skinning_3(v), object_default_xforms());
}
#endif

#ifdef SKIN_4
p_shadow main(v_model_skinned_4 v)
{
    return _main(skinning_4(v), object_default_xforms());
}
#endif

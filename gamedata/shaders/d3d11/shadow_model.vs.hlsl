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

p_shadow main(PULL_INPUT)
{
    return _main(PullModel(VertexId), object_get_xforms(DrawId));
}

#include "common\common.hlsli"
#include "common\common_iostructs.hlsli"

// Vertex
v2p_TL0uv vs_main(PULL_INPUT)
{
    v2p_TL0uv O;
    RingVertex V = PullRing(VertexId);

    O.HPos.x = (V.P.x + 0.5f) / screen_res.x * 2 - 1;
    O.HPos.y = ((V.P.y + 0.5f) / screen_res.y * 2 - 1) * -1;
    O.HPos.zw = float2(V.P.z, asfloat(V.X));
    O.Color = V.C;

    return O;
}

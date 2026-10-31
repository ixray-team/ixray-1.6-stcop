#include "common.hlsli"
#include "common_iostructs.hlsli"

// Vertex
v2p_TL0uv main(PULL_INPUT)
{
    v2p_TL0uv O;
    RingVertex V = PullRing(VertexId);

    O.HPos.x = (V.P.x + 0.5f) * screen_res.z * 2 - 1;
    O.HPos.y = ((V.P.y + 0.5f) * screen_res.w * 2 - 1) * -1;
    O.HPos.zw = float2(V.P.z, asfloat(V.X));
    O.Color = V.C;

    return O;
}

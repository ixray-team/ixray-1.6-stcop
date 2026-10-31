#include "common.hlsli"
#include "common_iostructs.hlsli"

#ifdef EDITOR_MODE
// Texture transform of the editor helper draws (fixed-function D3DTS_TEXTURE0 emulation)
#include "shared\extra_cb.hlsli"
#endif

// Vertex
v2p_TL main(PULL_INPUT)
{
    v2p_TL O;
    RingVertex V = PullRing(VertexId);

    O.HPos.x = (V.P.x + 0.5f) * screen_res.z * 2 - 1;
    O.HPos.y = ((V.P.y + 0.5f) * screen_res.w * 2 - 1) * -1;
    O.HPos.zw = float2(V.P.z, asfloat(V.X));

#ifdef EDITOR_MODE
    O.Tex0 = mul(m_tc_xform, float4(V.UV, 1.0f, 0.0f)).xy;
#else
    O.Tex0 = V.UV;
#endif
    O.Color = V.C;

    return O;
}

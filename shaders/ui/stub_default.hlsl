#include "common\common.hlsli"

// Vertex
v2p_TL vs_main(PULL_INPUT)
{
    v2p_TL O;
    RingVertex V = PullRing(VertexId);

    O.HPos = mul(m_WVP, float4(V.P, 1.0f));
    O.Tex0 = V.UV;
    O.Color = V.C;

    return O;
}

// Pixel
float4 ps_main(p_TL I) : SV_Target
{
    return s_base.Sample(smp_base, I.Tex0) * I.Color;

    //	float4 res = s_base.Sample( smp_base, I.Tex0 )*I.Color;
    //	clip(res-m_AlphaRef);
    //	clip(res-0.5);
    //	return res;
}

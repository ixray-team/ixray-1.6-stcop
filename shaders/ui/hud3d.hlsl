#include "common\common.hlsli"

struct ui_vert_out
{
    float2 tc0 : TEXCOORD0;
    float4 P : SV_POSITION;
};

ui_vert_out vs_main(PULL_INPUT)
{
    ui_vert_out O;
    RingVertex V = PullRing(VertexId);

    O.tc0 = V.UV;
    O.P = mul(m_WVP, float4(V.P, 1.0f));

    O.P.xy += m_taa_jitter.xy * O.P.w;
    return O;
}

float4 ps_main(ui_vert_out I) : SV_Target
{
    float4 r = s_base.Sample(smp_base, I.tc0);
    r.xyz = GammaToLinear(r.xyz);

    return r;
}

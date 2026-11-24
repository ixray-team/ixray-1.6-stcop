#include "common\common.hlsli"

struct v2p
{
    float4 c : COLOR0;
    float fog : FOG;
    float4 hpos : SV_POSITION;
};

v2p vs_main(PULL_INPUT)
{
    v2p o;
    RingVertex V = PullRing(VertexId);

    o.hpos = mul(m_VP, float4(V.P, 1.0f)); // xform, input in world coords
    o.c = V.C.zyxw;
    o.fog = calc_fogging(V.P); // fog, input in world coords
    o.c = lerp(o.c, fog_color, o.fog);
    o.fog = 1.0f - o.fog;

    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;
    return o;
}

float4 ps_main(float4 C : COLOR0) : SV_Target
{
    return SceneFromGamma(C);
}

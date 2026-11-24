// What is left of the projected shadows of the old static renderer, for materials that still name their
// blenders. BLOB_BLUR is the blur of the shadow's map, a fullscreen pass; without it, the shadow laid on the world
#include "common\r1_common.hlsli"

struct blob_vertex {
    float4 uv : TEXCOORD0;                  // the blur's two taps in .xy and .zw
    float4 color : COLOR0;
    float4 position_proj : SV_POSITION;
};

#ifdef BLOB_BLUR
blob_vertex vs_main(uint VertexId : SV_VertexID)
{
    blob_vertex output;
    float2 uv = float2((VertexId << 1u) & 2u, VertexId & 2u);
    output.position_proj = float4(uv * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f), 0.0f, 1.0f);
    output.uv = float4(uv - 0.75f / 512.0f, uv + 0.75f / 512.0f);
    output.color = 0.0f;
    return output;
}
#else
blob_vertex vs_main(PULL_INPUT)
{
    blob_vertex output;
    RingVertex vertex = PullRing(VertexId);
    output.position_proj = mul(m_WVP, float4(vertex.P, 1.0f));
    output.uv = float4(vertex.UV, 0.0f, 0.0f);
    output.color = vertex.C;
    return output;
}
#endif

float4 ps_main(blob_vertex input) : SV_Target
{
#ifdef BLOB_BLUR
    return (s_base.Sample(smp_rtlinear, input.uv.xy) + s_base.Sample(smp_rtlinear, input.uv.zw)) * (127.0f / 255.0f);
#else
    return saturate(s_base.Sample(smp_base, input.uv.xy) + input.color);
#endif
}

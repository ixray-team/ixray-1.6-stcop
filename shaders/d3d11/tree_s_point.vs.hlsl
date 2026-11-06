#include "r1_common.hlsli"
#include "r1_static.hlsli"



vf_point main(PULL_INPUT)
{
    PulledVertex V = PullVertex(VertexId);
    vf_point o;

    float3 pos = mul(m_xform, float4(V.P, 1.0f));

    float base = m_xform._24; 
    float dp = calc_cyclic(wave.w + dot(pos, (float3)wave));
    float H = pos.y - base; 
    float frac = DecodeS16x2(V.X).x * consts.x; 
    float inten = H * dp; 
    float2 result = calc_xz_wave(wind.xz * inten, frac);
    float4 f_pos = float4(pos, 1); 
    float3 f_N = normalize(mul((float3x3)m_xform, V.N));

    o.hpos = mul(m_VP, f_pos);
    o.tc0 = V.UV;
    o.color = calc_point(o.tc1, o.tc2, f_pos, f_N);

    return o;
}

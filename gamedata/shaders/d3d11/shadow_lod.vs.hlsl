#include "common.hlsli"


// Vertex
void main(PULL_INPUT, out p_shadow O)
{
    PulledVertex V = PullVertex(VertexId);
    float4 pos = float4(mul(m_xform, float4(V.P, 1.0f)).xyz, 1.0f);

#ifdef USE_TREEWAVE
    float base = m_xform._24;
    float dp = calc_cyclic(wave.w + dot(pos.xyz, wave.xyz));
    float H = pos.y - base;
    float inten = H * dp;
    float frac = DecodeS16x2(V.X).x * consts.x;
    pos.xz += calc_xz_wave(wind.xz * inten, frac);
#endif

    O.hpos = mul(m_VP, pos);
    O.tc0 = V.UV;
}


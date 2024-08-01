#include "common.hlsli"

uniform float4 consts;

uniform float4 wave;
uniform float4 dir2D;

uniform float4 array[200];

void main(in v_detail I, out p_bumped_new O)
{
    int i = I.misc.w;
    float4 m0 = array[i + 0];
    float4 m1 = array[i + 1];
    float4 m2 = array[i + 2];
    float4 c0 = array[i + 3];

    float4 pos;
    pos.x = dot(m0, I.pos);
    pos.y = dot(m1, I.pos);
    pos.z = dot(m2, I.pos);
    pos.w = 1.0f;

#ifdef USE_TREEWAVE
    float base = m1.w;
    float H = I.pos.y * length(m1.xyz);
    float frac = I.misc.z * consts.x;

    float dp = calc_cyclic(dot(pos, wave));
    float inten = H * dp;

    pos.xz += calc_xz_wave(dir2D.xz * inten, frac);
#endif

    float3 Pe = mul(m_WV, pos);
    float2 tc = I.misc.xy * consts.xy;

    float3 N;
    N.x = pos.x - m0.w;
    N.y = pos.y - m1.w + 0.75f;
    N.z = pos.z - m2.w;

    O.tcdh = float4(tc.xy, c0.w, c0.x);
    O.position = float4(Pe, 1.0f);

    float3x3 xform = mul((float3x3)m_WV, float3x3(
        0.0f, 0.0f, N.x,
        0.0f, 0.0f, N.y,
        0.0f, 0.0f, N.z));

    O.M1 = xform[0];
    O.M2 = xform[1];
    O.M3 = xform[2];

    O.hpos = mul(m_WVP, pos);
}

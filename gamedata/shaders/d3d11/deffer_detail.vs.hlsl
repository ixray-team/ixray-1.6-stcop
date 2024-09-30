#include "common.hlsli"

cbuffer DetailConstants
{
    float4 consts;

    float4 wave;
    float4 wave_old;

    float4 dir2D;
    float4 dir2D_old;

    float4 array[61 * 4];
}

void main(in v_detail I, out p_bumped_new O)
{
    int i = I.misc.w;
    float4 m0 = array[i + 0];
    float4 m1 = array[i + 1];
    float4 m2 = array[i + 2];
    float4 c0 = array[i + 3];

    float4 pos, pos_old;
    pos.x = dot(m0, I.pos);
    pos.y = dot(m1, I.pos);
    pos.z = dot(m2, I.pos);
    pos.w = 1.0f;
    pos_old = pos;

#ifdef USE_TREEWAVE
    float base = m1.w;
    float H = I.pos.y * length(m1.xyz);
    float frac = I.misc.z * consts.x;

    float dp = calc_cyclic(dot(pos, wave));
    float inten = H * dp;

    pos.xz += calc_xz_wave(dir2D.xz * inten, frac);

    #ifndef DETAIL_SHADOW_PASS
		float dp_old = calc_cyclic(dot(pos_old, wave_old));
		float inten_old = H * dp_old;

		pos_old.xz += calc_xz_wave(dir2D_old.xz * inten_old, frac);
    #endif
#endif

    float3 Pe = mul(m_WV, pos);
    float2 tc = I.misc.xy * consts.xy;

    float3 N;
    N.x = pos.x - m0.w;
    N.y = pos.y - m1.w + 0.75f;
    N.z = pos.z - m2.w;

    O.tcdh = float4(tc.xy, c0.w, c0.x);
    O.position = float4(Pe, 1.0f);

    N.xyz = mul((float3x3)m_WV, N.xyz);

    O.M1 = N.xxx;
    O.M2 = N.yyy;
    O.M3 = N.zzz;

    O.hpos = mul(m_WVP, pos);

#ifndef DETAIL_SHADOW_PASS
    O.hpos_curr = O.hpos;
    O.hpos_old = mul(m_VP_old, pos_old);

    O.hpos.xy += m_taa_jitter.xy * O.hpos.w;
#else
    O.hpos_curr = O.hpos_old = O.hpos;
#endif
}


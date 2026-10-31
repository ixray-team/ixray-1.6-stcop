#include "common.hlsli"

void main(PULL_INPUT, out p_bumped_new O)
{
    PulledVertex V = PullVertex(VertexId);
    float4 pos = float4(mul(m_xform, float4(V.P, 1.0f)).xyz, 1.0);
    float4 pos_old = pos;

    float2 tc = V.UV;
    float L_base = DecodeU8x4(V.C).x;
    float sun = L_base * c_sun.x + c_sun.y;
    float hemi = L_base * c_scale.w + c_bias.w;

#ifdef USE_LENGTH_BUFFER
	O.lmap = L_base * c_scale.xyz + c_bias.xyz;
#endif

#ifdef USE_TREEWAVE
    float base = m_xform._24;
    float H = pos.y - base;

    float dp = calc_cyclic(wave.w + dot(pos.xyz, wave.xyz));
    float frac = DecodeS16x2(V.X).x * consts.x;
    float inten = H * dp;

    pos.xz += calc_xz_wave(wind.xz * inten, frac);

    float dp_old = calc_cyclic(wave_old.w + dot(pos_old.xyz, wave_old.xyz));
    float frac_old = DecodeS16x2(V.X).x * consts_old.x;
    float inten_old = H * dp_old;

    pos_old.xz += calc_xz_wave(wind_old.xz * inten_old, frac_old);
#endif

    float3 Pe = mul(m_V, pos);

    O.tcdh = float4(tc.xy, hemi, sun);
    O.position = float4(Pe, 1.0f);

    float3 N = 2.0f * V.N;
	
#if defined(USE_BUMP) || defined(USE_TDETAIL_BUMP)
    float3 T = 2.0f * V.T;
    float3 B = 2.0f * V.B;

    float3x3 xform = mul((float3x3)m_xform_v, float3x3(
        T.x, B.x, N.x,
        T.y, B.y, N.y,
        T.z, B.z, N.z));

    O.M1 = xform[0];
    O.M2 = xform[1];
    O.M3 = xform[2];
#else
	N = mul((float3x3)m_xform_v, N);

    O.M1 = N.xxx;
    O.M2 = N.yyy;
    O.M3 = N.zzz;
#endif

    O.hpos = mul(m_VP, pos);

#ifndef DISABLE_MOTION_VECTORS
    O.hpos_curr = O.hpos;
    O.hpos_old = mul(m_VP_old, pos_old);
#endif

    O.hpos.xy += m_taa_jitter.xy * O.hpos.w;
}


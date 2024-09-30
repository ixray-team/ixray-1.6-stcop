#include "common.hlsli"

#ifdef USE_LM_HEMI
    #define v_in v_static
#else
    #define v_in v_static_color
#endif

void main(in v_in I, out p_bumped_new O)
{
    float2 tc = unpack_tc_base(I.tc, I.T.w, I.B.w);
    float3 Pe = mul(m_WV, I.P);

    O.tcdh = float4(tc.xy, I.Nh.w, I.Nh.w);
    O.position = float4(Pe, 1.0f);

    float3 N = unpack_bx4(unpack_D3DCOLOR(I.Nh).xyz);
	
#if defined(USE_BUMP) || defined(USE_TDETAIL_BUMP)
    float3 T = unpack_bx4(unpack_D3DCOLOR(I.T).xyz);
    float3 B = unpack_bx4(unpack_D3DCOLOR(I.B).xyz);

    float3x3 xform = mul((float3x3)m_WV, float3x3(
        T.x, B.x, N.x,
        T.y, B.y, N.y,
        T.z, B.z, N.z));

    O.M1 = xform[0];
    O.M2 = xform[1];
    O.M3 = xform[2];
#else
	N = mul((float3x3)m_WV, N);

    O.M1 = N.xxx;
    O.M2 = N.yyy;
    O.M3 = N.zzz;
#endif

#ifdef USE_LM_HEMI
    O.tcdh.zw = unpack_tc_lmap(I.lmh);
#else
    O.tcdh.w = I.color.w;
#endif
	
	O.hpos = mul(m_WVP, I.P);
	
    O.hpos_curr = O.hpos;
    O.hpos_old = mul(m_WVP_old, I.P);

    O.hpos.xy += m_taa_jitter.xy * O.hpos.w;
}


#ifdef EDITOR_MODE
// Editable meshes of the editors use the model vertex format (float position, normal, basis, uv)
#include "deffer_model.vs.hlsl"
#else
#include "common.hlsli"

void main(PULL_INPUT, out p_bumped_new O)
{
    PulledVertex V = PullVertex(VertexId);
    float4 P = float4(V.P, 1.0f);
    float4 C = DecodeU8x4(V.C);
    float3 Pe = mul(m_WV, P);

    O.tcdh = float4(V.UV, C.x, C.x);
    O.position = float4(Pe, 1.0f);

    float3 N = 2.0f * V.N;

#if defined(USE_BUMP) || defined(USE_TDETAIL_BUMP)
    float3 T = 2.0f * V.T;
    float3 B = 2.0f * V.B;

    float3x3 xform = mul((float3x3)m_WV, float3x3(
        T.x, B.x, N.x,
        T.y, B.y, N.y,
        T.z, B.z, N.z)
	);

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
    O.tcdh.zw = DecodeS16x2(V.X) * (1.0f / 32768.0f);
#else
	#ifdef USE_LENGTH_BUFFER
		O.lmap = DecodeU8x4(V.X).xyz;
	#endif
    O.tcdh.w = C.y;
#endif

	O.hpos = mul(m_WVP, P);

#ifndef DISABLE_MOTION_VECTORS
    O.hpos_curr = O.hpos;
    O.hpos_old = mul(m_WVP_old, P);
#endif

    O.hpos.xy += m_taa_jitter.xy * O.hpos.w;
}
#endif

#ifdef USE_R1_STATIC_LIGHTING
#include "r1_lod.vs.hlsl"
#else
#include "common.hlsli"
#define L_SCALE 1.55f

void main(PULL_INPUT, out p_bilbord O)
{
    PackedVertex Imposter = g_RingVertices[draw_vertex_base + VertexId / 6u];
    uint Corner = (0x021203u >> (VertexId % 6u * 4u)) & 0xFu;
    PackedVertex A = LoadPacked(Imposter.N + (Imposter.TF & 7u) * 4u + Corner);
    PackedVertex B = LoadPacked(Imposter.N + ((Imposter.TF >> 3u) & 7u) * 4u + Corner);
    float2 AF = float2((Imposter.TF.xx >> uint2(8u, 16u)) & 0xFFu) / 255.0f;

    float4 pos = float4(lerp(B.P, A.P, AF.y) + Imposter.P, 1.0f);
    float h = lerp(DecodeU8x4(B.C).w, DecodeU8x4(A.C).w, AF.y) * L_SCALE;
    float s = lerp(float(B.X & 0xFFu), float(A.X & 0xFFu), AF.y) / 255.0f * L_SCALE;
    O.af = float4(h, s, AF);

    O.hpos = mul(m_VP, pos);
    O.position = mul(m_V, pos);

    O.tc0 = DecodeUV(B.TF, B.UV);
    O.tc1 = DecodeUV(A.TF, A.UV);

#ifndef DISABLE_MOTION_VECTORS
    O.hpos_curr = O.hpos;
    O.hpos_old = mul(m_VP_old, pos);
#endif

    O.hpos.xy += m_taa_jitter.xy * O.hpos.w;
}


#endif

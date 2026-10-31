#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float2 tc2 : TEXCOORD2;
    float2 tc3 : TEXCOORD3;
    float4 c : COLOR0;
    float4 f : COLOR1;
    float fog : TEXCOORD7;
    float4 hpos : SV_POSITION;
};

#define L_SCALE (1.55)
#define L_SUN_HACK (.7)

vf main(PULL_INPUT)
{
    vf o;

    PackedVertex Imposter = g_RingVertices[draw_vertex_base + VertexId / 6u];
    uint Corner = (0x021203u >> (VertexId % 6u * 4u)) & 0xFu;
    PackedVertex A = LoadPacked(Imposter.N + (Imposter.TF & 7u) * 4u + Corner);
    PackedVertex B = LoadPacked(Imposter.N + ((Imposter.TF >> 3u) & 7u) * 4u + Corner);
    float2 AF = float2((Imposter.TF.xx >> uint2(8u, 16u)) & 0xFFu) / 255.0f;
    float factor = AF.y;
    float4 pos = float4(lerp(B.P, A.P, factor) + Imposter.P, 1);
    o.hpos = mul(m_VP, pos);

    o.tc0 = DecodeUV(B.TF, B.UV);
    o.tc1 = DecodeUV(A.TF, A.UV);
    o.tc2 = o.tc0;
    o.tc3 = o.tc1;

    float3 normal = normalize(lerp(normalize(DecodeOctRaw(B.N)), normalize(DecodeOctRaw(A.N)), factor));
    normal.y += 1;
    normal = normalize(normal);
    float4 rgbh = lerp(DecodeU8x4(B.C), DecodeU8x4(A.C), factor) * L_SCALE;
    float sun = lerp(float(B.X & 0xFFu), float(A.X & 0xFFu), factor) / 255.0f * L_SCALE;
    float sun_c = 1 + L_SUN_HACK * dot(normal, L_sun_dir_w.xyz);

    float3 L_rgb = rgbh.rgb;
    float3 L_hemi = L_hemi_color.xyz * rgbh.w;
    float3 L_sun = L_sun_color.xyz * sun * sun_c;
    float3 L_final = L_rgb + L_hemi + L_sun + L_ambient.xyz;

    o.c = float4(L_final, AF.x);
    o.f = factor;
    o.fog = calc_fogging(pos);

    return o;
}

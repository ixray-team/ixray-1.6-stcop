#include "r1_common.hlsli"
#include "r1_static.hlsli"



struct vf
{
    float2 TEX0 : TEXCOORD0;
    float3 COL0 : COLOR0;
    float fog : TEXCOORD1;
    float4 HPOS : SV_POSITION;
};

vf main(PULL_INPUT)
{
    PulledVertex V = PullVertex(VertexId);
    vf o;

    float3 pos = mul(m_xform, float4(V.P, 1.0f));

    float base = m_xform._24; 
    float dp = calc_cyclic(wave.w + dot(pos, (float3)wave));
    float H = pos.y - base; 
    float frac = DecodeS16x2(V.X).x * consts.x; 
    float inten = H * dp; 
    float2 result = calc_xz_wave(wind.xz * inten, frac);
    float4 f_pos = float4(pos, 1); 

    o.fog = calc_fogging(f_pos);

    o.HPOS = mul(m_VP, f_pos);

    float3 N = mul((float3x3)m_xform, V.N); 
    float L_base = DecodeU8x4(V.C).x; 
    float4 L_unpack = c_scale * L_base + c_bias; 
    float3 L_rgb = L_unpack.xyz; 
    float3 L_hemi = L_hemi_color.xyz * saturate(.75f + .25f * N.y) * L_unpack.w; 
    float3 L_sun = r1_v_sun(N) * (L_base * c_sun.x + c_sun.y); 
    
    float3 L_final = L_rgb + L_hemi + L_sun;
    o.COL0 = L_final;

    o.TEX0.xy = V.UV;

    return o;
}

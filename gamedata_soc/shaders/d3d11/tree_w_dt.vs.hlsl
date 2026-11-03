#include "r1_common.hlsli"
#include "r1_static.hlsli"



struct vf
{
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1; 
    float4 c0 : COLOR0; 
    float4 c1 : COLOR1; 
    float fog : TEXCOORD7;
    float4 HPOS : SV_POSITION;
};

vf main(v_tree v)
{
    vf o;

    float3 pos = mul(m_xform, v.P);

    float base = m_xform._24; 
    float dp = calc_cyclic(wave.w + dot(pos, (float3)wave));
    float H = pos.y - base; 
    float frac = v.tc.z * consts.x; 
    float inten = H * dp; 
    float2 result = calc_xz_wave(wind.xz * inten, frac);
    float4 f_pos = float4(pos.x + result.x, pos.y, pos.z + result.y, 1);

    o.fog = calc_fogging(f_pos);

    o.HPOS = mul(m_VP, f_pos);

    float3 N = mul((float3x3)m_xform, unpack_normal(v.Nh.xyz)); 
    float L_base = v.Nh.w; 
    float4 L_unpack = c_scale * L_base + c_bias; 
    float3 L_rgb = L_unpack.xyz; 
    float3 L_hemi = L_hemi_color.xyz * saturate(.75f + .25f * N.y) * L_unpack.w; 
    float3 L_sun = r1_v_sun(N) * (L_base * c_sun.x + c_sun.y); 
    float3 L_final = L_rgb + L_hemi + L_sun;

    o.tc0.xy = (v.tc * consts).xy;
    o.tc1 = o.tc0 * dt_params; 
    float2 dt = calc_detail(f_pos); 
    o.c0 = float4(L_final, dt.x); 
    o.c1 = dt.y; 

    return o;
}

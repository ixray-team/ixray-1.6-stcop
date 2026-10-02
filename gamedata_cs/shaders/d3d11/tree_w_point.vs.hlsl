#include "r1_common.hlsli"
#include "r1_static.hlsli"



vf_point main(v_tree v)
{
    vf_point o;

    float3 pos = mul(m_xform, v.P);

    float base = m_xform._24; 
    float dp = calc_cyclic(wave.w + dot(pos, (float3)wave));
    float H = pos.y - base; 
    float frac = v.tc.z * consts.x; 
    float inten = H * dp; 
    float2 result = calc_xz_wave(wind.xz * inten, frac);
    float4 f_pos = float4(pos.x + result.x, pos.y, pos.z + result.y, 1);
    float3 f_N = normalize(mul((float3x3)m_xform, unpack_normal(v.Nh.xyz)));

    o.hpos = mul(m_VP, f_pos);
    o.tc0 = (v.tc * consts).xy;
    o.color = calc_point(o.tc1, o.tc2, f_pos, f_N);

    return o;
}

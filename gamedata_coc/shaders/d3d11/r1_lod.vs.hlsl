#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vv
{
    float3 pos0 : POSITION0;
    float3 pos1 : POSITION1;
    float3 n0 : NORMAL0;
    float3 n1 : NORMAL1;
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float4 rgbh0 : TEXCOORD2; 
    float4 rgbh1 : TEXCOORD3; 
    float4 sun_af : COLOR0; 
};

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

vf main(vv v)
{
    vf o;

    v.sun_af.xyz = v.sun_af.zyx;
    v.rgbh0.xyz = v.rgbh0.zyx;
    v.rgbh1.xyz = v.rgbh1.zyx;
    float factor = v.sun_af.w;
    float4 pos = float4(lerp(v.pos0, v.pos1, factor), 1);
    o.hpos = mul(m_VP, pos); 

    o.tc0 = v.tc0;
    o.tc1 = v.tc1;
    o.tc2 = v.tc0;
    o.tc3 = v.tc1;

    float3 normal = normalize(lerp(v.n0, v.n1, factor));
    normal.y += 1;
    normal = normalize(normal);
    float4 rgbh = lerp(v.rgbh0, v.rgbh1, factor) * L_SCALE;
    float sun = lerp(v.sun_af.x, v.sun_af.y, factor) * L_SCALE;
    float sun_c = 1 + L_SUN_HACK * dot(normal, L_sun_dir_w.xyz); 

    float3 L_rgb = rgbh.rgb; 
    float3 L_hemi = L_hemi_color.xyz * rgbh.w; 
    float3 L_sun = L_sun_color.xyz * sun * sun_c; 
    float3 L_final = L_rgb + L_hemi + L_sun + L_ambient.xyz;

    o.c = float4(L_final, v.sun_af.z);
    o.f = factor;
    o.fog = calc_fogging(pos); 

    return o;
}

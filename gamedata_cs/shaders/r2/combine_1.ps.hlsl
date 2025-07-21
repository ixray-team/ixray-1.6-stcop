#include "common.hlsli"

uniform sampler2D s_half_depth;
uniform float3x4 m_v2w;

#include "lmodel.hlsli"
#include "hmodel.hlsli"

#include "ssao_blur.ps.hlsl"
#include "ssao.ps.hlsl"

struct _input
{
    float4 hpos : POSITION;
    float2 tc0 : TEXCOORD0;
    float2 tcJ : TEXCOORD1;
};

float4 main(_input I) : COLOR0
{
    // Sample the buffers:
    float4 P = tex2D(s_position, I.tc0); // position.(mtl or sun)
    float4 N = tex2D(s_normal, I.tc0); // normal.hemi
    float4 D = tex2D(s_diffuse, I.tc0); // rgb.gloss
    float4 L = tex2D(s_accumulator, I.tc0); // diffuse.specular

#ifdef USE_GAMMA_22
    D.rgb = (D.rgb * D.rgb);
#endif

    // static sun
    float mtl = P.w;
#ifdef USE_R2_STATIC_SUN
    float sun_occ = P.w * 2.0f;
    mtl = xmaterial;
    L += Ldynamic_color * sun_occ * plight_infinity(mtl, P.xyz, N.xyz, Ldynamic_dir);
#endif

    //  Calculate SSAO
// #ifdef USE_SSAO_BLUR
//  float occ = ssao_blur_ps(I.tc0);
    float occ = calc_ssao(P, N, I.tc0, I.tcJ);

    float3 hdiffuse, hspecular;
    hmodel(hdiffuse, hspecular, mtl, N.w, D.w, P.xyz, N.xyz);

    hdiffuse *= occ;
    hspecular *= occ;

    float4 light = float4(L.xyz + hdiffuse, L.w);
    float4 C = D * light;
    float3 spec = C.www + hspecular;

    float3 color = C.xyz + spec;

    // here should be distance fog
    float distance = length(P.xyz);
    float fog = saturate(distance * fog_params.w + fog_params.x);
    color = lerp(color, fog_color, fog);

    return float4(color, fog * fog);
}

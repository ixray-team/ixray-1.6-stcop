#ifndef HMODEL_H
#define HMODEL_H

#include "common.h"

TextureCube env_s0;
TextureCube env_s1;
TextureCube sky_s0;
TextureCube sky_s1;

void hmodel
(
    out float3 hdiffuse, out float3 hspecular,
    float m, float h, float s, float3 Pnt, float3 normal
)
{
    // hscale - something like diffuse reflection
    float3 nw = mul(m_invV, normal);
    float hscale = h;

#ifdef USE_GAMMA_22
    hscale = (hscale * hscale);        // make it more linear
#endif

    // reflection vector
    float3	v2PntL = normalize(Pnt);
    float3	v2Pnt = mul(m_invV, v2PntL);
    float3	vreflect = reflect(v2Pnt, nw);
    float	hspec = .5h + .5h * dot(vreflect, v2Pnt);

    // material
    float4	light = s_material.SampleLevel(smp_material, float3(hscale, hspec, m), 0.0f).xxxy;

    // diffuse color
    float3	e0d = env_s0.SampleLevel(smp_rtlinear, nw, 0.0f);
    float3	e1d = env_s1.SampleLevel(smp_rtlinear, nw, 0.0f);
    float3	env_d = L_hemi_color.xyz * lerp(e0d, e1d, L_hemi_color.w);
    env_d *= env_d;	// contrast
    hdiffuse = env_d * light.xyz + L_ambient.rgb;

    // specular color
    vreflect.y = vreflect.y * 2 - 1;

    float3	e0s = env_s0.SampleLevel(smp_rtlinear, vreflect, 0.0f);
    float3	e1s = env_s1.SampleLevel(smp_rtlinear, vreflect, 0.0f);
    float3	env_s = L_hemi_color.xyz * lerp(e0s, e1s, L_hemi_color.w);
    env_s *= env_s;
    hspecular = env_s * light.w * s;
}
#endif

#ifndef        HMODEL_H
#define HMODEL_H

#include "common.h"

//uniform samplerCUBE         env_s0                ;
//uniform samplerCUBE         env_s1                ;
//uniform samplerCUBE         sky_s0                ;
//uniform samplerCUBE         sky_s1                ;

TextureCube		env_s0;
TextureCube		env_s1;
TextureCube		sky_s0;
TextureCube		sky_s1;

uniform float4	env_color;        // color.w  = lerp factor
uniform float3x4	m_v2w;

void hmodel
(
	out float3 hdiffuse, out float3 hspecular, 
	float m, float h, float s, float3 Pnt, float3 normal
)
{
    // hscale - something like diffuse reflection
	float3	nw		= mul( m_v2w, normal );
	float	hscale	= h;

	hscale	= (hscale*hscale);        // make it more linear

	// reflection vector
	float3 v2PntL = normalize( Pnt );
	float3 v2Pnt = mul( m_v2w, v2PntL );
	float3 vreflect = reflect( v2Pnt, nw );
	float hspec = .5h + .5h * dot( vreflect, v2Pnt );

	// material
	float4 light = s_material.SampleLevel( smp_material, float3( hscale, hspec, m ), 0 ).xxxy;

	// diffuse color
	float3 e0d = env_s0.SampleLevel( smp_rtlinear, nw, 0 );
	float3 e1d = env_s1.SampleLevel( smp_rtlinear, nw, 0 );
	float3 env_d = env_color.xyz * lerp( e0d, e1d, env_color.w );
			env_d *=env_d;	// contrast
			hdiffuse= env_d * light.xyz + L_ambient.rgb;

	// specular color
	vreflect.y = vreflect.y*2-1;	// fake remapping
	float3 e0s = env_s0.SampleLevel( smp_rtlinear, vreflect, 0 );
	float3 e1s = env_s1.SampleLevel( smp_rtlinear, vreflect, 0 );

	float3 env_s = env_color.xyz * lerp( e0s, e1s, env_color.w);
	env_s *= env_s;	// contrast

	hspecular = env_s*light.w*s;
}
#endif
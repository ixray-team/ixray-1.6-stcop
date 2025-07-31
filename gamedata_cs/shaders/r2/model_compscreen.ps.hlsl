#include "common.hlsli"
#include "sload.hlsli"

uniform	float4 		m_affects;

float get_noise(float2 co)
{
	return (frac(sin(dot(co.xy ,float2(12.9898,78.233))) * 43758.5453))*0.5;
};

f_deffer 	main	( p_flat I )
{
  f_deffer	O;

  // diffuse
  float3 D = tex2D(s_base, I.tcdh);	// IN:  rgb.a

#ifdef	USE_TDETAIL
	D.rgb	= 2*D.rgb*tex2D	(s_detail, I.tcdbump).rgb;
#endif

	// hemi,sun,material
	float 	ms	= xmaterial		;
#ifdef USE_LM_HEMI
	float4	lm 	= tex2D			(s_hemi, I.lmh);
//	float 	h  	= dot			(lm.rgb,1.h/3.h);
	float 	h  	= get_hemi(lm);
# ifdef USE_R2_STATIC_SUN
//		 	ms 	= lm.w			;
			ms 	= get_sun(lm);
# endif
#else
	float 	h	= I.position.w	;
# ifdef USE_R2_STATIC_SUN
		 	ms	= I.tcdh.w		;
# endif
#endif

	// ��� ��� �������	
	float noise	= get_noise(I.tcdh*timers.z) *  m_affects.x * m_affects.x * 30;

	D.r += noise+0.1;
	D.g += noise+0.1;
	D.b += noise+0.1;

	// 2. Standart output
	O.N             = float4(normalize((float3)I.N.xyz), h);
	O.P             = float4(I.position.xyz + O.N.xyz*def_virtualh/2.0f, ms);
	O.C             = float4(D.rgb, def_gloss);	// OUT: rgb.gloss

	return O;
}

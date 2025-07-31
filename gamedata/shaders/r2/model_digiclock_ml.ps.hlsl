#include "common.hlsli"
#include "sload.hlsli"

uniform	float4 		m_digiclock;
uniform	float4 		m_affects;

float get_noise(float2 co)
{
	return (frac(sin(dot(co.xy ,float2(12.9898,78.233))) * 43758.5453))*0.5;
};

f_deffer 	main	( p_flat I )
{
  f_deffer	O;
  float digit = m_digiclock.a;
  I.tcdh.x = digit + (I.tcdh.x * 0.1);

  // diffuse
  float3 D	= tbase		(I.tcdh);	// IN:  rgb.a

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

	float noise	= get_noise(I.tcdh*timers.z) *  m_affects.x * m_affects.x * 30;
	float4	t_noise = tex2D	(s_lmap,I.tcdh);
	D.rgb += t_noise.rgb*noise+0.1;

	// 2. Standart output
	O.Ne          = float4		(normalize((float3)I.N.xyz), 					h			);
	O.position    = float4 	(I.position.xyz + O.Ne.xyz*def_virtualh/2.h, 	ms			);
	O.C			= float4		(D.rgb,											def_gloss	);	// OUT: rgb.gloss

	return O;
}


#ifndef	SHADOW_H
#define SHADOW_H

#include "common.h"

//uniform	sampler	s_smap	: register(ps,s0);	// 2D/cube shadowmap
//Texture2D<float>	s_smap;		// 2D/cube shadowmap
//	Used for RGBA texture too ?!
Texture2D	s_smap : register(ps,t0);		// 2D/cube shadowmap
SamplerComparisonState		smp_smap;	//	Special comare sampler
sampler		smp_jitter;

#define	KERNEL	.6f

//////////////////////////////////////////////////////////////////////////////////////////
// hardware + PCF
//////////////////////////////////////////////////////////////////////////////////////////
float sample_hw_pcf (float4 tc,float4 shift)
{
	static const float 	ts = KERNEL / float(SMAP_size);

//	return tex2Dproj( s_smap, tc + tc.w * shift * ts ).x;
	
	tc.xyz 	/= tc.w;
	tc.xy 	+= shift.xy * ts;

	return s_smap.SampleCmpLevelZero( smp_smap, tc.xy, tc.z).x;
}

float shadow_hw( float4 tc )
{
  	float	s0		= sample_hw_pcf( tc, float4( -1, -1, 0, 0) );
  	float	s1		= sample_hw_pcf( tc, float4( +1, -1, 0, 0) );
  	float	s2		= sample_hw_pcf( tc, float4( -1, +1, 0, 0) );
  	float	s3		= sample_hw_pcf( tc, float4( +1, +1, 0, 0) );

	return	(s0+s1+s2+s3)/4.h;
}
//////////////////////////////////////////////////////////////////////////////////////////
//	D24X8+PCF
//////////////////////////////////////////////////////////////////////////////////////////
float shadow( float4 tc ) 
{
	return shadow_hw( tc ); 
}

//////////////////////////////////////////////////////////////////////////////////////////
// testbed

//uniform sampler2D	jitter0;
//uniform sampler2D	jitter1;
Texture2D	jitter0;
Texture2D	jitter1;
//uniform sampler2D	jitter2;
//uniform sampler2D	jitter3;
//uniform float4 		jitterS;

Texture2D	jitterMipped;

float4 	test 		(float4 tc, float2 offset)
{

//	float4	tcx	= float4 (tc.xy + tc.w*offset, tc.zw);
//	return 	tex2Dproj (s_smap,tcx);

	tc.xyz 	/= tc.w;
	tc.xy 	+= offset;
	return s_smap.SampleCmpLevelZero( smp_smap, tc.xy, tc.z).x;
}

float 	shadowtest 	(float4 tc, float4 tcJ)				// jittered sampling
{
	float4	r;

	const 	float 	scale 	= (2.7f/float(SMAP_size));

//	float4	J0 	= tex2Dproj	(jitter0,tcJ)*scale;
//	float4	J1 	= tex2Dproj	(jitter1,tcJ)*scale;
	tcJ.xy		/=	tcJ.w;
	float4	J0 	= jitter0.Sample( smp_jitter, tcJ )*scale;
	float4	J1 	= jitter1.Sample( smp_jitter, tcJ )*scale;

		r.x 	= test 	(tc,J0.xy).x;
		r.y 	= test 	(tc,J0.wz).y;
		r.z		= test	(tc,J1.xy).z;
		r.w		= test	(tc,J1.wz).x;

	return	dot(r,1.h/4.h);
}

float 	shadowtest_sun 	(float4 tc, float2 tcJ)			// jittered sampling
{
	float4	r;

//	const 	float 	scale 	= (2.0f/float(SMAP_size));
	const 	float 	scale 	= (0.7f/float(SMAP_size));
//	float4	J0 	= tex2D	(jitter0,tcJ)*scale;
//	float4	J1 	= tex2D	(jitter1,tcJ)*scale;
	float4	J0 	= jitter0.Sample( smp_jitter, tcJ )*scale;
	float4	J1 	= jitter1.Sample( smp_jitter, tcJ )*scale;

		r.x 	= test 	(tc,J0.xy).x;
		r.y 	= test 	(tc,J0.wz).y;
		r.z		= test	(tc,J1.xy).z;
		r.w		= test	(tc,J1.wz).x;

	return	dot(r,1.h/4.h);
}

float 	shadow_rain 	(float4 tc, float2 tcJ)			// jittered sampling
{
	float4	r;

	const 	float 	scale 	= (4.0f/float(SMAP_size));
//	float4	J0 	= jitter0.Sample( smp_jitter, tcJ )*scale;
//	float4	J1 	= jitter1.Sample( smp_jitter, tcJ )*scale;
	float4	J0 	= jitter0.Sample( smp_linear, tcJ )*scale;
	float4	J1 	= jitter1.Sample( smp_linear, tcJ )*scale;

	r.x 	= test 	(tc,J0.xy).x;
	r.y 	= test 	(tc,J0.wz).y;
	r.z		= test	(tc,J1.xy).z;
	r.w		= test	(tc,J1.wz).x;

//	float4	J0 	= jitterMipped.Sample( smp_base, tcJ )*scale;

//	r.x 	= test 	(tc,J0.xy).x;
//	r.y 	= test 	(tc,J0.wz).y;
//	r.z		= test	(tc,J0.yz).z;
//	r.w		= test	(tc,J0.xw).x;

	return	dot(r,1.h/4.h);
}

//////////////////////////////////////////////////////////////////////////////////////////
#ifdef  USE_SUNMASK	
float3x4 m_sunmask;	// ortho-projection
float sunmask( float4 P )
{
	float2 		tc	= mul( m_sunmask, P );		//
//	return 		tex2D( s_lmap, tc ).w;			// A8 
	return 		s_lmap.Sample( smp_linear, tc ).w;	// A8 	
}
#else
float sunmask( float4 P ) { return 1.h; }		// 
#endif
//////////////////////////////////////////////////////////////////////////////////////////
uniform float4x4	m_shadow;

#endif
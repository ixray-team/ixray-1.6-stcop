#ifndef	SHADOW_H
#define SHADOW_H

#include "common.h"

//uniform	sampler	s_smap	: register(ps,s0);	// 2D/cube shadowmap
//Texture2D<float>	s_smap;		// 2D/cube shadowmap
//	Used for RGBA texture too ?!
Texture2D	s_smap : register(ps,t0);		// 2D/cube shadowmap

Texture2D<float>	s_smap_minmax;		// 2D/cube shadowmap
#include "gather.ps"

SamplerComparisonState		smp_smap;	//	Special comare sampler
sampler		smp_jitter;

Texture2D	jitter0;
Texture2D	jitter1;
//uniform sampler2D	jitter2;
//uniform sampler2D	jitter3;
//uniform float4 		jitterS;

Texture2D	jitterMipped;

#ifndef USE_ULTRA_SHADOWS
#define	KERNEL	0.6f
#else
#define	KERNEL	1.0f
#endif

float modify_light( float light )
{
   return ( light > 0.7 ? 1.0 : lerp( 0.0, 1.0, saturate( light / 0.7 ) ) ); 
}

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


#ifdef SM_4_1

float pcf( float4 s, float2 fc, float z ) 
{
  return ( lerp( lerp( (z) <= (s).w, (z) <= (s).z, (fc).x ), lerp( (z) <= (s).x, (z) <= (s).y, (fc).x ), (fc).y ) );
}

float dx10_1_hw_hq_7x7( float3 tc )
{
   float s;
   float z = tc.z;

   static const float scale = float( SMAP_size );
   float2 fc = frac( tc.xy * scale );

   float4 s_3_0, s_1_0, s1_0, s3_0;
   float4 s_3_1, s_1_1, s1_1, s3_1;

   // row -3
   s_3_0 = s_smap.Gather( smp_nofilter, tc, int2( -3, -3 ) ); 
   s_1_0 = s_smap.Gather( smp_nofilter, tc, int2( -1, -3 ) );
   s1_0  = s_smap.Gather( smp_nofilter, tc, int2(  1, -3 ) ); 
   s3_0  = s_smap.Gather( smp_nofilter, tc, int2(  3, -3 ) );
   
   s  = pcf( s_3_0, fc, z ) + 
	pcf( s_1_0, fc, z ) + 
	pcf( s1_0, fc, z ) + 
	pcf( s3_0, fc, z ) + 
        pcf( float4( s_3_0.y, s_1_0.xw, s_3_0.z ), fc, z ) + 
        pcf( float4( s_1_0.y, s1_0.xw,  s_1_0.z ), fc, z ) + 
        pcf( float4( s1_0.y,  s3_0.xw,  s1_0.z ), fc, z );

   // row -2
   s_3_1 = s_smap.Gather( smp_nofilter, tc, int2( -3, -1 ) ); 
   s_1_1 = s_smap.Gather( smp_nofilter, tc, int2( -1, -1 ) );
   s1_1  = s_smap.Gather( smp_nofilter, tc, int2(  1, -1 ) ); 
   s3_1  = s_smap.Gather( smp_nofilter, tc, int2(  3, -1 ) );
   s += pcf( float4( s_3_1.wz, s_3_0.yx ), fc, z ) + 
        pcf( float4( s_3_1.z, s_1_1.w, s_1_0.x, s_3_0.y ), fc, z ) + 
        pcf( float4( s_1_1.wz, s_1_0.yx ), fc, z ) + 
        pcf( float4( s_1_1.z, s1_1.w, s1_0.x, s_1_0.y ), fc, z ) + 
        pcf( float4( s1_1.wz, s1_0.yx ), fc, z ) + 
        pcf( float4( s1_1.z, s3_1.w, s3_0.x, s1_0.y ), fc, z ) +
	pcf( float4( s3_1.wz, s3_0.yx ), fc, z );

   // row -1
   s_3_0 = s_3_1; s_1_0 = s_1_1; s1_0 = s1_1; s3_0 = s3_1;
   s += pcf( s_3_0, fc, z ) + 
	pcf( s_1_0, fc, z ) + 
	pcf( s1_0, fc, z ) + 
	pcf( s3_0, fc, z ) + 
        pcf( float4( s_3_0.y, s_1_0.xw, s_3_0.z ), fc, z ) + 
        pcf( float4( s_1_0.y, s1_0.xw,  s_1_0.z ), fc, z ) + 
        pcf( float4( s1_0.y,  s3_0.xw,  s1_0.z ), fc, z );

   // row 0
   s_3_1 = s_smap.Gather( smp_nofilter, tc, int2( -3, 1 ) ); 
   s_1_1 = s_smap.Gather( smp_nofilter, tc, int2( -1, 1 ) );
   s1_1  = s_smap.Gather( smp_nofilter, tc, int2(  1, 1 ) ); 
   s3_1  = s_smap.Gather( smp_nofilter, tc, int2(  3, 1 ) );
   s += pcf( float4( s_3_1.wz, s_3_0.yx ), fc, z ) + 
        pcf( float4( s_3_1.z, s_1_1.w, s_1_0.x, s_3_0.y ), fc, z ) + 
        pcf( float4( s_1_1.wz, s_1_0.yx ), fc, z ) + 
        pcf( float4( s_1_1.z, s1_1.w, s1_0.x, s_1_0.y ), fc, z ) + 
        pcf( float4( s1_1.wz, s1_0.yx ), fc, z ) + 
        pcf( float4( s1_1.z, s3_1.w, s3_0.x, s1_0.y ), fc, z ) +
	pcf( float4( s3_1.wz, s3_0.yx ), fc, z );

   // row 1
   s_3_0 = s_3_1; s_1_0 = s_1_1; s1_0 = s1_1; s3_0 = s3_1;
   s += pcf( s_3_0, fc, z ) + 
	pcf( s_1_0, fc, z ) + 
	pcf( s1_0, fc, z ) + 
	pcf( s3_0, fc, z ) + 
        pcf( float4( s_3_0.y, s_1_0.xw, s_3_0.z ), fc, z ) + 
        pcf( float4( s_1_0.y, s1_0.xw,  s_1_0.z ), fc, z ) + 
        pcf( float4( s1_0.y,  s3_0.xw,  s1_0.z ), fc, z );

   // row 2
   s_3_1 = s_smap.Gather( smp_nofilter, tc, int2( -3, 3 ) ); 
   s_1_1 = s_smap.Gather( smp_nofilter, tc, int2( -1, 3 ) );
   s1_1  = s_smap.Gather( smp_nofilter, tc, int2(  1, 3 ) ); 
   s3_1  = s_smap.Gather( smp_nofilter, tc, int2(  3, 3 ) );
   s += pcf( float4( s_3_1.wz, s_3_0.yx ), fc, z ) + 
        pcf( float4( s_3_1.z, s_1_1.w, s_1_0.x, s_3_0.y ), fc, z ) + 
        pcf( float4( s_1_1.wz, s_1_0.yx ), fc, z ) + 
        pcf( float4( s_1_1.z, s1_1.w, s1_0.x, s_1_0.y ), fc, z ) + 
        pcf( float4( s1_1.wz, s1_0.yx ), fc, z ) + 
        pcf( float4( s1_1.z, s3_1.w, s3_0.x, s1_0.y ), fc, z ) +
	pcf( float4( s3_1.wz, s3_0.yx ), fc, z );

   // row 3
   s_3_0 = s_3_1; s_1_0 = s_1_1; s1_0 = s1_1; s3_0 = s3_1;
   s += pcf( s_3_0, fc, z ) + 
	pcf( s_1_0, fc, z ) + 
	pcf( s1_0, fc, z ) + 
	pcf( s3_0, fc, z ) + 
        pcf( float4( s_3_0.y, s_1_0.xw, s_3_0.z ), fc, z ) + 
        pcf( float4( s_1_0.y, s1_0.xw,  s_1_0.z ), fc, z ) + 
        pcf( float4( s1_0.y,  s3_0.xw,  s1_0.z ), fc, z );

   return s/49.0;
}
#endif

float dx10_0_hw_hq_7x7( float4 tc )
{
   float s = 0;
   for( int i = -3; i <= 3; ++i )
   {
      for( int j = -3; j <= 3; ++j )
      {
         s += sample_hw_pcf( tc, float4( j , i , 0, 0) ); 
      }
   }

	return	s/49.0;
}


#ifdef SM_MINMAX
bool cheap_reject( float3 tc, inout bool full_light ) 
{
   float4 plane0  = sm_minmax_gather( tc.xy, int2( -1,-1 ) );
   float4 plane1  = sm_minmax_gather( tc.xy, int2(  1,-1 ) );
   float4 plane2  = sm_minmax_gather( tc.xy, int2( -1, 1 ) );
   float4 plane3  = sm_minmax_gather( tc.xy, int2(  1, 1 ) );
   bool plane     = all( ( plane0 >= (0).xxxx ) * ( plane1 >= (0).xxxx ) * ( plane2 >= (0).xxxx ) * ( plane3 >= (0).xxxx ) );

   [flatten] if( !plane ) // if there are no proper plane equations in the support region
   {
      bool no_plane  = all( ( plane0 < (0).xxxx ) * ( plane1 < (0).xxxx ) * ( plane2 < (0).xxxx ) * ( plane3 < (0).xxxx ) );
      float4 z       = ( tc.z - 0.0005 ).xxxx;
      bool reject    = all( ( z > -plane0 ) * ( z > -plane1 ) * ( z > -plane2 ) * ( z > -plane3 ) ); 
      [flatten] if( no_plane && reject )
      {
         full_light = false;
         return true;
      }
      else
      {
         return false;
      }
   }
   else // plane equation detected
   {
      // compute corrected z for texel pos
      static const float scale = float( SMAP_size / 4 );
      float2 fc  = frac( tc.xy * scale );
      float  z   = lerp( lerp( plane0.y, plane1.x, fc.x ), lerp( plane2.z, plane3.w, fc.x ), fc.y );

      // do minmax test with new z
      full_light = ( ( tc.z - 0.0001 ) <= z );

      return true; 
   }
}

#endif	//	SM_MINMAX

float shadow_hw_hq( float4 tc )
{
#ifdef SM_MINMAX
   bool   full_light = false; 
   bool   cheap_path = cheap_reject( tc.xyz / tc.w, full_light );

   [branch] if( cheap_path )
   {
      [branch] if( full_light == true )
         return 1.0;
      else
         return sample_hw_pcf( tc, (0).xxxx ); 
   }
   else
   {
//#ifndef SM_4_1
      return dx10_0_hw_hq_7x7( tc ); 
//#else
//      return dx10_1_hw_hq_7x7( tc.xyz / tc.w ); // does not compiler because of HLSL compiler bug
///#endif
   }
#else //	SM_MINMAX
#ifndef SM_4_1
   return dx10_0_hw_hq_7x7( tc );
#else
   return dx10_1_hw_hq_7x7( tc.xyz / tc.w );
#endif
#endif //	SM_MINMAX
}

//////////////////////////////////////////////////////////////////////////////////////////
//	D24X8+PCF
//////////////////////////////////////////////////////////////////////////////////////////
float shadow( float4 tc ) 
{
#ifdef USE_ULTRA_SHADOWS
	return modify_light( shadow_hw_hq( tc ) ); 
#else
	return shadow_hw( tc ); 
#endif
}

#ifdef SM_MINMAX

//////////////////////////////////////////////////////////////////////////////////////////
// hardware + PCF
//////////////////////////////////////////////////////////////////////////////////////////

float shadow_dx10_1( float4 tc, float2 tcJ, float2 pos2d ) 
{
   return shadow( tc ); 
}

float shadow_dx10_1_sunshafts( float4 tc, float2 pos2d ) 
{
   float3 t         = tc.xyz / tc.w;
   float minmax     = s_smap_minmax.SampleLevel( smp_nofilter, t, 0 ).x;
   bool   umbra     = ( ( minmax.x < 0 ) && ( t.z > -minmax.x ) );

   [branch] if( umbra )
   {
      return 0.0;
   }
   else
   {
      return shadow_hw( tc ); 
   }
}

#endif


//////////////////////////////////////////////////////////////////////////////////////////
// testbed

//uniform sampler2D	jitter0;
//uniform sampler2D	jitter1;
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
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

	tc.xyz 	/= tc.w;
	tc.xy 	+= shift.xy * ts;

	return s_smap.SampleCmpLevelZero( smp_smap, tc.xy, tc.z).x;
}

#define GS2 3

//#ifndef SM_5
float shadow_hw( float4 tc )
{
  	float	s0		= sample_hw_pcf( tc, float4( -1, -1, 0, 0) );
  	float	s1		= sample_hw_pcf( tc, float4( +1, -1, 0, 0) );
  	float	s2		= sample_hw_pcf( tc, float4( -1, +1, 0, 0) );
  	float	s3		= sample_hw_pcf( tc, float4( +1, +1, 0, 0) );

	return	(s0+s1+s2+s3)/4.h;
}
/*
#else
float shadow_hw( float4 tc )
{
   tc.xyz /= tc.w;
   float  s = 0.0f;
   float2 stc = ( SMAP_size * tc.xy ) + float2( 0.5, 0.5 );
   float2 tcs = floor( stc );
   float2 fc;
   int    row;
   int    col;

   fc.xy = stc - tcs;
   tc.xy = tcs * ( 1.0 / SMAP_size );
   
   // loop over the rows
   for( row = -GS2; row <= GS2; row += 2 )
   {
       [unroll]for( col = -GS2; col <= GS2; col += 2 )
       {
            float4 v = s_smap.GatherCmpRed( smp_smap, tc.xy, tc.z, int2( col, row ) ); 
            
            if( row == -GS2 ) // top row
            {
                if( col == -GS2 ) // left
                    s += dot( float4( 1.0-fc.x, 1.0, 1.0-fc.y, (1.0-fc.x)*(1.0-fc.y) ), v );
                else if( col == GS2 ) // right
                    s += dot( float4( 1.0f, fc.x, fc.x*(1.0-fc.y), 1.0-fc.y ), v );
                else // center
                    s += dot( float4( 1.0, 1.0, 1.0-fc.y, 1.0-fc.y ), v );
            }
            else if( row == GS2 )  // bottom row
            {
                if( col == -GS2 ) // left
                    s += dot( float4( (1.0-fc.x)*fc.y, fc.y, 1.0, (1.0-fc.x) ), v );
                else if( col == GS2 ) // right
                    s += dot( float4( fc.y, fc.x*fc.y, fc.x, 1.0 ), v );
                else // center
                    s += dot( float4(fc.yy,1.0,1.0), v );
            }
            else // center rows
            {
                if( col == -GS2 ) // left
                    s += dot( float4( (1.0-fc.x), 1.0, 1.0, (1.0-fc.x) ), v ); 
                else if( col == GS2 ) // right
                    s += dot( float4( 1.0, fc.x, fc.x, 1.0 ), v ); 
                else // center
                    s += dot( (1.0).xxxx, v ); 
            }
        }
   }
  
   return s*(1.0/49);
}
#endif
*/
#ifdef SM_5

#define FILTER_SIZE	11
#define FS  FILTER_SIZE
#define FS2 ( FILTER_SIZE / 2 )

static const float W2[11][11] = 
                 { { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 }, 
			       { 0.0,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.0 },
			       { 0.0,0.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.2,0.0 },
			       { 0.0,0.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.2,0.0 },
			       { 0.0,0.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.2,0.0 },
			       { 0.0,0.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.2,0.0 },
			       { 0.0,0.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.2,0.0 },
			       { 0.0,0.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.2,0.0 },
			       { 0.0,0.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.2,0.0 },
			       { 0.0,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.0 },
			       { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 },
			       };

static const float W1[11][11] = 
                 { { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 }, 
			       { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 },
			       { 0.0,0.0,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.0,0.0 },
			       { 0.0,0.0,0.2,1.0,1.0,1.0,1.0,1.0,0.2,0.0,0.0 },
			       { 0.0,0.0,0.2,1.0,1.0,1.0,1.0,1.0,0.2,0.0,0.0 },
			       { 0.0,0.0,0.2,1.0,1.0,1.0,1.0,1.0,0.2,0.0,0.0 },
			       { 0.0,0.0,0.2,1.0,1.0,1.0,1.0,1.0,0.2,0.0,0.0 },
			       { 0.0,0.0,0.2,1.0,1.0,1.0,1.0,1.0,0.2,0.0,0.0 },
			       { 0.0,0.0,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.0,0.0 },
			       { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 },
			       { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 },
			       };

static const float W0[11][11] = 
                 { { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 }, 
			       { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 },
			       { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 },
			       { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 },
			       { 0.0,0.0,0.0,0.0,0.8,0.8,0.8,0.0,0.0,0.0,0.0 },
			       { 0.0,0.0,0.0,0.0,0.8,1.0,0.8,0.0,0.0,0.0,0.0 },
			       { 0.0,0.0,0.0,0.0,0.8,0.8,0.8,0.0,0.0,0.0,0.0 },
			       { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 },
			       { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 },
			       { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 },
			       { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 },
			       };

float Fw( int r, int c, float fL )
{
	return        (1.0-fL) * (1.0-fL) * (1.0-fL) * W0[r][c] +
	       3.0f * (1.0-fL) * (1.0-fL) *   fL     * W1[r][c] +
	       3.0f *    fL    *    fL    * (1.0-fL) * W2[r][c] +
                     fL    *    fL    *   fL     * 1.0f;
} 

#define BLOCKER_FILTER_SIZE	11
#define BFS  BLOCKER_FILTER_SIZE
#define BFS2 ( BLOCKER_FILTER_SIZE / 2 )

#define SUN_WIDTH 300.0f

float dx11_hw_extreme_quality( float3 tc )
{
   float  s   = 0.0f;
   float2 stc = ( SMAP_size * tc.xy ) + float2( 0.5, 0.5 );
   float2 tcs = floor( stc );
   float2 fc;
   int    row;
   int    col;
   float  w = 0.0;
   float  avgBlockerDepth = 0;
   float  blockerCount = 0;
   float  fRatio;
   float4 v1[ FS2 + 1 ];
   float2 v0[ FS2 + 1 ];
   float2 off;

   fc     = stc - tcs;
   tc.xy  = tc.xy - ( (1.0/SMAP_size) * fc );
   tc.z  -= 0.0001;

   // loop over the rows
   for( row = -BFS2; row <= BFS2; row += 2 )
   {
	   [unroll]for( col = -BFS2; col <= BFS2; col += 2 )
	   {
		   float4 d4 = s_smap.GatherRed( smp_nofilter, tc.xy, int2( col, row ) );
		   float4 b4 = ( tc.zzzz <= d4 ) ? (0.0).xxxx : (1.0).xxxx; 

		   blockerCount    = blockerCount + dot( b4, (1.0).xxxx );
		   avgBlockerDepth = avgBlockerDepth + dot( d4, b4 );
	   }
   }
   
   // compute ratio average blocker depth vs. pixel depth
   if( blockerCount > 0.0 )
   {
	   avgBlockerDepth /= blockerCount;
       fRatio = saturate( ( ( tc.z - avgBlockerDepth ) * SUN_WIDTH ) / avgBlockerDepth );
       fRatio *= fRatio;
   }
   else
   {
	   fRatio = 0.0; 
   }

   for( row = 0; row < FS; ++row )
   {
      for( col = 0; col < FS; ++col )
         w += Fw(row,col,fRatio);
   }

   [unroll(11)]for( row = -FS2; row <= FS2; row += 2 )
   {
	   for( col = -FS2; col <= FS2; col += 2 )
	   {
		  v1[(col+FS2)/2] = s_smap.GatherCmpRed( smp_smap, tc.xy, tc.z, int2( col, row ) );
		  
		  if( col == -FS2 )
		  {
			 s += ( 1 - fc.y ) * ( v1[0].w * ( Fw(row+FS2,0,fRatio) - Fw(row+FS2,0,fRatio) * fc.x ) + v1[0].z * ( fc.x * ( Fw(row+FS2,0,fRatio) - Fw(row+FS2,1,fRatio) ) +  Fw(row+FS2,1,fRatio) ) );
			 s += (     fc.y ) * ( v1[0].x * ( Fw(row+FS2,0,fRatio) - Fw(row+FS2,0,fRatio) * fc.x ) + v1[0].y * ( fc.x * ( Fw(row+FS2,0,fRatio) - Fw(row+FS2,1,fRatio) ) +  Fw(row+FS2,1,fRatio) ) );
			 if( row > -FS2 )
			 {
				s += ( 1 - fc.y ) * ( v0[0].x * ( Fw(row+FS2-1,0,fRatio) - Fw(row+FS2-1,0,fRatio) * fc.x ) + v0[0].y * ( fc.x * ( Fw(row+FS2-1,0,fRatio) - Fw(row+FS2-1,1,fRatio) ) +  Fw(row+FS2-1,1,fRatio) ) );
				s += (     fc.y ) * ( v1[0].w * ( Fw(row+FS2-1,0,fRatio) - Fw(row+FS2-1,0,fRatio) * fc.x ) + v1[0].z * ( fc.x * ( Fw(row+FS2-1,0,fRatio) - Fw(row+FS2-1,1,fRatio) ) +  Fw(row+FS2-1,1,fRatio) ) );
			 }
		  }
		  else if( col == FS2 )
		  {
			 s += ( 1 - fc.y ) * ( v1[FS2].w * ( fc.x * ( Fw(row+FS2,FS-2,fRatio) - Fw(row+FS2,FS-1,fRatio) ) + Fw(row+FS2,FS-1,fRatio) ) + v1[FS2].z * fc.x * Fw(row+FS2,FS-1,fRatio) );
			 s += (     fc.y ) * ( v1[FS2].x * ( fc.x * ( Fw(row+FS2,FS-2,fRatio) - Fw(row+FS2,FS-1,fRatio) ) + Fw(row+FS2,FS-1,fRatio) ) + v1[FS2].y * fc.x * Fw(row+FS2,FS-1,fRatio) );
			 if( row > -FS2 )
			 {
				s += ( 1 - fc.y ) * ( v0[FS2].x * ( fc.x * ( Fw(row+FS2-1,FS-2,fRatio) - Fw(row+FS2-1,FS-1,fRatio) ) + Fw(row+FS2-1,FS-1,fRatio) ) + v0[FS2].y * fc.x * Fw(row+FS2-1,FS-1,fRatio) );
				s += (     fc.y ) * ( v1[FS2].w * ( fc.x * ( Fw(row+FS2-1,FS-2,fRatio) - Fw(row+FS2-1,FS-1,fRatio) ) + Fw(row+FS2-1,FS-1,fRatio) ) + v1[FS2].z * fc.x * Fw(row+FS2-1,FS-1,fRatio) );
			 }
		  }
		  else
		  {
			 s += ( 1 - fc.y ) * ( v1[(col+FS2)/2].w * ( fc.x * ( Fw(row+FS2,col+FS2-1,fRatio) - Fw(row+FS2,col+FS2+0,fRatio) ) + Fw(row+FS2,col+FS2+0,fRatio) ) +
						           v1[(col+FS2)/2].z * ( fc.x * ( Fw(row+FS2,col+FS2-0,fRatio) - Fw(row+FS2,col+FS2+1,fRatio) ) + Fw(row+FS2,col+FS2+1,fRatio) ) );
			 s += (     fc.y ) * ( v1[(col+FS2)/2].x * ( fc.x * ( Fw(row+FS2,col+FS2-1,fRatio) - Fw(row+FS2,col+FS2+0,fRatio) ) + Fw(row+FS2,col+FS2+0,fRatio) ) +
						           v1[(col+FS2)/2].y * ( fc.x * ( Fw(row+FS2,col+FS2-0,fRatio) - Fw(row+FS2,col+FS2+1,fRatio) ) + Fw(row+FS2,col+FS2+1,fRatio) ) );
			 if( row > -FS2 )
			 {
				s += ( 1 - fc.y ) * ( v0[(col+FS2)/2].x * ( fc.x * ( Fw(row+FS2-1,col+FS2-1,fRatio) - Fw(row+FS2-1,col+FS2+0,fRatio) ) + Fw(row+FS2-1,col+FS2+0,fRatio) ) +
							          v0[(col+FS2)/2].y * ( fc.x * ( Fw(row+FS2-1,col+FS2-0,fRatio) - Fw(row+FS2-1,col+FS2+1,fRatio) ) + Fw(row+FS2-1,col+FS2+1,fRatio) ) );
				s += (     fc.y ) * ( v1[(col+FS2)/2].w * ( fc.x * ( Fw(row+FS2-1,col+FS2-1,fRatio) - Fw(row+FS2-1,col+FS2+0,fRatio) ) + Fw(row+FS2-1,col+FS2+0,fRatio) ) +
							          v1[(col+FS2)/2].z * ( fc.x * ( Fw(row+FS2-1,col+FS2-0,fRatio) - Fw(row+FS2-1,col+FS2+1,fRatio) ) + Fw(row+FS2-1,col+FS2+1,fRatio) ) );
			 }
	      }
		  if( row != FS2 )
			v0[(col+FS2)/2] = v1[(col+FS2)/2].xy;
	   }
   }

   return s/w;
}

#endif

#ifdef SM_4_1

float dx10_1_hw_hq_7x7( float3 tc )
{
   float  s = 0.0f;
   float2 stc = ( SMAP_size * tc.xy ) + float2( 0.5, 0.5 );
   float2 tcs = floor( stc );
   float2 fc;
   int    row;
   int    col;

   fc.xy = stc - tcs;
   tc.xy = tcs * ( 1.0 / SMAP_size );
   
   // loop over the rows
   for( row = -GS2; row <= GS2; row += 2 )
   {
       [unroll]for( col = -GS2; col <= GS2; col += 2 )
       {
            float4 v = ( tc.zzzz <= s_smap.Gather( smp_nofilter, tc.xy, int2( col, row ) ) ) ? (1.0).xxxx : (0.0).xxxx; 
            
            if( row == -GS2 ) // top row
            {
                if( col == -GS2 ) // left
                    s += dot( float4( 1.0-fc.x, 1.0, 1.0-fc.y, (1.0-fc.x)*(1.0-fc.y) ), v );
                else if( col == GS2 ) // right
                    s += dot( float4( 1.0f, fc.x, fc.x*(1.0-fc.y), 1.0-fc.y ), v );
                else // center
                    s += dot( float4( 1.0, 1.0, 1.0-fc.y, 1.0-fc.y ), v );
            }
            else if( row == GS2 )  // bottom row
            {
                if( col == -GS2 ) // left
                    s += dot( float4( (1.0-fc.x)*fc.y, fc.y, 1.0, (1.0-fc.x) ), v );
                else if( col == GS2 ) // right
                    s += dot( float4( fc.y, fc.x*fc.y, fc.x, 1.0 ), v );
                else // center
                    s += dot( float4(fc.yy,1.0,1.0), v );
            }
            else // center rows
            {
                if( col == -GS2 ) // left
                    s += dot( float4( (1.0-fc.x), 1.0, 1.0, (1.0-fc.x) ), v ); 
                else if( col == GS2 ) // right
                    s += dot( float4( 1.0, fc.x, fc.x, 1.0 ), v ); 
                else // center
                    s += dot( (1.0).xxxx, v ); 
            }
        }
   }
  
   return s*(1.0/49.0);
}

#endif

float dx10_0_hw_hq_7x7( float4 tc )
{
   tc.xyz /= tc.w;

   float  s   = 0.0;
   float2 stc = ( SMAP_size * tc.xy ) + float2( 0.5, 0.5 );
   float2 tcs = floor( stc );
   float2 fc;

   fc    = stc - tcs;
   tc.xy = tc.xy - ( fc * ( 1.0/SMAP_size ) );

   float2 pwAB = ( ( 2.0 ).xx - fc ); 	
   float2 tcAB = ( 1.0/SMAP_size ).xx / pwAB;
   float2 tcM  = (0.5/SMAP_size ).xx;
   float2 pwGH = ( ( 1.0 ).xx + fc );
   float2 tcGH = (1.0/SMAP_size) * ( fc / pwGH );

   for( int row = -GS2; row <= GS2; row += 2 )
   {
      for( int col = -GS2; col <= GS2; col += 2 )
	  {
		if( row == -GS2 ) // top row
		{
			if( col == -GS2 ) // left
				s += ( pwAB.x * pwAB.y ) * s_smap.SampleCmpLevelZero( smp_smap, tc.xy + tcAB, tc.z, int2( col, row ) ).x;
			else if( col == GS2 ) // right
				s += ( pwGH.x * pwAB.y ) * s_smap.SampleCmpLevelZero( smp_smap, tc.xy + float2( tcGH.x, tcAB.y), tc.z, int2( col, row ) ).x;
			else // center
				s += (    2.0 * pwAB.y ) * s_smap.SampleCmpLevelZero( smp_smap, tc.xy + float2( tcM.x, tcAB.y), tc.z, int2( col, row ) ).x;
		}
		else if( row == GS2 )  // bottom row
		{
			if( col == -GS2 ) // left
				s += ( pwAB.x * pwGH.y ) * s_smap.SampleCmpLevelZero( smp_smap, tc.xy + float2( tcAB.x, tcGH.y ), tc.z, int2( col, row ) ).x;
			else if( col == GS2 ) // right
				s += ( pwGH.x * pwGH.y ) * s_smap.SampleCmpLevelZero( smp_smap, tc.xy + tcGH, tc.z, int2( col, row ) ).x;
			else // center
				s += (    2.0 * pwGH.y ) * s_smap.SampleCmpLevelZero( smp_smap, tc.xy + float2( tcM.x, tcGH.y ), tc.z, int2( col, row ) ).x;
		}
		else // center rows
		{
			if( col == -GS2 ) // left
				s += ( pwAB.x * 2.0    ) * s_smap.SampleCmpLevelZero( smp_smap, tc.xy + float2( tcAB.x, tcM.y ), tc.z, int2( col, row ) ).x;
			else if( col == GS2 ) // right
				s += ( pwGH.x * 2.0    ) * s_smap.SampleCmpLevelZero( smp_smap, tc.xy + float2( tcGH.x, tcM.y),  tc.z, int2( col, row ) ).x;
			else // center
				s += (    2.0 * 2.0    ) * s_smap.SampleCmpLevelZero( smp_smap, tc.xy + tcM, tc.z, int2( col, row ) ).x;
		}
      }
	}

    return s/49.0;
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
#ifdef SM_5
#if SUN_QUALITY==4 // extreme quality
      return dx11_hw_extreme_quality( tc.xyz / tc.w );
#else // SUN_QUALITY==4
#ifdef SM_4_1
      return dx10_1_hw_hq_7x7( tc.xyz / tc.w );
#else // SM_4_1
      return dx10_0_hw_hq_7x7( tc ); 
#endif // SM_4_1
#endif //SUN_QUALITY==4
#else // SM_5
#ifdef SM_4_1
      return dx10_1_hw_hq_7x7( tc.xyz / tc.w );
#else // SM_4_1
      return dx10_0_hw_hq_7x7( tc ); 
#endif// SM_4_1
#endif // SM_5
   }
#else //	SM_MINMAX
#ifdef SM_5
#if SUN_QUALITY==4 // extreme quality
      return dx11_hw_extreme_quality( tc.xyz / tc.w );
#else // SUN_QUALITY==4
#ifdef SM_4_1
      return dx10_1_hw_hq_7x7( tc.xyz / tc.w );
#else // SM_4_1
      return dx10_0_hw_hq_7x7( tc ); 
#endif // SM_4_1
#endif //SUN_QUALITY==4
#else // SM_5
#ifdef SM_4_1
      return dx10_1_hw_hq_7x7( tc.xyz / tc.w );
#else // SM_4_1
      return dx10_0_hw_hq_7x7( tc ); 
#endif// SM_4_1
#endif // SM_5
#endif //	SM_MINMAX
}

//////////////////////////////////////////////////////////////////////////////////////////
//	D24X8+PCF
//////////////////////////////////////////////////////////////////////////////////////////
float shadow( float4 tc ) 
{
#ifdef USE_ULTRA_SHADOWS
#ifdef SM_MINMAX
	return modify_light( shadow_hw_hq( tc ) ); 
#else
	return shadow_hw_hq( tc ); 
#endif
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
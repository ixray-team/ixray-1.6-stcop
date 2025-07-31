#ifndef	common_defines_h_included
#define	common_defines_h_included

//////////////////////////////////////////////////////////////////////////////////////////
// Defines                                		//
#define def_gloss       half(2.f /255.f)
#define def_aref        half(200.f/255.f)
#define def_dbumph      half(0.333f)
#define def_virtualh    half(0.05f)              // 5cm
#define def_distort     half(0.05f)             // we get -0.5 .. 0.5 range, this is -512 .. 512 for 1024, so scale it
#define def_hdr         half(9.h)         		// hight luminance range half(3.h)
#define def_hdr_clip	half(0.75h)        		//

#define	LUMINANCE_VECTOR	half3(0.3f, 0.38f, 0.22f)

//////////////////////////////////////////////////////////////////////////////////////////
#ifndef SMAP_size
#define SMAP_size        1024
#endif
#define PARALLAX_H 0.02
#define parallax float2(PARALLAX_H, -PARALLAX_H/2)
//////////////////////////////////////////////////////////////////////////////////////////

#endif	//	common_defines_h_included
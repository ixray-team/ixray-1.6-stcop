#ifndef	LMODEL_H
#define LMODEL_H

#include "common.h"

//////////////////////////////////////////////////////////////////////////////////////////
// Lighting formulas			// 
float4 	plight_infinity		(half m, half3 point, half3 normal, half3 light_direction)       				{
  half3 N		= normal;							// normal 
  half3 V 		= -normalize	(point);					// vector2eye
  half3 L 		= -light_direction;						// vector2light
  half3 H		= normalize	(L+V);						// half-angle-vector 
  return tex3D 		(s_material,	half3( dot(L,N), dot(H,N), m ) );		// sample material
}
float4 	plight_infinity2	(half m, half3 point, half3 normal, half3 light_direction)       				{
  	half3 N		= normal;							// normal 
  	half3 V 	= -normalize		(point);		// vector2eye
  	half3 L 	= -light_direction;					// vector2light
 	half3 H		= normalize			(L+V);			// half-angle-vector 
	half3 R     = reflect         	(-V,N);
	half 	s	= saturate(dot(L,R));
			s	= saturate(dot(H,N));
	half 	f 	= saturate(dot(-V,R));
			s  *= f;
	float4	r	= tex3D 			(s_material,	half3( dot(L,N), s, m ) );	// sample material
			r.w	= pow(saturate(s),4);
  	return	r	;
}
float4 	plight_local		(half m, half3 point, half3 normal, half3 light_position, half light_range_rsq, out float rsqr)  {
  half3 N		= normal;							// normal 
  half3 L2P 	= point-light_position;                         		// light2point 
  half3 V 		= -normalize	(point);					// vector2eye
  half3 L 		= -normalize	((half3)L2P);					// vector2light
  half3 H		= normalize	(L+V);						// half-angle-vector
		rsqr	= dot		(L2P,L2P);					// distance 2 light (squared)
  half  att 	= saturate	(1 - rsqr*light_range_rsq);			// q-linear attenuate
  float4 light	= tex3D		(s_material, half3( dot(L,N), dot(H,N), m ) ); 	// sample material
  return att*light;
}

float4	blendp	(float4	value, float4 	tcp)    		{
	#ifndef FP16_BLEND  
		value 	+= (float4)tex2Dproj 	(s_accumulator, tcp); 	// emulate blend
	#endif
	return 	value	;
}
float4	blend	(float4	value, float2 	tc)			{
	#ifndef FP16_BLEND  
		value 	+= (float4)tex2D 	(s_accumulator, tc); 	// emulate blend
	#endif
	return 	value	;
}

#endif

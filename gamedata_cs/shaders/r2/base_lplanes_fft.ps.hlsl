#include "common.hlsli"

struct 	v2p
{
 	float2 	tc0: 		TEXCOORD0;	// base
 	float2 	tc1: 		TEXCOORD1;	// lmap
  	float4	c0:		COLOR0;		// sun
};

uniform	float4		m_hud_params;	//

inline bool isCollimatorActive()
{
	return (m_hud_params.w == 1.f);
}

//////////////////////////////////////////////////////////////////////////////////////////
// Pixel
float4 	main	( v2p I )	: COLOR
{
	float4	t_base 	= tex2D	(s_base,I.tc0);

	// out
	
	if(isCollimatorActive())
	{
		return  float4	(t_base.r,t_base.g,t_base.b,t_base.a * I.c0.a);
	}
	else
	{
		return  float4	(t_base.r,t_base.g,t_base.b,t_base.a * 0.0f);
	}
}

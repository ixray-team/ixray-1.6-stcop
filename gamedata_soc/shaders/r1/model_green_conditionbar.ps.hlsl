#include "common.hlsli"

struct 	v2p
{
 	float4 	tc0: 		TEXCOORD0;	// base
 	float4 	tc1: 		TEXCOORD1;	// environment
  	float4	c0:			COLOR0;		// sun.(fog*fog)
};

//////////////////////////////////////////////////////////////////////////////////////////

float4 	main	( v2p I )	: COLOR
{
	float4	t_base 	= tex2D	(s_base,I.tc0);	
	t_base.a = (I.tc0.x < m_actor_params.z) ? 1 : 0;
	t_base.r += (0.5 < m_actor_params.z) ? 0 : 0.5;
	t_base.g -= (0.25 < m_actor_params.z) ? 0 : 0.5;

	return  float4	(t_base.r, t_base.g, t_base.b, t_base.a);	
}



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
	float4	t_base;
	t_base.r = ((m_actor_params.z<0.5) && (I.tc0.x < m_actor_params.z)) ? 1 : 0;
	t_base.g = ((m_actor_params.z>0.25) && (I.tc0.x < m_actor_params.z)) ? 1 : 0; 
	t_base.b = 0;
	t_base.a = (I.tc0.x < m_actor_params.z) ? 1 : 0;
		
	return  float4	(t_base.r, t_base.g, t_base.b, t_base.a);	
}



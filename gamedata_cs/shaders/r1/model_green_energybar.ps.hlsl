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
	t_base.a = (I.tc0.x < m_actor_params.a) ? 1 : 0;
	t_base.r += (0.1 < m_actor_params.a) ? 0 : 0.5;
	t_base.g -= (0.1 < m_actor_params.a) ? 0 : 0.5;
	
	//получаем пиксель шума и масштабируем его в соответствии с текущим уровнем проблем
	float4 t_noise = tex2D(s_lmap, I.tc0) * m_affects.x * 2; 	
	t_base.rgb += t_noise.rgb;		
		
	return  float4	(t_base.r, t_base.g, t_base.b, t_base.a);	
}



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
	float digit = m_digiclock.a;
	
	float2 coords = I.tc0;	
	coords.x = digit + (coords.x * 0.1);
	float4	t_base 	= tex2D	(s_base,coords);

	//получаем пиксель шума и масштабируем его в соответствии с текущим уровнем проблем
	float4 t_noise = tex2D(s_lmap, I.tc0) * m_affects.x * 2;
	t_base.rgb += t_noise.rgb+0.1;

	return  t_base;
}
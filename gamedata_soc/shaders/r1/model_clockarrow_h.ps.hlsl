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
	float2 coords = I.tc0;
	float2 coords_new;



	// Rotate texture
	float sin_a = m_timearrow.x;
	float cos_a = m_timearrow.y;
	coords.x = coords.x-0.5;
	coords.y = coords.y-0.5;

	coords_new.x = coords.x * cos_a + coords.y * sin_a;
	coords_new.y = -coords.x * sin_a + coords.y * cos_a;

	coords_new.x = coords_new.x+0.5;
	coords_new.y = coords_new.y+0.5;


	float4	t_base 	= tex2D	(s_base,coords_new);

	//получаем пиксель шума и масштабируем его в соответствии с текущим уровнем проблем
	float4 t_noise = tex2D(s_lmap, I.tc0) * m_affects.x * 2;
	t_base.rgb += t_noise.rgb;

	return  t_base;
}
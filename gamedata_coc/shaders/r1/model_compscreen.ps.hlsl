#include "common.hlsli"

struct 	v2p
{
 	float2 	tc0: 	TEXCOORD0;	// base
};

//////////////////////////////////////////////////////////////////////////////////////////
// Pixel
float4 	main	( v2p I )	: COLOR
{
	// получаем координату нормальной текстуры
	float4 t_base = tex2D	(s_base, I.tc0);	

	//получаем пиксель шума и масштабируем его в соответствии с текущим уровнем проблем
	float4 t_noise = tex2D(s_lmap, I.tc0) * m_affects.x * 2; 	
	t_base.rgb += t_noise.rgb+0.1;

	return	t_base;
}

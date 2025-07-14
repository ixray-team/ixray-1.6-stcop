#include "common.hlsli"

struct 	v2p
{
 	float4 	tc0: 		TEXCOORD0;	// base
 	float4 	tc1: 		TEXCOORD1;	// environment
  	float4	c0:			COLOR0;		// sun.(fog*fog)
};

//////////////////////////////////////////////////////////////////////////////////////////

// Pixel
uniform sampler2D	s_vp2;
uniform sampler2D	s_load;
uniform sampler2D	s_noise;

//float4 problems_main( v2p I )
float4 	main	( v2p I )	: COLOR
{
	//выбираем основное изображение, которое выведется на экран
	float4	t_vp2	 = ( m_affects.a * m_affects.x > 0 ) ? tex2D(s_load, I.tc0) : ((m_affects.x < 0.27) ? tex2D(s_vp2, I.tc0) : tex2D(s_base, I.tc0));

	//получаем пиксель шума и масштабируем его в соответствии с текущим уровнем проблем
	float4 t_noise = tex2D(s_noise, I.tc0) * m_affects.x * (1-m_affects.a) * 2;
	//t_vp2.rgb += t_noise.rgb;

	t_vp2.rgb = (m_affects.x > 0.41) ? 0 : t_vp2.rgb + t_noise.rgb;

	return  float4	(t_vp2.r, t_vp2.g, t_vp2.b, 1);
}
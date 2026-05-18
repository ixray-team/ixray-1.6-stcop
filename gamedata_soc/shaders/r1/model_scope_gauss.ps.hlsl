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
uniform sampler2D	s_noise;

float4 	main	( v2p I )	: COLOR
{
	float4	t_base 	= tex2D	(s_base,I.tc0);


	I.tc0.x = (I.tc0.x-0.5f)*m_hud_params.x+0.5f+m_zoom_deviation.x;	// Растягиваем картинку в линзе так, чтобы на любом разрешении экрана были правильные пропорции
	I.tc0.y	= I.tc0.y+m_zoom_deviation.y;
	float4	t_vp2	 = tex2D (s_vp2, I.tc0);			// Изображение со второго вьюпорта

	t_vp2.b = t_vp2.b + (t_vp2.r + t_vp2.g + t_vp2.b+1)*m_zoom_deviation.z;

	//получаем пиксель шума и масштабируем его в соответствии с текущим уровнем проблем
	float4 t_noise = tex2D(s_noise, I.tc0) * m_affects.x * 2;
	t_vp2.rgb += t_noise.rgb;

	// Миксуем с сеткой
	float4 res = lerp	(t_vp2, t_base, t_base.a);

	// out
	return  float4	(res.r, res.g, res.b, min(m_hud_params.y, m_hud_params.a));
}

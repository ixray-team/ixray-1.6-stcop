#include "common.hlsli"
uniform    float4        screen_res;
#include "pnv.hlsli"

struct 	v2p
{
 	float4 	tc0: 		TEXCOORD0;	// base
 	float4 	tc1: 		TEXCOORD1;	// environment
  	float4	c0:			COLOR0;		// sun.(fog*fog)
};

//////////////////////////////////////////////////////////////////////////////////////////

// Pixel
uniform sampler2D	s_vp2;

float4 	main	( v2p I )	: COLOR
{

	float4	t_base 	= tex2D	(s_base,I.tc0);

	I.tc0.x = (I.tc0.x-0.5f)*m_hud_params.x+0.5f+m_zoom_deviation.x;	// Растягиваем картинку в линзе так, чтобы на любом разрешении экрана были правильные пропорции
	I.tc0.y	= I.tc0.y+m_zoom_deviation.y;
	float4	t_vp2	 = tex2D (s_vp2, I.tc0);			// Изображение со второго вьюпорта

	float4	final	 = float4 (0, 0, 0, 0);

	//** Ночной режим **// //Зеленый ПНВ
	t_vp2.rgb = calc_night_vision_effect(I.tc0, t_vp2, float3(1.0, 2.0, 1.0));

	// Миксуем с сеткой
	float4 res = lerp	(t_vp2, t_base, t_base.a);

	// out
	return  float4	(res.r, res.g, res.b, min(m_hud_params.y, m_hud_params.a));
}

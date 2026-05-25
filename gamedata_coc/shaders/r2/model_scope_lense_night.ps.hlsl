#include "common.hlsli"
#include "pnv.hlsli"


struct 	v2p
{
 	float2 	tc0: 		TEXCOORD0;	// base
 	float3 	tc1: 		TEXCOORD1;	// environment
  	float4	c0:			COLOR0;		// sun.(fog*fog)
};

//////////////////////////////////////////////////////////////////////////////////////////

float  resize(float input, float factor, float offset)
{
	return (input-0.5f+offset) / factor+0.5f-offset;
}

// Pixel
uniform	float4 		m_hud_params;
uniform sampler2D	s_vp2;

float4 	main	( v2p I )	: COLOR
{

	float4	t_base 		= tex2D		(s_base,	I.tc0);		// Текстура сетки

	//сдвиг от состояния оружия
	I.tc0.x	= I.tc0.x+m_zoom_deviation.x;
	I.tc0.y	= I.tc0.y+m_zoom_deviation.y;

	// Растягиваем картинку в линзе так, чтобы на любом разрешении экрана были правильные пропорции
	I.tc0.x = resize(I.tc0.x, screen_res.x/screen_res.y, 0);

	float4	t_vp2	 = tex2D		(s_vp2, I.tc0);			// Изображение со второго вьюпорта
	float3	final	 = float3(0, 0, 0);

	//** Ночной режим **// //Зеленый ПНВ
	t_vp2.rgb = calc_night_vision_effect(I.tc0, t_vp2, float3(1.0, 2.0, 1.0));
	// Миксуем с сеткой
	final	= lerp	(t_vp2, t_base, t_base.a);

	// out
	return  float4	(final.r, final.g, final.b, min(m_hud_params.y, m_hud_params.a));
}

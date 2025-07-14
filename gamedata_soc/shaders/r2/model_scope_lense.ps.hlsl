#include "common.hlsli"

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
uniform	float4		screen_res;
uniform	float4 		m_hud_params;
uniform	float4 		m_zoom_deviation;

uniform sampler2D	s_vp2;
//uniform sampler2D	s_skymap;

//uniform samplerCUBE	s_env0;
//uniform samplerCUBE	s_env1;

float random(float2 co)
{
      return 0.5+(frac(sin(dot(co.xy ,float2(12.9898,78.233))) * 43758.5453))*0.5;
};

float4 	main	( v2p I )	: COLOR
{

	float4	t_base 		= tex2D		(s_base,	I.tc0);		// Текстура сетки
 //	float4	t_skymap 	= tex2D		(s_skymap,	I.tc0);		// Карта отражения неба

	//сдвиг от состояния оружия
	I.tc0.x	= I.tc0.x+m_zoom_deviation.x;
	I.tc0.y	= I.tc0.y+m_zoom_deviation.y; 
	
	// Растягиваем картинку в линзе так, чтобы на любом разрешении экрана были правильные пропорции
	I.tc0.x = resize(I.tc0.x, screen_res.x/screen_res.y, 0);	
	float4	t_vp2	 = tex2D		(s_vp2, I.tc0);			// Изображение со второго вьюпорта
	
	float2 offset = distance(I.tc0, float2( .5f, .5f ))*float2(m_hud_params.z, m_hud_params.z);
	
	t_vp2.r = tex2D	( s_vp2, I.tc0+offset).r;
	t_vp2.b = tex2D	( s_vp2, I.tc0-offset).b;	
	
	float3	final	 = float3(0, 0, 0);
	
	{	//Стандартный режим
	
		// Текущая и следующая текстура неба
		//float3	env0		= texCUBE	(s_env0, I.tc1);
		//float3	env1		= texCUBE	(s_env1, I.tc1);
		//float3	env			= lerp		(env0, env1, L_ambient.w);	// Их миксовка
		
		// Миксуем всё и собираем финальную картинку
		float3 	base	= lerp	(t_vp2, t_base, t_base.a);		// Сетку с вьюпортом
				final	= base;
		//		final	= lerp	(base,  env,    t_skymap.a);	// base c небом
	}
	
	// out
	return  float4	(final.r, final.g, final.b, min(m_hud_params.y, m_hud_params.a));
}

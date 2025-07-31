#include "common.hlsli"

struct 	v2p
{
 	float2 	tc0: 		TEXCOORD0;	// base
 	float3 	tc1: 		TEXCOORD1;	// environment
  	float4	c0:			COLOR0;		// sun.(fog*fog)
};

//////////////////////////////////////////////////////////////////////////////////////////

// Pixel
uniform	float4		screen_res;
uniform	float4 		m_hud_params;
uniform	float4 		m_zoom_deviation;
uniform	float4 		m_affects;

float  resize(float input, float factor, float offset)
{
	return (input-0.5f+offset) / factor+0.5f-offset;
}

float get_noise(float2 co)
{
	return (frac(sin(dot(co.xy ,float2(12.9898,78.233))) * 43758.5453))*0.5;
};

uniform sampler2D	s_vp2;

float random(float2 co)
{
      return 0.5+(frac(sin(dot(co.xy ,float2(12.9898,78.233))) * 43758.5453))*0.5;
};

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
	
	//Абберация
	float2 offset = distance(I.tc0, float2( .5f, .5f ))*float2(m_hud_params.z, m_hud_params.z);	
	t_vp2.r = tex2D	( s_vp2, I.tc0+offset).r;
	t_vp2.b = tex2D	( s_vp2, I.tc0-offset).b;		
	
	//Ночной режим
		
	// Рассчитываем случайный шум пикселя
	float noise		= random(I.tc0*timers.z) * m_zoom_deviation.w;

	float gray = ((t_vp2.r + t_vp2.g + t_vp2.b))*m_zoom_deviation.z*7;	
	t_vp2.r = t_vp2.r*(1-m_zoom_deviation.z) + gray;
	t_vp2.g = t_vp2.g*(1-m_zoom_deviation.z) + gray;	
	t_vp2.b = t_vp2.b*(1-m_zoom_deviation.z) + gray;		
	
	t_vp2.b+= (0.4 + noise)*m_zoom_deviation.z;

	// Шум при выбросе
	float noise_blow	= get_noise(I.tc0*timers.z) * m_affects.x * m_affects.x * 30;		
	t_vp2.r += noise_blow;
	t_vp2.g += noise_blow;
	t_vp2.b += noise_blow;	
	
	// Миксуем с сеткой
	final	= lerp	(t_vp2, t_base, t_base.a);

	
	// out
	return  float4	(final.r, final.g, final.b, min(m_hud_params.y, m_hud_params.a));
}

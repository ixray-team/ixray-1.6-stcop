// #include "common.hlsli"

// uniform	float4 		m_hud_params;
// uniform	float4 		m_zoom_deviation;
// uniform	float4 		m_affects;

// struct 	v2p
// {
 	// float2 	tc0: 		TEXCOORD0;	// base
 	// float3 	tc1: 		TEXCOORD1;	// environment
  	// float4	c0:			COLOR0;		// sun.(fog*fog)
// };

// float get_noise(float2 co)
// {
	// return (frac(sin(dot(co.xy ,float2(12.9898,78.233))) * 43758.5453))*0.5;
// };

// //////////////////////////////////////////////////////////////////////////////////////////

// float  resize(float input, float factor, float offset)
// {
	// return (input-0.5f+offset) / factor+0.5f-offset;
// }

// // Pixel
// uniform	float4		screen_res;

// Texture2D 	s_vp2;
// //Texture2D s_skymap;

// //uniform samplerCUBE	s_env0;
// //uniform samplerCUBE	s_env1;

// float random(float2 co)
// {
      // return 0.5+(frac(sin(dot(co.xy ,float2(12.9898,78.233))) * 43758.5453))*0.5;
// };

// float4 main( v2p I ) : SV_Target
// {
	// if (m_hud_params.y < 0.0001) return float4 (0,0,0,0);
	
// //	float2 	distort	= s_distort.Sample( smp_rtlinear, I.tc);
// //	float3	image 	= s_base.Sample( smp_rtlinear, I.tc + offset);
	
	// float4	t_base 		= s_base.Sample		( smp_base, I.tc0);		// Текстура сетки	
 // //	float4	t_skymap 	= s_skymap.Sample	( smp_base, I.tc0); 	// Карта отражения неба
	
	// //сдвиг от состояния оружия
	// I.tc0.x	= I.tc0.x+m_zoom_deviation.x;
	// I.tc0.y	= I.tc0.y+m_zoom_deviation.y;	
	
	// // Растягиваем картинку в линзе так, чтобы на любом разрешении экрана были правильные пропорции
	// I.tc0.x = resize(I.tc0.x, screen_res.x/screen_res.y, 0);
	
	// float4	t_vp2	 = s_vp2.Sample	( smp_base, I.tc0);  			// Изображение со второго вьюпорта

	// //абберация
	// float2 offset = distance(I.tc0, float2( .5f, .5f ))*float2(m_hud_params.z, m_hud_params.z);
	
	// t_vp2.r = s_vp2.Sample	( smp_base, I.tc0+offset).r;
	// t_vp2.b = s_vp2.Sample	( smp_base, I.tc0-offset).b;
	
	// float3	final	 = float3(0, 0, 0);
	
	// // Рассчитываем случайный шум пикселя
	// float noise		= random(I.tc0*timers.z) * m_zoom_deviation.w;
	
	// float gray = ((t_vp2.r + t_vp2.g + t_vp2.b))*m_zoom_deviation.z*7;	
	// t_vp2.r = t_vp2.r*(1-m_zoom_deviation.z) + gray;
	// t_vp2.g = t_vp2.g*(1-m_zoom_deviation.z) + gray;	
	// t_vp2.b = t_vp2.b*(1-m_zoom_deviation.z) + gray;		
	
	// t_vp2.b+= (0.4 + noise)*m_zoom_deviation.z;
		
	// // Шум при выбросе
	// float blow_noise	= get_noise(I.tc0*timers.z) * m_affects.x * m_affects.x * 30;	
	// t_vp2.r += blow_noise;
	// t_vp2.g += blow_noise;
	// t_vp2.b += blow_noise;		
	// t_base.xyz = detonemap(t_base.xyz);
		
	// // Миксуем с сеткой
	// final	= lerp	(t_vp2, t_base, t_base.a);		
	
	// // out
	// return  float4	(final.r, final.g, final.b, min(m_hud_params.y, m_hud_params.a));
// }

#include "common.hlsli"

uniform float4 m_hud_params;

struct v2p
{
    float2 tc0: TEXCOORD0;
    float3 tc1: TEXCOORD1;
    float4 c0: COLOR0;
};

float4 main(v2p I, float4 pos2d : SV_POSITION) : SV_Target
{
    if (m_hud_params.y * m_hud_params.a < 0.0001f) {
		return 0.0f;
	}
	
    float4 t_base = s_base.Sample(smp_base, I.tc0);
	t_base.xyz = detonemap(t_base.xyz);
	
    float4 t_vp2 = s_image[pos2d.xy];
	
	float alpha = m_hud_params.y * m_hud_params.a;
	t_vp2 *= saturate(alpha * 2.0f - 1.0f);

    float3 final = lerp(t_vp2.xyz, t_base.xyz, t_base.a);
    return float4(final.xyz, saturate(alpha * 2.0f));
}


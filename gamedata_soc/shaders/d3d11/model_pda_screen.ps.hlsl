#include "common.hlsli"

uniform	float4 		m_affects;

struct 	v2p
{
 	float2 	tc0: 		TEXCOORD0;	// base
 	float3 	tc1: 		TEXCOORD1;	// environment
  	float4	c0:			COLOR0;		// sun.(fog*fog)
};

float get_noise(float2 co)
{
      return (frac(sin(dot(co.xy ,float2(12.9898,78.233))) * 43758.5453))*0.5;
};

Texture2D 	s_vp2;
Texture2D 	s_load;
float4 problems_main(p_bumped_new I)
{
    // Получаем базовые UV-координаты из tcdh.xy
    float2 base_uv = I.tcdh.xy;
    
    // Узкая полоска искажений
    float problems = frac(timers.z * 5 * (1 + 2 * m_affects.x));    
    base_uv.x += (m_affects.x > 0.09 && base_uv.y > problems - 0.01 && base_uv.y < problems) ? 
                 sin((base_uv.y - problems) * 5 * m_affects.y) : 0;

    // Широкая полоска искажений    
    problems = cos((frac(timers.z * 2) - 0.5) * 3.1416) * 2 - 0.8;
    float AMPL = 0.13;
    base_uv.x -= (m_affects.x > 0.15 && base_uv.y > problems - AMPL && base_uv.y < problems + AMPL) ? 
                 cos(4.71 * (base_uv.y - problems) / AMPL) * sin(frac(timers.z) * 6.2831 * 90) * 0.02 * 
                 (AMPL - abs(base_uv.y - problems)) / AMPL : 0;        
    
    // Тряска влево-вправо в финальной стадии
    base_uv.x += (m_affects.x > 0.38) ? (m_affects.y - 0.5) * 0.04 : 0;    
    
    // Выбор текстуры в зависимости от состояния эффекта
    float4 t_vp2 = (m_affects.x < 0.27) ? 
                   s_vp2.Sample(smp_rtlinear, base_uv) : 
                   s_base.Sample(smp_base, base_uv);  
    
    // Шум при выбросе
    float noise = get_noise(base_uv * timers.z) * m_affects.x * m_affects.x * 20;        
    t_vp2.r += noise;
    t_vp2.g += noise;
    t_vp2.b += noise;

    // Отключение экрана
    t_vp2.rgb = (m_affects.x > 0.41) ? 0 : t_vp2.rgb;

    return t_vp2;
}

float4 loading_main( p_bumped_new I )
{
    float4 t_load = s_load.Sample ( smp_base, I.tcdh.xy);
	return t_load;
}

float4 main( p_bumped_new I ) : SV_Target
{
    float4 final = 1.0f;

    [branch]
    if (m_affects.a > 0 && m_affects.x >= 0.08)
    {
        final.xyz = loading_main(I).xyz;
    }
    else
    {
        final.xyz = problems_main(I).xyz;
    }

    final.xyz = detonemap(final.xyz * 0.8f);
    
    return final;
}

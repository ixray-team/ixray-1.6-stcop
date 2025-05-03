#include "common.hlsli"

struct 	v2p
{
	float2 	tc0: TEXCOORD0;	// base
	float4	c0: COLOR0; // sun
};

uniform	float4 m_affects;

float get_noise(float2 co)
{
	return (frac(sin(dot(co.xy ,float2(12.9898,78.233))) * 43758.5453))*0.5;
};

//////////////////////////////////////////////////////////////////////////////////////////
// Pixel
float4 main( v2p I ) : SV_Target
{
	float4	t_base 	= s_base.Sample( smp_base, I.tc0 );

	// Шум при выбросе
	float noise	= get_noise(I.tc0*timers.z) * m_affects.x * 2;		
	t_base.r += noise;
	t_base.g += noise;
	t_base.b += noise;	
	
	t_base.xyz = detonemap(t_base.xyz);
	return float4(t_base.xyz,t_base.w * I.c0.w);
}


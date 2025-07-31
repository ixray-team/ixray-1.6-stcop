#include "common.hlsli"

struct 	v2p
{
 	float2 	tc0: 		TEXCOORD0;	// base
 	float3 	tc1: 		TEXCOORD1;
  	float4	c0:			COLOR0;		// sun.(fog*fog)
};

float get_noise(float2 co)
{
	return (frac(sin(dot(co.xy ,float2(12.9898,78.233))) * 43758.5453))*0.5;
};

uniform float4	m_affects;
uniform	float4 	m_timearrow2;

float4 main( v2p I ) : SV_Target
{
	float2 coords = I.tc0;
	float2 coords_new;

	// Rotate texture
	float sin_a = m_timearrow2.x;
	float cos_a = m_timearrow2.y;
	coords.x = coords.x-0.5;
	coords.y = coords.y-0.5;

	coords_new.x = coords.x * cos_a + coords.y * sin_a;
	coords_new.y = -coords.x * sin_a + coords.y * cos_a;

	coords_new.x = coords_new.x+0.5;
	coords_new.y = coords_new.y+0.5;


	float4	t_base 	= s_base.Sample( smp_base, coords_new );
	float4	t_noise = s_lmap.Sample( smp_base, coords_new );

	float noise	= get_noise(I.tc0*timers.z) * m_affects.x * m_affects.x * 30;

	t_base.rgb += t_noise.rgb*noise+0.1;
	t_base.xyz = detonemap(t_base.xyz);

	return  t_base;
}

#include "common.hlsli"

struct 	v2p
{
	float2 	tc0: TEXCOORD0;	// base
	float4	c0: COLOR0; // sun
};

uniform	float4 m_affects;

//////////////////////////////////////////////////////////////////////////////////////////
// Pixel
float4 main( v2p I ) : SV_Target
{
	float4	t_base 	= s_base.Sample( smp_base, I.tc0 );

	// ��� ��� �������
	float noise	= get_noise(I.tc0*timers.z) * m_affects.x * 2;		
	t_base.r += noise;
	t_base.g += noise;
	t_base.b += noise;	
	
	t_base.xyz = detonemap(t_base.xyz);
	
	// #TODO: plohoi fix, no aref model sloman
	return float4(t_base.xyz*4.0f,t_base.w * I.c0.w);
}


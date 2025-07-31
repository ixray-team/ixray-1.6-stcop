#include "common.hlsli"

struct 	v2p
{
	float2 tc0: TEXCOORD0;	// base
// 	float2 tc1: TEXCOORD1;	// lmap
	float4 c0: COLOR0;		// sun
};

uniform	float4 m_hud_params;

inline bool isCollimatorActive()
{
	return (m_hud_params.w == 1.f);
}

//////////////////////////////////////////////////////////////////////////////////////////
// Pixel
float4 main( p_bumped_new I ) : SV_Target
{
	float4 color = s_base.Sample(smp_base, I.tcdh.xy);

	if (isCollimatorActive())
	{
		return float4(color.xyz * color.w, 0.0f);
	}
	else
	{
		return float4(0.0, 0.0, 0.0, 0.0);
	}
}

#include "common.hlsli"
uniform float4 m_hud_params;

float main(in PSInputFullscreen I) : SV_Depth
{
	float Depth = s_position.Load(int3(I.hpos.xy, 0)).x;
	float Alpha = m_hud_params.y * m_hud_params.w;
	float Point = I.hpos.z * 0.02f;
	
#ifdef USE_LENSE_LERP
	I.texcoord.x *= 0.5f;
#endif
	
    float4 t_base = s_base.Sample(smp_base, I.texcoord.xy);
	
	if(Depth > Point && Alpha >= 0.5f && t_base.w < 1.0f) 
	{
		return 1.0f;
	}
	
	return Depth;
}
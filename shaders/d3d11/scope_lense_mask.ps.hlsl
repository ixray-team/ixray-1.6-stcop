#include "common.hlsli"

struct ScopeMaskOutput
{
	float Depth : SV_Depth;
	float DofMask : SV_Target0;
};

ScopeMaskOutput main(in PSInputFullscreen I)
{
	float Depth = s_position.Load(int3(I.hpos.xy, 0)).x;
	float Alpha = m_hud_params.y * m_hud_params.w;
	float Point = I.hpos.z * 0.02f;
	
#ifdef USE_LENSE_LERP
	I.texcoord.x *= 0.5f;
#endif
	
    float4 t_base = s_base.Sample(smp_base, I.texcoord.xy);
	
	ScopeMaskOutput Result;
	Result.Depth = Depth;
	Result.DofMask = 0.0f;
	if(Depth > Point && Alpha >= 0.5f && t_base.w < 1.0f)
	{
		Result.Depth = 1.0f;
		Result.DofMask = 1.0f;
	}
	
	return Result;
}

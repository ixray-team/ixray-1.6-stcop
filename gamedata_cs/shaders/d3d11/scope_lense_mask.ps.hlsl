#include "common.hlsli"
uniform float4 m_hud_params;

float main(float Z : TEXCOORD0, float4 pos2d : SV_POSITION) : SV_Depth
{
	float Depth = s_position.Load(int3(pos2d.xy, 0)).x;
	float Alpha = m_hud_params.y * m_hud_params.w;
	
	if(Depth > pos2d.z * 0.02f && Alpha >= 0.5f) {
		return 1.0f;
	}
	
	return Depth;
}
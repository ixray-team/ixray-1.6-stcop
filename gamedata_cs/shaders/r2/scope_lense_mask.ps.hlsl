#include "common.hlsli"

uniform float4 m_hud_params;

float4 main(float2 tc0 : TEXCOORD0) : COLOR
{
	float Alpha = m_hud_params.y * m_hud_params.w;
#ifdef USE_LENSE_LERP
	tc0.x *= 0.5f;
#endif
	float4 Base = tex2D(s_base, tc0);

	clip((Alpha >= 0.5f && Base.a < 1.0f) ? 1.0f : -1.0f);
	return 0.0f;
}

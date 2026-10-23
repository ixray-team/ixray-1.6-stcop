#ifndef SCOPE_LENS_HLSLI
#define SCOPE_LENS_HLSLI

uniform sampler2D s_vp2;

float2 GetScopeSceneUV(float4 ScreenPosition)
{
	return ScreenPosition.xy / ScreenPosition.w;
}

float4 SampleScopeScene(float2 LensUV, float2 SceneUV)
{
	float4 Color = tex2D(s_vp2, SceneUV);
	if (m_hud_params.z > 0.0f)
	{
		// Gunslinger separates red and blue diagonally, increasingly towards the lens edge.
		float2 Offset = distance(LensUV, float2(0.5f, 0.5f)) * m_hud_params.zz;
		Color.r = tex2D(s_vp2, SceneUV + Offset).r;
		Color.b = tex2D(s_vp2, SceneUV - Offset).b;
	}
	return Color;
}

float GetScopeLensAlpha()
{
	return min(m_hud_params.y, m_hud_params.a);
}

#endif

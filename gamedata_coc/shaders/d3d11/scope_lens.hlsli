#ifndef SCOPE_LENS_HLSLI
#define SCOPE_LENS_HLSLI

float4 SampleScopeScene(float2 LensUV, float2 ScreenPosition)
{
	float4 Color = s_image[ScreenPosition];
	[branch]
	if (m_hud_params.z > 0.0f)
	{
		// Gunslinger separates red and blue diagonally, increasingly towards the lens edge.
		float2 Offset = distance(LensUV, float2(0.5f, 0.5f)) * m_hud_params.zz;
		uint Width, Height;
		s_image.GetDimensions(Width, Height);
		float2 TexelSize = rcp(float2(Width, Height));
		float2 SceneUV = ScreenPosition * TexelSize;
		float2 HalfTexel = TexelSize * 0.5f;
		Color.r = s_image.SampleLevel(smp_rtlinear, clamp(SceneUV + Offset, HalfTexel, 1.0f - HalfTexel), 0).r;
		Color.b = s_image.SampleLevel(smp_rtlinear, clamp(SceneUV - Offset, HalfTexel, 1.0f - HalfTexel), 0).b;
	}
	return Color;
}

#endif

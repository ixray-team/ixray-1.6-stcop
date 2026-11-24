#ifndef SCOPE_POSTPROCESS_HLSLI
#define SCOPE_POSTPROCESS_HLSLI

#ifdef SCOPE_POSTPROCESS
Texture2D s_scope_tonemap : register(t25);
#include "common\post_cb.hlsli"

// combine_2's exposure, for the scene's own values
float GetScopeExposure()
{
	return SceneExposure(s_scope_tonemap.Load(uint3(0, 0, 0)).x, tonemap_params.w, bloom_params.w);
}
#endif

float3 PreparePostprocessColor(float3 Color)
{
#ifdef SCOPE_POSTPROCESS
	Color = ToneToDisplay(tonemap(max(Color, 0.0f), GetScopeExposure()));
#endif
	return saturate(Color);
}

float3 RestorePostprocessColor(float3 Color)
{
#ifdef SCOPE_POSTPROCESS
	// The lens is composed before the scene's exposure and tone mapping.
	Color = detonemap(DisplayToTone(saturate(Color))) / GetScopeExposure();
#endif
	return Color;
}

#endif

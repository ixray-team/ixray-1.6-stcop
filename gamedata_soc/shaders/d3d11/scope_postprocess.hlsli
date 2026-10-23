#ifndef SCOPE_POSTPROCESS_HLSLI
#define SCOPE_POSTPROCESS_HLSLI

#ifdef SCOPE_POSTPROCESS
Texture2D s_scope_tonemap;
float4 bloom_params; // x - ps_r2_bloom_amount

// Matches combine_2 without the bloom term: the legacy pipeline squares the color only to mix bloom.
float GetScopeExposure()
{
	float Exposure = max(s_scope_tonemap.Load(uint3(0, 0, 0)).x, EPS_S);
#ifdef USE_LEGACY_LIGHT
	Exposure *= rsqrt(bloom_params.x + 1.0f);
#else
	Exposure *= rcp(bloom_params.x + 1.0f);
#endif
	return Exposure;
}
#endif

float3 PreparePostprocessColor(float3 Color)
{
#ifdef SCOPE_POSTPROCESS
	Color = tonemap(max(Color, 0.0f), GetScopeExposure());
#endif
	return saturate(Color);
}

float3 RestorePostprocessColor(float3 Color)
{
#ifdef SCOPE_POSTPROCESS
	// The lens is composed before the scene's exposure and tone mapping.
	Color = detonemap(saturate(Color)) / GetScopeExposure();
#endif
	return Color;
}

#endif

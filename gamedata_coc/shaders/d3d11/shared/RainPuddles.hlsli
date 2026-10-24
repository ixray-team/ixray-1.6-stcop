#ifndef RAIN_PUDDLES_HLSLI
#define RAIN_PUDDLES_HLSLI

// Ripple animation adapted from Screen Space Shaders by Ascii1457,
// based on Sebastien Lagarde's dynamic rain technique.
// https://www.moddb.com/mods/stalker-anomaly/addons/screen-space-shaders
// https://seblagarde.wordpress.com/2013/01/03/water-drop-2b-dynamic-rain-and-its-effects/

Texture2D s_puddles_perlin;
Texture2D s_puddles_normal;
Texture2D s_puddles_ripples;
Texture2D<float2> s_puddles_water_height;
float4 RainPuddleWaterBounds;
float4 RainPuddleWaterHeightRange;
float4 RainPuddleControl;

#ifdef RAIN_PUDDLE_COMPUTE
#define RAIN_PUDDLE_SAMPLE(Texture, Sampler, TexCoord) Texture.SampleLevel(Sampler, TexCoord, 0.0f)
#else
#define RAIN_PUDDLE_SAMPLE(Texture, Sampler, TexCoord) Texture.Sample(Sampler, TexCoord)
#endif

float2 ProcessPuddleRipple(float4 Ripple, float Speed)
{
	float2 Normal = Ripple.yz * 2.0f - 1.0f;
	float Time = frac(Ripple.w + timers.x * Speed);
	float Frequency = clamp((Time - 1.0f + Ripple.x) * 10.0f, 0.0f, 4.0f);
	float Fade = saturate(0.7f - Time) * Ripple.x * sin(Frequency * PI);
	Fade *= 1.0f - smoothstep(0.0f, 4.0f, Frequency);
	return Normal * Fade;
}

float2 RainPuddleRipples(float2 TexCoord, float Depth)
{
	float DistanceFade = saturate((15.0f - Depth) / 15.0f);
	[branch]
	if (DistanceFade <= 0.0f || rain_params.x <= 0.0f)
	{
		return 0.0f;
	}

	float4 Layer0 = RAIN_PUDDLE_SAMPLE(s_puddles_ripples, smp_linear, TexCoord);
	float4 Layer1 = RAIN_PUDDLE_SAMPLE(s_puddles_ripples, smp_linear, TexCoord * 0.61f + float2(0.5f, 0.25f));
	float4 Layer2 = RAIN_PUDDLE_SAMPLE(s_puddles_ripples, smp_linear, TexCoord * 0.87f + float2(0.31f, 0.5f));
	float2 Ripples = ProcessPuddleRipple(Layer0, 1.05f * 0.85f);
	Ripples += ProcessPuddleRipple(Layer1, 1.31f * 0.85f);
	Ripples += ProcessPuddleRipple(Layer2, 1.58f * 0.85f);
	return clamp(Ripples * DistanceFade, -1.0f, 1.0f) * rain_params.x;
}

float GetRainPuddleWaterVisibility(float3 ViewPoint)
{
	// Fade both the liquid footprint and the vertical approach to the water surface.
	[branch]
	if (RainPuddleWaterBounds.z > 0.0f && RainPuddleWaterBounds.w > 0.0f)
	{
		float3 WorldPoint = mul(m_invV, float4(ViewPoint, 1.0f)).xyz;
		float2 WaterTexCoord = (WorldPoint.xz - RainPuddleWaterBounds.xy) * RainPuddleWaterBounds.zw;
		[branch]
		if (all(WaterTexCoord >= 0.0f) && all(WaterTexCoord < 1.0f))
		{
			float2 Water = s_puddles_water_height.SampleLevel(smp_linear, WaterTexCoord, 0.0f);
			[branch]
			if (Water.y > EPS)
			{
				// Empty cells have zero height and coverage; divide out filtered coverage.
				float WaterHeight = RainPuddleWaterHeightRange.x + Water.x / Water.y * RainPuddleWaterHeightRange.y;
				float HeightFade = smoothstep(0.02f, 0.25f, WorldPoint.y - WaterHeight);
				return lerp(1.0f, HeightFade, Water.y);
			}
		}
	}
	return 1.0f;
}

float GetRainPuddle(float2 TexCoord, float3 FlatNormal, float3 ViewPoint, 
#ifdef RAIN_PUDDLE_COMPUTE
	float TerrainHeight,
	bool HasTerrainHeight,
#endif
out float3 WaterNormal, out float2 Refraction)
{
	WaterNormal = mul((float3x3)m_V, float3(0.0f, 1.0f, 0.0f));
	Refraction = 0.0f;
	float Wetness = saturate(rain_params.y);
	[branch]
	if (Wetness <= 0.0f || RainPuddleControl.x <= 0.0f)
	{
		return 0.0f;
	}

	float WaterVisibility = GetRainPuddleWaterVisibility(ViewPoint);
	if (WaterVisibility <= 0.0f)
	{
		return 0.0f;
	}

	float3 WorldNormal = mul((float3x3)m_invV, normalize(FlatNormal));
	/*
	#ifdef RAIN_PUDDLE_COMPUTE
		// Fade shallow terrain slopes after contour smoothing to avoid triangle-shaped cuts.
		float Tilt = max(abs(WorldNormal.x), abs(WorldNormal.z));
		float Slope = (1.0f - smoothstep(0.05f, 0.2f, Tilt)) * step(0.0f, WorldNormal.y);
	#else
		// Terrain Upgrade's slope test, restricted to upward-facing surfaces.
		float Slope = saturate((0.1f - max(abs(WorldNormal.x), abs(WorldNormal.z))) * 13.0f) * step(0.0f, WorldNormal.y);
	#endif
		float Noise = RAIN_PUDDLE_SAMPLE(s_puddles_perlin, smp_base, TexCoord * 15.0f).r;
		float Amount = saturate((Noise - 0.12f) * Wetness);
	#ifndef RAIN_PUDDLE_COMPUTE
		Amount *= Slope;
	#endif
		// Keep the original soft edge transition (border hardness 0.15).
		Amount = smoothstep(0.0f, 0.255f, Amount);
	#ifdef RAIN_PUDDLE_COMPUTE
		Amount *= Slope;
	#endif
	*/
	#ifdef RAIN_PUDDLE_COMPUTE
		if (!HasTerrainHeight)
		{
			return 0.0f;
		}

		float MaxFillHeight = 0.6f;
		float HeightEdgeWidth = 2.0f / 127.0f;

		float WaterLevel = Wetness * MaxFillHeight;
		float FillDepth = WaterLevel - TerrainHeight;
		float Amount = smoothstep(0.0f, HeightEdgeWidth, FillDepth);
		float Tilt = max(abs(WorldNormal.x), abs(WorldNormal.z));
		float Slope = (1.0f - smoothstep(0.05f, 0.2f, Tilt)) * step(0.0f, WorldNormal.y);
		Amount *= Slope;
	#else
		float Noise = RAIN_PUDDLE_SAMPLE(s_puddles_perlin, smp_base, TexCoord * 15.0f).r;
		// Terrain Upgrade's slope test, restricted to upward-facing surfaces.
		float Slope = saturate((0.1f - max(abs(WorldNormal.x), abs(WorldNormal.z))) * 13.0f) * step(0.0f, WorldNormal.y);
		float Amount = saturate((Noise - 0.12f) * Wetness) * Slope;
		Amount = smoothstep(0.0f, 0.255f, Amount);
	#endif
	Amount *= WaterVisibility;
	[branch]
	if (Amount <= 0.0f)
	{
		return 0.0f;
	}

	float RainIntensity = saturate(rain_params.x * 1.5f);
	float Animation = timers.x * (0.01f + RainIntensity * 0.008f);
	float2 RippleTexCoord = TexCoord * 140.0f;
	float3 Normal0 = RAIN_PUDDLE_SAMPLE(s_puddles_normal, smp_base, RippleTexCoord + float2(0.0f, Animation)).xyz;
	float3 Normal1 = RAIN_PUDDLE_SAMPLE(s_puddles_normal, smp_base, RippleTexCoord - float2(0.33f, Animation)).xyz;
	float3 Ripples = normalize(float3(Normal0.xy + Normal1.xy, Normal0.z * Normal1.z)) * 2.0f - 1.0f;
	Ripples *= saturate(0.05f + RainIntensity * 0.1f);
	Ripples = Ripples * 0.666f + float3(RainPuddleRipples(RippleTexCoord * 1.3f, ViewPoint.z) * 0.333f, 0.0f);

#ifdef RAIN_PUDDLE_COMPUTE
	// Follow the receiving surface so grazing reflection rays do not hit the ground again.
	WorldNormal = normalize(WorldNormal);
	float3 Tangent = normalize(float3(WorldNormal.y, -WorldNormal.x, 0.0f));
	float3 Bitangent = cross(Tangent, WorldNormal);
	float3 RippleNormal = normalize(WorldNormal + Tangent * Ripples.x + Bitangent * Ripples.y);
	WaterNormal = mul((float3x3)m_V, RippleNormal);
#else
	WaterNormal = mul((float3x3)m_V, float3(Ripples.x, 1.0f, Ripples.y));
#endif
	Refraction = Ripples.xy * 0.15f * smoothstep(0.6f, 0.8f, Amount);
	return Amount;
}

#undef RAIN_PUDDLE_SAMPLE

#endif

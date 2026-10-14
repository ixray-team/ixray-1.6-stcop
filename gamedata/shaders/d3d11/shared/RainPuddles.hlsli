#ifndef RAIN_PUDDLES_HLSLI
#define RAIN_PUDDLES_HLSLI

// Ripple animation adapted from Screen Space Shaders by Ascii1457,
// based on Sebastien Lagarde's dynamic rain technique.
// https://www.moddb.com/mods/stalker-anomaly/addons/screen-space-shaders
// https://seblagarde.wordpress.com/2013/01/03/water-drop-2b-dynamic-rain-and-its-effects/

Texture2D s_puddles_perlin;
Texture2D s_puddles_normal;
Texture2D s_puddles_ripples;

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

	float4 Layer0 = s_puddles_ripples.Sample(smp_linear, TexCoord);
	float4 Layer1 = s_puddles_ripples.Sample(smp_linear, TexCoord * 0.61f + float2(0.5f, 0.25f));
	float4 Layer2 = s_puddles_ripples.Sample(smp_linear, TexCoord * 0.87f + float2(0.31f, 0.5f));
	float2 Ripples = ProcessPuddleRipple(Layer0, 1.05f * 0.85f);
	Ripples += ProcessPuddleRipple(Layer1, 1.31f * 0.85f);
	Ripples += ProcessPuddleRipple(Layer2, 1.58f * 0.85f);
	return clamp(Ripples * DistanceFade, -1.0f, 1.0f) * rain_params.x;
}

float GetRainPuddle(float2 TexCoord, float3 FlatNormal, float Depth, out float3 WaterNormal, out float2 Refraction)
{
	WaterNormal = mul((float3x3)m_V, float3(0.0f, 1.0f, 0.0f));
	Refraction = 0.0f;
	float Wetness = saturate(rain_params.y);
	[branch]
	if (Wetness <= 0.0f)
	{
		return 0.0f;
	}

	float3 WorldNormal = mul((float3x3)m_invV, normalize(FlatNormal));
	// Terrain Upgrade's slope test, restricted to upward-facing surfaces.
	float Slope = saturate((0.1f - max(abs(WorldNormal.x), abs(WorldNormal.z))) * 13.0f) * step(0.0f, WorldNormal.y);
	float Noise = s_puddles_perlin.Sample(smp_base, TexCoord * 15.0f).r;
	float Amount = saturate((Noise - 0.12f) * Wetness) * Slope;
	// Keep the original soft edge transition (border hardness 0.15).
	Amount = smoothstep(0.0f, 0.255f, Amount);
	[branch]
	if (Amount <= 0.0f)
	{
		return 0.0f;
	}

	float RainIntensity = saturate(rain_params.x * 1.5f);
	float Animation = timers.x * (0.01f + RainIntensity * 0.008f);
	float2 RippleTexCoord = TexCoord * 140.0f;
	float3 Normal0 = s_puddles_normal.Sample(smp_base, RippleTexCoord + float2(0.0f, Animation)).xyz;
	float3 Normal1 = s_puddles_normal.Sample(smp_base, RippleTexCoord - float2(0.33f, Animation)).xyz;
	float3 Ripples = normalize(float3(Normal0.xy + Normal1.xy, Normal0.z * Normal1.z)) * 2.0f - 1.0f;
	Ripples *= saturate(0.05f + RainIntensity * 0.1f);
	Ripples = Ripples * 0.666f + float3(RainPuddleRipples(RippleTexCoord * 1.3f, Depth) * 0.333f, 0.0f);

	WaterNormal = mul((float3x3)m_V, float3(Ripples.x, 1.0f, Ripples.y));
	Refraction = Ripples.xy * 0.15f * smoothstep(0.6f, 0.8f, Amount);
	return Amount;
}

#endif

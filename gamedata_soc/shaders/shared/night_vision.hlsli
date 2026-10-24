#ifndef NIGHT_VISION_HLSLI
#define NIGHT_VISION_HLSLI

uniform float4 NightVisionState;
uniform float4 NightVisionProfile;
uniform float4 nvg_color;

float GetNightVisionVisibility()
{
	// Derived from m_affects.x with a local slew limit for immediate script updates.
	float Problems = saturate(NightVisionState.y);
	float Progress = smoothstep(0.02f, 0.4f, Problems);
	float SlowPulse = 0.65f * (0.5f + 0.5f * sin(timers.x * 5.654867f));
	SlowPulse += 0.35f * (0.5f + 0.5f * sin(timers.x * 10.681415f + 1.7f));
	// Only dim the effect: no white flashes or random full-frame on/off transitions.
	float Flicker = 0.18f * (4.0f * Progress * (1.0f - Progress)) * SlowPulse;
	return (1.0f - Progress) * (1.0f - Flicker);
}

float3 ApplyNightVision(float3 Scene, float2 Texcoord)
{
	float Visibility = NightVisionState.x * GetNightVisionVisibility();
	[branch]
	if (Visibility <= 0.0f)
	{
		return Scene;
	}
	float Noise = frac(sin(dot(Texcoord, float2(12.0f, 78.0f) + timers.x * 3.17f)) * 43758.0f);
	float Luminance = dot(Scene, float3(0.2126f, 0.7152f, 0.0722f));
	float3 Image = Luminance * saturate(nvg_color.rgb) * max(nvg_color.a, 1.0f);
	Image += NightVisionProfile.z * sin(Texcoord.y * timers.x);
	Image += NightVisionProfile.y * sin(timers.x * 50.0f);
	Image += Noise * NightVisionProfile.x;
	Image *= saturate(NightVisionProfile.w - distance(Texcoord, float2(0.5f, 0.5f)) * 1.5f);
	return lerp(Scene, saturate(Image), Visibility);
}

#endif

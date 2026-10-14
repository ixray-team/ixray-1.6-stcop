// Night vision profiles adapted from the source calc_pnv shader.
// UI lens masking is maintained independently by the night vision effector.
#include "common.hlsli"

float4 NightVisionPostprocess;
float NightVisionInterference;

struct PSInput
{
	float4 hpos : SV_POSITION;
	float2 texcoord : TEXCOORD0;
};

uniform float4 nvg_color;

float GetNightVisionVisibility()
{
	// Derived from m_affects.x with a local slew limit for immediate script updates.
	float Problems = saturate(NightVisionInterference);
	float Progress = smoothstep(0.02f, 0.4f, Problems);
	float SlowPulse = 0.65f * (0.5f + 0.5f * sin(timers.x * 5.654867f));
	SlowPulse += 0.35f * (0.5f + 0.5f * sin(timers.x * 10.681415f + 1.7f));
	// Only dim the effect: no white flashes or random full-frame on/off transitions.
	float Flicker = 0.18f * (4.0f * Progress * (1.0f - Progress)) * SlowPulse;
	return (1.0f - Progress) * (1.0f - Flicker);
}

float4 main(PSInput I) : SV_Target
{
	float4 Scene = s_image.SampleLevel(smp_rtlinear, I.texcoord, 0.0f);
	float Visibility = GetNightVisionVisibility();
	[branch]
	if (Visibility <= 0.0f)
	{
		return Scene;
	}

	float Noise = frac(sin(dot(I.texcoord, float2(12.0f, 78.0f) + timers.x * 3.17f)) * 43758.0f);
	float Luminance = dot(Scene.rgb, LUMINANCE_VECTOR);
	float3 Image = Luminance * saturate(nvg_color.rgb) * max(nvg_color.a, 1.0f);
	Image += NightVisionPostprocess.z * sin(I.texcoord.y * timers.x);
	Image += NightVisionPostprocess.y * sin(timers.x * 50.0f);
	Image += Noise * NightVisionPostprocess.x;
	Image *= saturate(NightVisionPostprocess.w - distance(I.texcoord, float2(0.5f, 0.5f)) * 1.5f);

	return float4(lerp(Scene.rgb, saturate(Image), Visibility), Scene.a);
}

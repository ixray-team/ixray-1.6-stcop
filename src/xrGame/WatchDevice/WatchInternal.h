#pragma once

#include "WatchTypes.h"

namespace WatchDetail
{
inline constexpr float ZoneSampleHz = 10.0f;
inline constexpr float CompassGravitySenseMin = 50.0f;
inline constexpr float CompassGravityQueryRadius = 80.0f;
inline constexpr float CompassGravityChaosEnter = 0.05f;
inline constexpr float CompassGravityEarlyOut = 0.95f;
inline constexpr float CompassSpringSettleRad = 0.004363323f;
inline constexpr float CompassSpringMaxStep = 0.05f;
inline constexpr u32 CompassSpringMaxSubsteps = 4;
inline constexpr float SurgeSampleHz = 1.0f;
inline constexpr float BarometerResponse = 0.35f;
inline constexpr float BarometerDrift = 2.0f;
inline constexpr float LightSampleHz = 1.0f;
inline constexpr float LedSmoothing = 4.0f;
inline constexpr float LagSmoothing = 1.5f;
inline constexpr float DisplayMaxHz = 60.0f;
inline constexpr float DisplayMinFreeze = 0.25f;
inline constexpr float DisplayGlitchLevel = 0.9f;
inline constexpr float DisplayGlitchWrap = 1000.0f;
inline constexpr float HudLightMaxStrength = 4.0f;
inline constexpr const char* SurgeSecondsFunction = "watch_surge.seconds_to_surge";
inline constexpr const char* SurgeProgressFunction = "watch_surge.surge_progress";

struct SWatchLedChannelDesc
{
	const char* Name;
	const char* Section;
	const char* PresentSection;
	EWatchGlow Glow;
	u32 FallbackRgba;
};

inline constexpr SWatchLedChannelDesc LedChannels[WatchLedChannelCount] =
	{
		{"anomaly", "anomaly", "indicators.anomaly", EWatchGlow::Anomaly, color_rgba(255, 30, 20, 255)},
		{"motion", "motion", "indicators.motion", EWatchGlow::Motion, color_rgba(255, 255, 255, 255)},
		{"noise", "noise", "indicators.noise", EWatchGlow::Noise, color_rgba(255, 220, 80, 255)}
};

inline const SWatchLedChannelDesc& ChannelDesc(EWatchLedChannel Channel)
{
	return LedChannels[u32(Channel)];
}

inline shared_str MakeWatchSection(const shared_str& Root, const char* Suffix)
{
	if (!Suffix || !Suffix[0])
	{
		return Root;
	}

	string256 Buffer = {};
	xr_sprintf(Buffer, "%s.%s", Root.c_str(), Suffix);
	return shared_str(Buffer);
}

template <class F>
void VisitSections(SWatchConfig& A, SWatchConfig& B, F&& Func)
{
	Func("", A.Root, B.Root, true);
	Func("bones", A.Bones, B.Bones, false);
	Func("indicators", A.Masters, B.Masters, false);
	Func("compass", A.Compass, B.Compass, true);
	Func("display", A.Display, B.Display, true);
	Func("fonts", A.Fonts, B.Fonts, true);
	Func("lag.surge", A.SurgeLag, B.SurgeLag, true);
	Func("lag.anomaly", A.AnomalyLag, B.AnomalyLag, true);
	Func("light", A.Light, B.Light, true);

	for (u32 Index = 0; Index < WatchLedChannelCount; ++Index)
	{
		const SWatchLedChannelDesc& Desc = LedChannels[Index];
		if (Index == u32(EWatchLedChannel::Anomaly))
		{
			Func(Desc.Section, A.Anomaly, B.Anomaly, true);
		}
		Func(Desc.Section, A.Blink[Index], B.Blink[Index], true);
		Func(Desc.PresentSection, A.Present[Index], B.Present[Index], true);
	}
}
}

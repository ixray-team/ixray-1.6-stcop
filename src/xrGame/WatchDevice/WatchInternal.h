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
inline constexpr float ConditionSmoothing = 6.0f;
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

struct SWatchConditionDesc
{
	const char* Name;
	const char* PresentSection;
	const char* XmlIcon;
	const char* XmlBar;
	u32 FallbackRgba;
};

inline constexpr SWatchConditionDesc ConditionChannels[WatchConditionCount] =
	{
		{"health", "conditions.health", "health_icon", "health_bar", color_rgba(255, 70, 70, 255)},
		{"power", "conditions.power", "power_icon", "power_bar", color_rgba(255, 220, 80, 255)},
		{"radiation", "conditions.radiation", "radiation_icon", "radiation_bar", color_rgba(80, 255, 120, 255)},
		{"satiety", "conditions.satiety", "satiety_icon", "satiety_bar", color_rgba(255, 180, 60, 255)},
		{"thirst", "conditions.thirst", "thirst_icon", "thirst_bar", color_rgba(80, 180, 255, 255)},
		{"sleepiness", "conditions.sleepiness", "sleepiness_icon", "sleepiness_bar", color_rgba(180, 140, 255, 255)},
		{"intoxication", "conditions.intoxication", "intoxication_icon", "intoxication_bar", color_rgba(160, 255, 80, 255)},
		{"bleeding", "conditions.bleeding", "bleeding_icon", "bleeding_bar", color_rgba(255, 40, 40, 255)}
};

inline const SWatchConditionDesc& ConditionDesc(EWatchCondition Condition)
{
	return ConditionChannels[u32(Condition)];
}

inline float ConditionBadness(float DisplayValue, bool SeverityHigher)
{
	const float Value = clampr(DisplayValue, 0.0f, 1.0f);
	return SeverityHigher ? Value : (1.0f - Value);
}

inline EWatchConditionSeverity EvaluateConditionSeverity(float Badness, const SWatchConditionPresent& Present)
{
	Badness = clampr(Badness, 0.0f, 1.0f);
	if (Badness >= Present.TierCritical)
	{
		return EWatchConditionSeverity::Critical;
	}
	if (Badness >= Present.TierMedium)
	{
		return EWatchConditionSeverity::Medium;
	}
	if (Badness >= Present.TierWeak)
	{
		return EWatchConditionSeverity::Weak;
	}
	return EWatchConditionSeverity::None;
}

inline float ConditionSeverityGlow(EWatchConditionSeverity Severity, const SWatchConditionPresent& Present)
{
	switch (Severity)
	{
		case EWatchConditionSeverity::Weak:
			return Present.GlowWeak;
		case EWatchConditionSeverity::Medium:
			return Present.GlowMedium;
		case EWatchConditionSeverity::Critical:
			return Present.GlowCritical;
		default:
			return Present.GlowNone;
	}
}

inline u32 ConditionColorFromRgba(const Fvector4& Color)
{
	const u32 R = u32(clampr(iFloor(Color.x + 0.5f), 0, 255));
	const u32 G = u32(clampr(iFloor(Color.y + 0.5f), 0, 255));
	const u32 B = u32(clampr(iFloor(Color.z + 0.5f), 0, 255));
	const u32 A = u32(clampr(iFloor(Color.w + 0.5f), 0, 255));
	return color_rgba(R, G, B, A);
}

inline u32 ConditionSeverityColor(EWatchConditionSeverity Severity, const SWatchConditionPresent& Present, u32 Fallback)
{
	const Fvector4* Color = nullptr;
	switch (Severity)
	{
		case EWatchConditionSeverity::Weak:
			Color = &Present.ColorWeak;
			break;
		case EWatchConditionSeverity::Medium:
			Color = &Present.ColorMedium;
			break;
		case EWatchConditionSeverity::Critical:
			Color = &Present.ColorCritical;
			break;
		default:
			return Fallback;
	}

	if (!Color || Color->w <= EPS)
	{
		return Fallback;
	}

	return ConditionColorFromRgba(*Color);
}

inline const char* ConditionSeverityName(EWatchConditionSeverity Severity)
{
	switch (Severity)
	{
		case EWatchConditionSeverity::Weak:
			return "weak";
		case EWatchConditionSeverity::Medium:
			return "medium";
		case EWatchConditionSeverity::Critical:
			return "critical";
		default:
			return "none";
	}
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
	Func("conditions", A.ConditionMasters, B.ConditionMasters, false);
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

	for (u32 Index = 0; Index < WatchConditionCount; ++Index)
	{
		const SWatchConditionDesc& Desc = ConditionChannels[Index];
		Func(Desc.PresentSection, A.Conditions[Index], B.Conditions[Index], true);
	}
}
}

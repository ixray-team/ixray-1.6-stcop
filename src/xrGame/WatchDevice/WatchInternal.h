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
inline constexpr const char* ScriptSourcePrefix = "script:";
inline constexpr u32 DefaultLedRgba = color_rgba(255, 255, 255, 255);
inline constexpr u32 DefaultGaugeRgba = color_rgba(255, 255, 255, 255);

struct SWatchNamedColor
{
	const char* Name;
	u32 Rgba;
};

inline constexpr SWatchNamedColor LedColors[] =
	{
		{"anomaly", color_rgba(255, 30, 20, 255)},
		{"motion", color_rgba(255, 255, 255, 255)},
		{"noise", color_rgba(255, 220, 80, 255)}
};

inline constexpr SWatchNamedColor GaugeColors[] =
	{
		{"health", color_rgba(255, 70, 70, 255)},
		{"power", color_rgba(255, 220, 80, 255)},
		{"radiation", color_rgba(80, 255, 120, 255)},
		{"satiety", color_rgba(255, 180, 60, 255)},
		{"thirst", color_rgba(80, 180, 255, 255)},
		{"sleepiness", color_rgba(180, 140, 255, 255)},
		{"intoxication", color_rgba(160, 255, 80, 255)},
		{"bleeding", color_rgba(255, 40, 40, 255)}
};

inline constexpr const char* ConditionNames[WatchConditionCount] =
	{
		"health",
		"power",
		"radiation",
		"satiety",
		"thirst",
		"sleepiness",
		"intoxication",
		"bleeding"
};

template <size_t TCount>
u32 FindNamedColor(const SWatchNamedColor (&Table)[TCount], const shared_str& Name, u32 Fallback)
{
	for (const SWatchNamedColor& Entry : Table)
	{
		if (Name.equal(Entry.Name))
		{
			return Entry.Rgba;
		}
	}
	return Fallback;
}

inline u32 LedFallbackColor(const shared_str& Name)
{
	return FindNamedColor(LedColors, Name, DefaultLedRgba);
}

inline u32 GaugeFallbackColor(const shared_str& Name)
{
	return FindNamedColor(GaugeColors, Name, DefaultGaugeRgba);
}

inline bool FindCondition(const char* Name, EWatchCondition& Out)
{
	for (u32 Index = 0; Index < WatchConditionCount; ++Index)
	{
		if (!xr_strcmp(Name, ConditionNames[Index]))
		{
			Out = EWatchCondition(Index);
			return true;
		}
	}
	return false;
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
		case EWatchConditionSeverity::Weak: return Present.GlowWeak;
		case EWatchConditionSeverity::Medium: return Present.GlowMedium;
		case EWatchConditionSeverity::Critical: return Present.GlowCritical;
		default: return Present.GlowNone;
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

inline const Fvector4* ConditionSeverityRgba(EWatchConditionSeverity Severity, const SWatchConditionPresent& Present)
{
	switch (Severity)
	{
		case EWatchConditionSeverity::Weak: return &Present.ColorWeak;
		case EWatchConditionSeverity::Medium: return &Present.ColorMedium;
		case EWatchConditionSeverity::Critical: return &Present.ColorCritical;
		default: return nullptr;
	}
}

inline const char* ConditionSeverityName(EWatchConditionSeverity Severity)
{
	switch (Severity)
	{
		case EWatchConditionSeverity::Weak: return "weak";
		case EWatchConditionSeverity::Medium: return "medium";
		case EWatchConditionSeverity::Critical: return "critical";
		default: return "none";
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
	Func("compass", A.Compass, B.Compass, true);
	Func("display", A.Display, B.Display, true);
	Func("fonts", A.Fonts, B.Fonts, true);
	Func("lag.surge", A.SurgeLag, B.SurgeLag, true);
	Func("lag.anomaly", A.AnomalyLag, B.AnomalyLag, true);
	Func("light", A.Light, B.Light, true);
	Func("anomaly", A.Anomaly, B.Anomaly, true);

	if (A.Leds.size() == B.Leds.size())
	{
		SWatchMasters<SWatchLedElement> MastersA = {&A.Leds};
		SWatchMasters<SWatchLedElement> MastersB = {&B.Leds};
		Func("indicators", MastersA, MastersB, false);

		for (size_t Index = 0; Index < A.Leds.size(); ++Index)
		{
			Func(A.Leds[Index].Name.c_str(), A.Leds[Index].Blink, B.Leds[Index].Blink, true);
			Func(A.Leds[Index].PresentSection.c_str(), A.Leds[Index].Present, B.Leds[Index].Present, true);
		}
	}

	if (A.Gauges.size() == B.Gauges.size())
	{
		SWatchMasters<SWatchGaugeElement> MastersA = {&A.Gauges};
		SWatchMasters<SWatchGaugeElement> MastersB = {&B.Gauges};
		Func("conditions", MastersA, MastersB, false);

		for (size_t Index = 0; Index < A.Gauges.size(); ++Index)
		{
			Func(A.Gauges[Index].PresentSection.c_str(), A.Gauges[Index].Present, B.Gauges[Index].Present, true);
		}
	}
}
}

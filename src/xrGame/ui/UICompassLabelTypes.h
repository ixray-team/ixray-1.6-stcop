#pragma once

enum class ECompassLabelKind : u8
{
	Cardinal = 0,
	Intermediate,
	Degree
};

struct SCompassLabelSettings
{
	bool showCardinal = true;
	bool showDegrees = false;
	bool showIntermediateCardinal = false;
	u32 degreeStep = 30;
};

struct SCompassLabelDesc
{
	float angleRad = 0.0f;
	float navAngleDeg = 0.0f;
	ECompassLabelKind kind = ECompassLabelKind::Cardinal;
	shared_str id;
	shared_str label;
};

struct SCompassCardinalDirection
{
	float navAngleDeg;
	const char* id;
	const char* label;
	bool intermediate;
};

namespace CompassLabels
{
	inline constexpr SCompassCardinalDirection kDirections[] = {
		{ 0.f, "n", "N", false },
		{ 45.f, "ne", "NE", true },
		{ 90.f, "e", "E", false },
		{ 135.f, "se", "SE", true },
		{ 180.f, "s", "S", false },
		{ 225.f, "sw", "SW", true },
		{ 270.f, "w", "W", false },
		{ 315.f, "nw", "NW", true },
	};

	inline constexpr u32 kValidDegreeSteps[] = {
		1, 2, 3, 4, 5, 6,
		10, 12, 15, 18, 20,
		30, 36, 45, 60, 90, 120, 180
	};

	inline constexpr float kMinorTickStepDeg = 5.0f;

	inline float NavDegToEngineRad(float navAngleDeg)
	{
		return deg2rad(90.0f - navAngleDeg);
	}

	inline float NormalizeNavDeg(float navAngleDeg)
	{
		float a = fmodf(navAngleDeg, 360.0f);
		if (a < 0.0f)
		{
			a += 360.0f;
		}
		return a;
	}

	inline u32 SanitizeDegreeStep(u32 step)
	{
		if (step == 0)
		{
			return 30;
		}

		u32 best = kValidDegreeSteps[0];
		u32 bestDist = u32(-1);
		for (u32 candidate : kValidDegreeSteps)
		{
			const u32 dist = (candidate > step) ? (candidate - step) : (step - candidate);
			if (dist < bestDist)
			{
				bestDist = dist;
				best = candidate;
			}
		}
		return best;
	}
}

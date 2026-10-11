#include "StdAfx.h"
#include "WatchLogic.h"
#include "WatchInternal.h"

static bool ParseLightLevel(const char* Name, EWatchLightLevel& Level)
{
	static const xr_pair<const char*, EWatchLightLevel> Levels[] =
		{
			{"day", EWatchLightLevel::Day},
			{"dusk", EWatchLightLevel::Dusk},
			{"night", EWatchLightLevel::Night},
			{"dark", EWatchLightLevel::Dark}
	};

	for (const auto& [Key, Value] : Levels)
	{
		if (!xr_strcmp(Name, Key))
		{
			Level = Value;
			return true;
		}
	}
	return false;
}

static float LightLevelEmission(const SWatchLight& Light, EWatchLightLevel Level)
{
	switch (Level)
	{
		case EWatchLightLevel::Dusk:
			return Light.DuskEmission;
		case EWatchLightLevel::Night:
			return Light.NightEmission;
		case EWatchLightLevel::Dark:
			return Light.DarkEmission;
		default:
			return Light.DayEmission;
	}
}

namespace WatchLogic
{
float SmoothTowards(float Current, float Target, float Dt, float Rate)
{
	return Current + (Target - Current) * (1.0f - expf(-Dt * Rate));
}

float HashUnit(u32 Value)
{
	Value ^= Value >> 16;
	Value *= 0x7FEB352Du;
	Value ^= Value >> 15;
	Value *= 0x846CA68Bu;
	Value ^= Value >> 16;
	return float(Value & 0xFFFFFFu) / float(0x1000000u);
}

float EvaluateLagUpdateHz(float BaseHz, float MinHz, float Strength)
{
	if (MinHz <= EPS || Strength <= EPS)
	{
		return BaseHz;
	}

	const float From = (BaseHz > EPS) ? BaseHz : WatchDetail::DisplayMaxHz;
	return (MinHz < From) ? From + (MinHz - From) * Strength : From;
}

float DisplayFlickerNoise(float Phase)
{
	const float Wave = 0.55f * sinf(Phase * 11.0f) + 0.30f * sinf(Phase * 29.0f + 1.7f) + 0.15f * sinf(Phase * 67.0f + 0.4f);
	return clampr(0.5f + 0.5f * Wave, 0.0f, 1.0f);
}

float CompassChaosSpin(float Time)
{
	return sinf(Time * 19.0f) * 1.35f +
		   cosf(Time * 11.0f) * 0.95f +
		   sinf(Time * 31.0f) * 0.55f +
		   sinf(Time * 47.0f) * cosf(Time * 13.0f) * 0.35f;
}

void UpdateLedBlink(float& BlinkPhase, float& Value, const SWatchLedBlink& Blink, float Intensity, float Dt)
{
	if (Intensity <= Blink.Threshold)
	{
		BlinkPhase = 0.0f;
		Value = 0.0f;
		return;
	}

	if (Intensity >= Blink.SolidThreshold)
	{
		BlinkPhase = 0.0f;
		Value = 1.0f;
		return;
	}

	const float Span = std::max(Blink.SolidThreshold - Blink.Threshold, EPS);
	const float T = clampr((Intensity - Blink.Threshold) / Span, 0.0f, 1.0f);
	BlinkPhase += Dt * (Blink.BlinkMinHz + (Blink.BlinkMaxHz - Blink.BlinkMinHz) * T);
	BlinkPhase -= floorf(BlinkPhase);
	Value = (BlinkPhase < Blink.BlinkDuty) ? 1.0f : 0.0f;
}

void ParseFormat(const char* Format, bool Date, xr_vector<SWatchFormatToken>& Out)
{
	Out.clear();

	for (const char* Cursor = Format; Cursor && *Cursor;)
	{
		auto MatchLambda = [&Cursor](const char* Key, size_t Length)
		{
			if (strncmp(Cursor, Key, Length) != 0)
			{
				return false;
			}
			Cursor += Length;
			return true;
		};

		if (MatchLambda("YYYY", 4))
		{
			Out.push_back({EWatchFormatToken::Year4});
		}
		else if (MatchLambda("YY", 2))
		{
			Out.push_back({EWatchFormatToken::Year2});
		}
		else if (MatchLambda("HH", 2))
		{
			Out.push_back({EWatchFormatToken::Hour});
		}
		else if (MatchLambda("MM", 2))
		{
			Out.push_back({Date ? EWatchFormatToken::Month : EWatchFormatToken::Minute});
		}
		else if (MatchLambda("SS", 2))
		{
			Out.push_back({EWatchFormatToken::Second});
		}
		else if (MatchLambda("DD", 2))
		{
			Out.push_back({EWatchFormatToken::Day});
		}
		else
		{
			Out.push_back({EWatchFormatToken::Literal, *Cursor++});
		}
	}
}

void FormatText(const xr_vector<SWatchFormatToken>& Tokens, const SWatchGameTime& Time, string64& Out)
{
	Out[0] = 0;

	string16 Part = {};
	for (const SWatchFormatToken& Token : Tokens)
	{
		switch (Token.Type)
		{
			case EWatchFormatToken::Hour:
				xr_sprintf(Part, "%02u", Time.Hour);
				break;
			case EWatchFormatToken::Minute:
				xr_sprintf(Part, "%02u", Time.Minute);
				break;
			case EWatchFormatToken::Second:
				xr_sprintf(Part, "%02u", Time.Second);
				break;
			case EWatchFormatToken::Day:
				xr_sprintf(Part, "%02u", Time.Day);
				break;
			case EWatchFormatToken::Month:
				xr_sprintf(Part, "%02u", Time.Month);
				break;
			case EWatchFormatToken::Year2:
				xr_sprintf(Part, "%02u", Time.Year % 100);
				break;
			case EWatchFormatToken::Year4:
				xr_sprintf(Part, "%04u", Time.Year);
				break;
			default:
				Part[0] = Token.Literal;
				Part[1] = 0;
				break;
		}
		xr_strcat(Out, Part);
	}
}

bool ParseLightSchedule(const char* Text, xr_vector<SWatchLightKey>& Keys)
{
	Keys.clear();
	if (!Text)
	{
		return false;
	}

	const int Count = _GetItemCount(Text);
	for (int Index = 0; Index < Count; ++Index)
	{
		string64 Item = {};
		string16 Name = {};
		SWatchLightKey Key;
		_GetItem(Text, Index, Item);
		if (sscanf(Item, "%f:%15s", &Key.Hour, Name) != 2 || Key.Hour < 0.0f || Key.Hour >= 24.0f || !ParseLightLevel(Name, Key.Level))
		{
			Keys.clear();
			return false;
		}
		if (!Keys.empty() && Key.Hour <= Keys.back().Hour)
		{
			Keys.clear();
			return false;
		}
		Keys.push_back(Key);
	}
	return !Keys.empty();
}

float EvaluateLightSchedule(const xr_vector<SWatchLightKey>& Keys, const SWatchLight& Light, float Hour)
{
	if (Keys.empty())
	{
		return 0.0f;
	}

	const SWatchLightKey* From = &Keys.back();
	const SWatchLightKey* To = &Keys.front();
	for (size_t Index = 0; Index + 1 < Keys.size(); ++Index)
	{
		if (Hour >= Keys[Index].Hour && Hour < Keys[Index + 1].Hour)
		{
			From = &Keys[Index];
			To = &Keys[Index + 1];
			break;
		}
	}

	float Span = To->Hour - From->Hour;
	float Offset = Hour - From->Hour;
	if (Span <= 0.0f)
	{
		Span += 24.0f;
	}
	if (Offset < 0.0f)
	{
		Offset += 24.0f;
	}

	const float A = LightLevelEmission(Light, From->Level);
	const float B = LightLevelEmission(Light, To->Level);
	return A + (B - A) * clampr(Offset / Span, 0.0f, 1.0f);
}

bool IsPointerKind(EWatchCompassSourceKind Kind)
{
	return Kind == EWatchCompassSourceKind::Story ||
		Kind == EWatchCompassSourceKind::Additional ||
		Kind == EWatchCompassSourceKind::UserNav;
}

bool WorldDirToCompassAngle(const Fvector& WorldDir, const Fmatrix& View, float OffsetRad, float& OutAngle)
{
	Fvector Direction = WorldDir;
	Direction.y = 0.0f;
	if (Direction.square_magnitude() < EPS_L)
	{
		return false;
	}

	Direction.normalize();
	Fvector LocalDir;
	View.transform_dir(LocalDir, Direction);
	LocalDir.y = 0.0f;

	if (LocalDir.square_magnitude() < EPS_L)
	{
		return false;
	}

	OutAngle = angle_normalize_signed(-LocalDir.getH() + OffsetRad);
	return true;
}

void StepCompassSpring(float& Angle, float& Velocity, float Desired, float Stiffness, float Damping, float Dt)
{
	float Remaining = Dt;
	u32 Steps = 0;
	while (Remaining > 0.0f && Steps < WatchDetail::CompassSpringMaxSubsteps)
	{
		const float Step = (Remaining > WatchDetail::CompassSpringMaxStep) ? WatchDetail::CompassSpringMaxStep : Remaining;
		Remaining -= Step;
		++Steps;

		const float Error = angle_difference_signed(Desired, Angle);
		if (fabsf(Error) < WatchDetail::CompassSpringSettleRad)
		{
			Angle = Desired;
			Velocity = 0.0f;
			break;
		}

		const float Accel = Stiffness * Error - Damping * Velocity;
		Velocity += Accel * Step;
		Angle = angle_normalize_signed(Angle + Velocity * Step);
	}
}

void MakeOffsetXform(const Fvector& Position, const Fvector& RotationDeg, Fmatrix& Out)
{
	Fvector RotationRad = RotationDeg;
	RotationRad.mul(PI / 180.0f);
	Out.setHPB(RotationRad.x, RotationRad.y, RotationRad.z);
	Out.translate_over(Position);
}
}

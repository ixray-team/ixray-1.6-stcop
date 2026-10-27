#include "../../xrGame/WeaponsModifiers.h"

#include <cmath>
#include <cstdio>
#include <initializer_list>
#include <limits>

static int g_Failures = 0;

static void Check(bool IsValid, const char* Message)
{
	if (!IsValid)
	{
		std::fprintf(stderr, "%s\n", Message);
		++g_Failures;
	}
}

static bool IsNear(float Actual, float Expected)
{
	return std::fabs(Actual - Expected) < 0.00001f;
}

int main()
{
	WeaponsModifiers Modifiers;
	Check(Modifiers.Get(WeaponModifier::HitPowerCoeff) == 1.0f && Modifiers.Get(WeaponModifier::RpmCoeff) == 1.0f, "Default coefficients must be neutral");
	Check(Modifiers.ApplyHitPower(0.7f) == 0.7f && Modifiers.ApplyShotInterval(0.1f) == 0.1f, "Neutral coefficients must preserve the original values exactly");
	Check(Modifiers.Set(WeaponModifier::HitPowerCoeff, 1.2f), "Valid damage coefficient rejected");
	Check(IsNear(Modifiers.ApplyHitPower(0.7f), 0.84f), "The multiplier must apply to upgraded damage, not the original base");
	Modifiers.Set(WeaponModifier::HitPowerCoeff, 1.2f);
	Check(IsNear(Modifiers.ApplyHitPower(0.7f), 0.84f), "Repeated assignment must not compound damage");
	Check(Modifiers.Set(WeaponModifier::RpmCoeff, 2.0f), "Valid RPM coefficient rejected");
	Check(IsNear(Modifiers.ApplyShotInterval(0.1f), 0.05f), "Higher RPM must shorten the shot interval");
	Check(IsNear(Modifiers.ApplyShotInterval(0.3f), 0.15f), "Alternative single-shot intervals must scale too");
	Modifiers.Set(WeaponModifier::RpmCoeff, 0.5f);
	Check(IsNear(Modifiers.ApplyShotInterval(0.1f), 0.2f), "Lower RPM must lengthen the shot interval");
	Check(!Modifiers.Set(WeaponModifier::RpmCoeff, 0.0f), "Zero RPM must be rejected");
	Check(Modifiers.Get(WeaponModifier::RpmCoeff) == 0.5f, "A rejected setter must preserve the previous value");
	Check(Modifiers.Set(WeaponModifier::HitPowerCoeff, 0.0f) && Modifiers.ApplyHitPower(0.7f) == 0.0f, "Zero damage is a valid multiplier");

	for (WeaponModifier Type : {WeaponModifier::HitPowerCoeff, WeaponModifier::RpmCoeff})
	{
		const float Previous = Modifiers.Get(Type);
		volatile float InvalidValues[] = {-1.0f, std::numeric_limits<float>::infinity(), std::numeric_limits<float>::quiet_NaN()};
		for (float Value : InvalidValues)
		{
			Check(!Modifiers.Set(Type, Value), "Negative, infinite or NaN coefficient accepted");
		}
		Check(Modifiers.Get(Type) == Previous, "Invalid coefficients changed state");
	}

	Check(!Modifiers.Set((WeaponModifier)100, 2.0f), "Unknown modifier accepted");
	Check(Modifiers.Get((WeaponModifier)100) == 1.0f, "Unknown modifier getter must be neutral");
	Modifiers.Set(WeaponModifier::RpmCoeff, std::numeric_limits<float>::min());
	Check(Modifiers.ApplyShotInterval(10.0f) == 10.0f, "Overflowed shot interval must fall back safely");
	Modifiers.Set(WeaponModifier::RpmCoeff, FLT_MAX);
	Check(Modifiers.ApplyShotInterval(std::numeric_limits<float>::min()) == std::numeric_limits<float>::min(), "Underflowed shot interval must fall back safely");
	Modifiers = {};
	Check(Modifiers.ApplyHitPower(0.7f) == 0.7f && Modifiers.ApplyShotInterval(0.1f) == 0.1f, "Actor reinitialization must restore both coefficients");
	return g_Failures == 0 ? 0 : 1;
}

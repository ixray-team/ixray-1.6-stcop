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
	for (int Index = 0; Index < (int)WeaponModifier::Count; ++Index)
	{
		Check(Modifiers.Get((WeaponModifier)Index) == 1.0f, "Every default coefficient must be neutral");
	}
	Check(Modifiers.ApplyHitPower(0.7f) == 0.7f && Modifiers.ApplyShotInterval(0.1f) == 0.1f, "Neutral coefficients must preserve the original values exactly");
	Check(Modifiers.ApplyHitPower(0.7f, true) == 0.7f && Modifiers.ApplyHitImpulse(120.0f) == 120.0f, "Neutral silenced damage and impact impulse must be unchanged");
	Check(Modifiers.ApplyFireDispersionBase(0.03f) == 0.03f && Modifiers.ApplyCrosshairInertion(5.91f) == 5.91f && Modifiers.ApplyBulletSpeed(900.0f) == 900.0f, "Neutral dispersion, inertion and speed must be unchanged");
	Check(Modifiers.Set(WeaponModifier::HitPowerCoeff, 1.2f), "Valid damage coefficient rejected");
	Check(IsNear(Modifiers.ApplyHitPower(0.7f), 0.84f), "The multiplier must apply to upgraded damage, not the original base");
	Modifiers.Set(WeaponModifier::HitPowerCoeff, 1.2f);
	Check(IsNear(Modifiers.ApplyHitPower(0.7f), 0.84f), "Repeated assignment must not compound damage");
	Check(Modifiers.Set(WeaponModifier::SilencerHitPowerCoeff, 1.5f) && Modifiers.Set(WeaponModifier::UnsilencedHitPowerCoeff, 0.8f), "Independent damage coefficients rejected");
	Check(IsNear(Modifiers.ApplyHitPower(0.7f, true), 1.26f), "Silenced damage must compose the common and silencer coefficients");
	Check(IsNear(Modifiers.ApplyHitPower(0.7f, false), 0.672f), "Unsilenced damage must not include the silencer coefficient");
	Check(IsNear(Modifiers.ApplyHitPower(0.7f, true) * 0.9f * 1.1f, 1.2474f), "Silencer and cartridge damage factors must still compose with actor modifiers");
	Modifiers.Set(WeaponModifier::SilencerHitPowerCoeff, 1.0f);
	Check(IsNear(Modifiers.ApplyHitPower(0.7f, true), 0.84f), "A silenced shot must not fall back to the unsilenced modifier when its own coefficient is neutral");
	Modifiers.Set(WeaponModifier::UnsilencedHitPowerCoeff, 1.0f);
	Check(Modifiers.Set(WeaponModifier::HitImpulseCoeff, 2.0f), "Impact impulse coefficient rejected");
	Check(IsNear(Modifiers.ApplyHitImpulse(120.0f * 0.5f) * 1.5f, 180.0f), "Impact impulse must compose weapon, silencer, actor and cartridge factors");
	Check(IsNear(Modifiers.ApplyHitPower(0.7f), 0.84f), "Impact impulse must not change bullet damage");
	Check(Modifiers.Set(WeaponModifier::FireDispersionBaseCoeff, 0.5f), "Dispersion coefficient rejected");
	Check(IsNear(Modifiers.ApplyFireDispersionBase(0.04f) + 0.02f, 0.04f), "Accuracy must scale base dispersion without scaling the shooter's added dispersion");
	Check(Modifiers.Set(WeaponModifier::CrosshairInertionCoeff, 0.5f), "Handling coefficient rejected");
	Check(IsNear(Modifiers.ApplyCrosshairInertion(5.91f), 2.955f), "Handling must scale crosshair settling time");
	Check(Modifiers.Set(WeaponModifier::BulletSpeedCoeff, 1.5f), "Bullet speed coefficient rejected");
	Check(IsNear(Modifiers.ApplyBulletSpeed(800.0f * 0.75f), 900.0f), "Bullet speed must include the silencer factor");
	Check(IsNear(Modifiers.ApplyBulletSpeed(400.0f), 600.0f), "Initial burst speed must scale from the current value");
	Check(!Modifiers.Set(WeaponModifier::BulletSpeedCoeff, 0.0f) && Modifiers.Get(WeaponModifier::BulletSpeedCoeff) == 1.5f, "Zero bullet speed must be rejected without changing state");
	Check(Modifiers.Set(WeaponModifier::RpmCoeff, 2.0f), "Valid RPM coefficient rejected");
	Check(IsNear(Modifiers.ApplyShotInterval(0.1f), 0.05f), "Higher RPM must shorten the shot interval");
	Check(IsNear(Modifiers.ApplyShotInterval(0.3f), 0.15f), "Alternative single-shot intervals must scale too");
	Modifiers.Set(WeaponModifier::RpmCoeff, 0.5f);
	Check(IsNear(Modifiers.ApplyShotInterval(0.1f), 0.2f), "Lower RPM must lengthen the shot interval");
	Check(!Modifiers.Set(WeaponModifier::RpmCoeff, 0.0f), "Zero RPM must be rejected");
	Check(Modifiers.Get(WeaponModifier::RpmCoeff) == 0.5f, "A rejected setter must preserve the previous value");
	Check(Modifiers.Set(WeaponModifier::HitPowerCoeff, 0.0f) && Modifiers.ApplyHitPower(0.7f) == 0.0f, "Zero damage is a valid multiplier");

	for (int Index = 0; Index < (int)WeaponModifier::Count; ++Index)
	{
		const WeaponModifier Type = (WeaponModifier)Index;
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
	Modifiers.Set(WeaponModifier::BulletSpeedCoeff, FLT_MAX);
	Check(Modifiers.ApplyBulletSpeed(900.0f) == 900.0f, "Overflowed bullet speed must fall back safely");
	Modifiers.Set(WeaponModifier::BulletSpeedCoeff, std::numeric_limits<float>::min());
	Check(Modifiers.ApplyBulletSpeed(std::numeric_limits<float>::min()) == std::numeric_limits<float>::min(), "Underflowed bullet speed must fall back safely");
	for (WeaponModifier Type : {WeaponModifier::SilencerHitPowerCoeff, WeaponModifier::UnsilencedHitPowerCoeff, WeaponModifier::HitImpulseCoeff, WeaponModifier::FireDispersionBaseCoeff, WeaponModifier::CrosshairInertionCoeff})
	{
		Check(Modifiers.Set(Type, 0.0f) && Modifiers.Get(Type) == 0.0f, "Zero must be supported for damage, impact impulse, base dispersion and crosshair inertion");
	}
	Check(Modifiers.ApplyHitImpulse(120.0f) == 0.0f && Modifiers.ApplyFireDispersionBase(0.03f) == 0.0f && Modifiers.ApplyCrosshairInertion(5.91f) == 0.0f, "Zero coefficients must disable their corresponding contribution");
	Modifiers = {};
	for (int Index = 0; Index < (int)WeaponModifier::Count; ++Index)
	{
		Check(Modifiers.Get((WeaponModifier)Index) == 1.0f, "Actor reinitialization must restore every coefficient");
	}
	Check(Modifiers.ApplyHitPower(0.7f) == 0.7f && Modifiers.ApplyShotInterval(0.1f) == 0.1f, "Actor reinitialization must restore neutral calculations");
	return g_Failures == 0 ? 0 : 1;
}

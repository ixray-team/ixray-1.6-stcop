#pragma once

#include <cfloat>
#include "../xrCore/Containers/FixedVector.h"

// IDs define Lua constants and the fixed order of coefficients in actor saves.
namespace WeaponModifiers
{
enum WeaponModifier : u32
{
	eBulletSpeed,
	eCrosshairInertion,
	eFireDispersionBase,
	eHitImpulse,
	eHitPower,
	eRecoil,
	eRpm,
	eSilencerHitPower,
	eUnsilencedHitPower,
	eWpnModifierTypeMax
};

using WpnModifierType = FixedVector<float, WeaponModifier::eWpnModifierTypeMax>;

inline float ApplyHitPower(const WpnModifierType& Modifiers, float HitPower, bool IsSilenced = false)
{
	return HitPower * Modifiers[WeaponModifier::eHitPower] * (IsSilenced ? Modifiers[WeaponModifier::eSilencerHitPower] : Modifiers[WeaponModifier::eUnsilencedHitPower]);
}

inline float ApplyHitImpulse(const WpnModifierType& Modifiers, float HitImpulse)
{
	return HitImpulse * Modifiers[WeaponModifier::eHitImpulse];
}

inline float ApplyFireDispersionBase(const WpnModifierType& Modifiers, float Dispersion)
{
	return Dispersion * Modifiers[WeaponModifier::eFireDispersionBase];
}

inline float ApplyCrosshairInertion(const WpnModifierType& Modifiers, float Inertion)
{
	return Inertion * Modifiers[WeaponModifier::eCrosshairInertion];
}

inline float ApplyRecoil(const WpnModifierType& Modifiers, float Offset)
{
	return Offset * Modifiers[WeaponModifier::eRecoil];
}

inline float ApplyRecoilKick(const WpnModifierType& Modifiers, float PreviousAngle, float ShotAngle, float MaxAngle)
{
	if (Modifiers[WeaponModifier::eRecoil] == 1.0f)
	{
		return ShotAngle;
	}
	// Scale only this shot's contribution, keeping the accumulated angle intact.
	const float Result = PreviousAngle + ApplyRecoil(Modifiers, ShotAngle - PreviousAngle);
	if (Result < -MaxAngle)
	{
		return -MaxAngle;
	}
	if (Result > MaxAngle)
	{
		return MaxAngle;
	}
	return Result;
}

inline float ApplyBulletSpeed(const WpnModifierType& Modifiers, float BulletSpeed)
{
	return BulletSpeed * Modifiers[WeaponModifier::eBulletSpeed];
}

inline float ApplyShotInterval(const WpnModifierType& Modifiers, float Interval)
{
	const float RpmCoeff = Modifiers[WeaponModifier::eRpm];
	if (RpmCoeff == 1.0f)
	{
		return Interval;
	}

	const float Result = Interval / RpmCoeff;
	// Keep the float shot timer advancing and bound catch-up shots per frame.
	constexpr float MinimumShotInterval = 0.001f;
	return Result < MinimumShotInterval ? MinimumShotInterval : Result;
}
} // namespace WeaponModifiers

using WeaponModifier = WeaponModifiers::WeaponModifier;

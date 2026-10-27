#pragma once

#include <cfloat>
#include "../xrCore/Containers/FixedVector.h"

// IDs define Lua constants and the fixed order of coefficients in actor saves.
namespace WeaponModifiers
{
enum WeaponModifier : u32
{
	eBulletSpeedCoeff,
	eCrosshairInertionCoeff,
	eFireDispersionBaseCoeff,
	eHitImpulseCoeff,
	eHitPowerCoeff,
	eRecoilCoeff,
	eRpmCoeff,
	eSilencerHitPowerCoeff,
	eUnsilencedHitPowerCoeff,
	eWpnModifierTypeMax
};

using WpnModifierType = FixedVector<float, WeaponModifier::eWpnModifierTypeMax>;

inline float ApplyHitPower(const WpnModifierType& Modifiers, float HitPower, bool IsSilenced = false)
{
	return HitPower * Modifiers[WeaponModifier::eHitPowerCoeff] * (IsSilenced ? Modifiers[WeaponModifier::eSilencerHitPowerCoeff] : Modifiers[WeaponModifier::eUnsilencedHitPowerCoeff]);
}

inline float ApplyHitImpulse(const WpnModifierType& Modifiers, float HitImpulse)
{
	return HitImpulse * Modifiers[WeaponModifier::eHitImpulseCoeff];
}

inline float ApplyFireDispersionBase(const WpnModifierType& Modifiers, float Dispersion)
{
	return Dispersion * Modifiers[WeaponModifier::eFireDispersionBaseCoeff];
}

inline float ApplyCrosshairInertion(const WpnModifierType& Modifiers, float Inertion)
{
	return Inertion * Modifiers[WeaponModifier::eCrosshairInertionCoeff];
}

inline float ApplyRecoil(const WpnModifierType& Modifiers, float Offset)
{
	return Offset * Modifiers[WeaponModifier::eRecoilCoeff];
}

inline float ApplyRecoilKick(const WpnModifierType& Modifiers, float PreviousAngle, float ShotAngle, float MaxAngle)
{
	if (Modifiers[WeaponModifier::eRecoilCoeff] == 1.0f)
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
	return BulletSpeed * Modifiers[WeaponModifier::eBulletSpeedCoeff];
}

inline float ApplyShotInterval(const WpnModifierType& Modifiers, float Interval)
{
	const float RpmCoeff = Modifiers[WeaponModifier::eRpmCoeff];
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

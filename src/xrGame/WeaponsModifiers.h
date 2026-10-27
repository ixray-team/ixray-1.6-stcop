#pragma once

#include <cfloat>
#include "../xrCore/Containers/FixedVector.h"

// IDs define Lua constants and the fixed order of coefficients in actor saves.
enum class WeaponModifier
{
	HitPowerCoeff,
	RpmCoeff,
	SilencerHitPowerCoeff,
	UnsilencedHitPowerCoeff,
	HitImpulseCoeff,
	FireDispersionBaseCoeff,
	CrosshairInertionCoeff,
	BulletSpeedCoeff,
	RecoilCoeff,
	Count
};

namespace WeaponModifiers
{
using WpnModifierType = FixedVector<float, static_cast<int>(WeaponModifier::Count)>;

inline float ApplyHitPower(const WpnModifierType& Modifiers, float HitPower, bool IsSilenced = false)
{
	return HitPower * Modifiers[static_cast<u32>(WeaponModifier::HitPowerCoeff)] * (IsSilenced ? Modifiers[static_cast<u32>(WeaponModifier::SilencerHitPowerCoeff)] : Modifiers[static_cast<u32>(WeaponModifier::UnsilencedHitPowerCoeff)]);
}

inline float ApplyHitImpulse(const WpnModifierType& Modifiers, float HitImpulse)
{
	return HitImpulse * Modifiers[static_cast<u32>(WeaponModifier::HitImpulseCoeff)];
}

inline float ApplyFireDispersionBase(const WpnModifierType& Modifiers, float Dispersion)
{
	return Dispersion * Modifiers[static_cast<u32>(WeaponModifier::FireDispersionBaseCoeff)];
}

inline float ApplyCrosshairInertion(const WpnModifierType& Modifiers, float Inertion)
{
	return Inertion * Modifiers[static_cast<u32>(WeaponModifier::CrosshairInertionCoeff)];
}

inline float ApplyRecoil(const WpnModifierType& Modifiers, float Offset)
{
	return Offset * Modifiers[static_cast<u32>(WeaponModifier::RecoilCoeff)];
}

inline float ApplyRecoilKick(const WpnModifierType& Modifiers, float PreviousAngle, float ShotAngle, float MaxAngle)
{
	if (Modifiers[static_cast<u32>(WeaponModifier::RecoilCoeff)] == 1.0f)
	{
		return ShotAngle;
	}
	if (Modifiers[static_cast<u32>(WeaponModifier::RecoilCoeff)] == 0.0f)
	{
		return PreviousAngle;
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
	const float Result = BulletSpeed * Modifiers[static_cast<u32>(WeaponModifier::BulletSpeedCoeff)];
	return Result > 0.0f && Result <= FLT_MAX ? Result : BulletSpeed;
}

inline float ApplyShotInterval(const WpnModifierType& Modifiers, float Interval)
{
	const float RpmCoeff = Modifiers[static_cast<u32>(WeaponModifier::RpmCoeff)];
	if (RpmCoeff == 0.0f || RpmCoeff == 1.0f)
	{
		return Interval;
	}

	const float Result = Interval / RpmCoeff;
	if (!(Result > 0.0f && Result <= FLT_MAX))
	{
		return Interval;
	}

	// Keep the float shot timer advancing and bound catch-up shots per frame.
	constexpr float MinimumShotInterval = 0.001f;
	return Result < MinimumShotInterval ? MinimumShotInterval : Result;
}
} // namespace WeaponModifiers

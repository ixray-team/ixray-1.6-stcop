#pragma once

#include <cfloat>
#include "../xrCore/Containers/FixedVector.h"

// IDs define Lua constants and the fixed order of coefficients in living entity saves.
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

inline const WpnModifierType DefaultCoefficients = []
{
	WpnModifierType Coefficients;
	Coefficients.resize(WeaponModifier::eWpnModifierTypeMax);
	for (float& Coefficient : Coefficients)
	{
		Coefficient = 1.0f;
	}
	return Coefficients;
}();

ICF float ApplyRecoilKick(const WpnModifierType& Modifiers, float PreviousAngle, float ShotAngle, float MaxAngle)
{
	if (Modifiers[WeaponModifier::eRecoilCoeff] == 1.0f)
	{
		return ShotAngle;
	}
	// Scale only this shot's contribution, keeping the accumulated angle intact.
	const float Result = PreviousAngle + (ShotAngle - PreviousAngle) * Modifiers[WeaponModifier::eRecoilCoeff];
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

} // namespace WeaponModifiers

using WeaponModifier = WeaponModifiers::WeaponModifier;

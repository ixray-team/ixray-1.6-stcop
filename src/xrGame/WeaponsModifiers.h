#pragma once

#include <cfloat>

enum class WeaponModifier
{
	HitPowerCoeff,
	RpmCoeff,
	Count
};

struct WeaponsModifiers
{
	float HitPowerCoeff = 1.0f;
	float RpmCoeff = 1.0f;

	float Get(WeaponModifier Type) const
	{
		switch (Type)
		{
			case WeaponModifier::HitPowerCoeff:
				return HitPowerCoeff;
			case WeaponModifier::RpmCoeff:
				return RpmCoeff;
			default:
				return 1.0f;
		}
	}

	bool Set(WeaponModifier Type, float Value)
	{
		// This also rejects NaN and infinity without changing the current value.
		if (!(Value >= 0.0f && Value <= FLT_MAX))
		{
			return false;
		}

		switch (Type)
		{
			case WeaponModifier::HitPowerCoeff:
			{
				HitPowerCoeff = Value;
				return true;
			}
			case WeaponModifier::RpmCoeff:
			{
				if (Value == 0.0f)
				{
					return false;
				}
				RpmCoeff = Value;
				return true;
			}
			default:
				return false;
		}
	}

	float ApplyHitPower(float HitPower) const { return HitPower * HitPowerCoeff; }

	float ApplyShotInterval(float Interval) const
	{
		const float Result = Interval / RpmCoeff;
		return Result > 0.0f && Result <= FLT_MAX ? Result : Interval;
	}
};

#pragma once

#include <cfloat>

// IDs are exposed to Lua and stored in actor saves. Append new values before Count.
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

struct WeaponsModifiers
{
	float HitPowerCoeff = 1.0f;
	float RpmCoeff = 1.0f;
	float SilencerHitPowerCoeff = 1.0f;
	float UnsilencedHitPowerCoeff = 1.0f;
	float HitImpulseCoeff = 1.0f;
	float FireDispersionBaseCoeff = 1.0f;
	float CrosshairInertionCoeff = 1.0f;
	float BulletSpeedCoeff = 1.0f;
	float RecoilCoeff = 1.0f;

	float GetCoefficient(WeaponModifier Type) const
	{
		switch (Type)
		{
			case WeaponModifier::HitPowerCoeff:
				return HitPowerCoeff;
			case WeaponModifier::RpmCoeff:
				return RpmCoeff;
			case WeaponModifier::SilencerHitPowerCoeff:
				return SilencerHitPowerCoeff;
			case WeaponModifier::UnsilencedHitPowerCoeff:
				return UnsilencedHitPowerCoeff;
			case WeaponModifier::HitImpulseCoeff:
				return HitImpulseCoeff;
			case WeaponModifier::FireDispersionBaseCoeff:
				return FireDispersionBaseCoeff;
			case WeaponModifier::CrosshairInertionCoeff:
				return CrosshairInertionCoeff;
			case WeaponModifier::BulletSpeedCoeff:
				return BulletSpeedCoeff;
			case WeaponModifier::RecoilCoeff:
				return RecoilCoeff;
			default:
				return 1.0f;
		}
	}

	bool SetCoefficient(WeaponModifier Type, float Value)
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
			case WeaponModifier::SilencerHitPowerCoeff:
			{
				SilencerHitPowerCoeff = Value;
				return true;
			}
			case WeaponModifier::UnsilencedHitPowerCoeff:
			{
				UnsilencedHitPowerCoeff = Value;
				return true;
			}
			case WeaponModifier::HitImpulseCoeff:
			{
				HitImpulseCoeff = Value;
				return true;
			}
			case WeaponModifier::FireDispersionBaseCoeff:
			{
				FireDispersionBaseCoeff = Value;
				return true;
			}
			case WeaponModifier::CrosshairInertionCoeff:
			{
				CrosshairInertionCoeff = Value;
				return true;
			}
			case WeaponModifier::BulletSpeedCoeff:
			{
				if (Value == 0.0f)
				{
					return false;
				}
				BulletSpeedCoeff = Value;
				return true;
			}
			case WeaponModifier::RecoilCoeff:
			{
				RecoilCoeff = Value;
				return true;
			}
			default:
				return false;
		}
	}

	float ApplyHitPower(float HitPower, bool IsSilenced = false) const
	{
		return HitPower * HitPowerCoeff * (IsSilenced ? SilencerHitPowerCoeff : UnsilencedHitPowerCoeff);
	}

	float ApplyHitImpulse(float HitImpulse) const { return HitImpulse * HitImpulseCoeff; }
	float ApplyFireDispersionBase(float Dispersion) const { return Dispersion * FireDispersionBaseCoeff; }
	float ApplyCrosshairInertion(float Inertion) const { return Inertion * CrosshairInertionCoeff; }
	float ApplyRecoil(float Offset) const { return Offset * RecoilCoeff; }

	float ApplyRecoilKick(float PreviousAngle, float ShotAngle, float MaxAngle) const
	{
		if (RecoilCoeff == 1.0f)
		{
			return ShotAngle;
		}
		if (RecoilCoeff == 0.0f)
		{
			return PreviousAngle;
		}

		// Scale only this shot's contribution, keeping the accumulated angle intact.
		const float Result = PreviousAngle + ApplyRecoil(ShotAngle - PreviousAngle);
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

	float ApplyBulletSpeed(float BulletSpeed) const
	{
		const float Result = BulletSpeed * BulletSpeedCoeff;
		return Result > 0.0f && Result <= FLT_MAX ? Result : BulletSpeed;
	}

	float ApplyShotInterval(float Interval) const
	{
		if (RpmCoeff == 1.0f)
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
};

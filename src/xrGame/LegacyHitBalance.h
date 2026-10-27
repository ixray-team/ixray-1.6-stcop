#pragma once

namespace LegacyHitBalance
{
	constexpr float ClearSkyArtefactDamage(float HitPower, float Protection)
	{
		const float Result = HitPower - Protection;
		return Result > 0.0f ? Result : 0.0f;
	}

	constexpr float ShadowStalkerBulletDamage(float HitPower, float BoneArmor, float HitFraction)
	{
		const float Result = HitPower - BoneArmor;
		const float Minimum = HitPower * HitFraction;
		return Result < Minimum ? Minimum : Result;
	}

	constexpr float ShadowPsyDamage(float HitPower, float Immunity)
	{
		return HitPower * Immunity * Immunity;
	}
}

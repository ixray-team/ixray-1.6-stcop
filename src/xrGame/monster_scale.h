#pragma once

namespace MonsterScale
{
	inline float Navigation(float Scale)
	{
		return std::max(1.f, Scale);
	}

	inline float Movement(float Scale)
	{
		return _sqrt(Scale);
	}

	inline float Arrival(float Scale)
	{
		return std::min(1.f, Scale);
	}

	inline u32 LocalMoveTime(u32 Time, float Scale)
	{
		return u32(float(Time) / std::min(1.f, Movement(Scale)));
	}

	inline float MeleeDistance(float Distance, float OwnerScale, float TargetScale, float TargetRadius)
	{
		return std::max(0.f, Distance + TargetRadius * (OwnerScale / TargetScale - 1.f));
	}

	inline bool NeedsPathUpdate(float PreviousScale, float Scale)
	{
		return std::abs(Navigation(Scale) / Navigation(PreviousScale) - 1.f) >= 0.02f;
	}

	inline bool TurningInPlace(bool Turning, float Angle)
	{
		return Angle > deg2rad(Turning ? 10.f : 60.f);
	}
}

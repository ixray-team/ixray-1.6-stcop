#pragma once

// Pure decisions shared by gameplay and standalone regression tests.
class CMonsterBehaviorPolicy
{
public:
	enum ENoiseOutcome
	{
		Ignore,
		Panic,
		Investigate
	};
	static constexpr ENoiseOutcome AccumulatedNoiseOutcome(bool Interested, bool Frightened, bool Cooling)
	{
		if (Frightened) return Cooling ? Ignore : Panic;
		return Interested ? Investigate : Ignore;
	}
	static constexpr ENoiseOutcome NoiseOutcome(bool Enabled, float Radius, float DistanceSquared, float Power, float MinimumPower, float PanicChance, bool Attract, float Roll, bool CoolingDown, bool CanReact)
	{
		if (!Enabled || !(Radius > 0.f) || !(Power >= MinimumPower) || DistanceSquared > Radius * Radius || CoolingDown || !CanReact)
		{
			return Ignore;
		}
		if (PanicChance > 0.f && Roll < PanicChance)
		{
			return Panic;
		}
		return Attract ? Investigate : Ignore;
	}
	static constexpr bool Active(u32 Now, u32 Until)
	{
		return Until != 0 && (Until - Now) != 0 && (Until - Now) < 0x80000000u;
	}
	static constexpr bool InspectionActive(bool Committed, bool Arrived, bool Unreachable)
	{
		return Committed && !Arrived && !Unreachable;
	}
	static constexpr bool ApproachActive(u32 Now, u32 Until, u32 Progress, u32 StuckTime, bool Arrived, bool Unreachable)
	{
		return !Arrived && !Unreachable && Progress != 0 && Active(Now, Until) && Now - Progress < StuckTime;
	}
	static constexpr u32 ApproachDuration(float Distance, float Speed, u32 Maximum)
	{
		const float Estimate = Distance / (Speed > .1f ? Speed : .1f) * 2000.f;
		const u32 Duration = Estimate < 15000.f ? 15000u : Estimate > float(Maximum) ? Maximum
																					 : u32(Estimate);
		return Duration < Maximum ? Duration : Maximum;
	}
};

#pragma once
#include "../CELCrowSettings.h"
class CBaseMonster;
struct SELSoundEvent;
struct SELPerceptionProfile;

class CELCrowPerception
{
public:
	void Observe(CBaseMonster& Owner, SELCrowState& State, const SELCrowSettings& Settings, float DeltaTime);
	void Hear(CBaseMonster& Owner, SELCrowState& State, const SELCrowSettings& Settings, const SELSoundEvent& Event, const SELPerceptionProfile& Profile);
	u16 FoodIdentifier() const { return KnownFood; }
	u16 ThreatIdentifier() const { return KnownThreat; }
	bool IsFoodSafe(CBaseMonster& Owner, const Fvector& Position, float Radius) const;
	void Remove(u16 ObjectIdentifier);

private:
	void ObserveWeather(SELCrowState& State, const SELCrowSettings& Settings, float DeltaTime);
	u32 NextScan = 0;
	u32 NextWeather = 0;
	u32 ThreatCursor = 0;
	u32 FoodCursor = 0;
	u16 KnownFood = u16(-1);
	u16 KnownThreat = u16(-1);
	bool ThreatWasHeard = false;
	bool IsRaining = false;
	bool IsNight = false;
};

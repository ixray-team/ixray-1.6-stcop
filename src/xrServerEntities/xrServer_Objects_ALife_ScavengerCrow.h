#pragma once
#include "xrServer_Objects_ALife_Monsters.h"
#include "../xrGame/ai/monsters/flying/crow_behavior_timing.h"

// Offline simulation uses ALife's existing graph movement and scheduler.
// Binary NPC/client packets retain their existing layout.
class CSE_ALifeScavengerCrow : public CSE_ALifeMonsterBase
{
public:
	enum class ERest : u8 { None, Ground, Roof, Corpse, Shelter, Night };
	struct SState
	{
		bool Valid = false, UpdatedOffline = false, NeedsPlacement = false, Enabled = true;
		float Satiety = .8f, Fear = 0.f, Remaining = 0.f, Altitude = 20.f;
		ERest Rest = ERest::None;
		u32 Corpse = u32(-1);
		Fvector Danger = {0,0,0};
		CrowBehaviorTiming::SCrowReaction Rain, Night;
		ALife::_TIME_ID Time = 0;
	} State;

	explicit CSE_ALifeScavengerCrow(const char* Section) : CSE_ALifeMonsterBase(Section) {}
	void STATE_Serialize(ISaveObject& Object) override;
#ifdef XRGAME_EXPORTS
	void update() override;
	void add_offline(const xr_vector<ALife::_OBJECT_ID>& SavedChildren, const bool& UpdateRegistries) override;
#endif

private:
	bool ConfigLoaded = false, WeatherRain = false;
	float Interval = 1.f, HungerTime = 900.f, Hungry = .35f, FearRadius = 10.f, FearDecay = .2f;
	float FearCalm = .1f;
	float NightStart = 20.f, NightEnd = 6.f, RainStart = .1f, RainStop = .03f, AltitudeMin = 12.f, AltitudeMax = 25.f;
	float SeedGain = .012f, MeatGain = .07f, FlightSpeed = 6.f, FoodRadius = 70.f;
	bool NightEnabled = true, RainEnabled = true;
	CrowBehaviorTiming::SCrowRange Sky, Ground, Roof, Corpse, Escape, NightRest, NightDelay, WakeDelay, RainDelay, DryDelay, Tick;
#ifdef XRGAME_EXPORTS
	void LoadOfflineConfig();
	void ChooseOfflineGoal();
#endif
};

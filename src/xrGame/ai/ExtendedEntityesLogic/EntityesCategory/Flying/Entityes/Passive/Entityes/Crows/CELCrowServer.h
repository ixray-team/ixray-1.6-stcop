#pragma once
#include <algorithm>
#include "../xrServerEntities/xrServer_Objects_ALife_Monsters.h"
#include "../xrCore/Save/SaveInterface.h"
#include "CELCrowState.h"
struct SELCrowSettings;

// Serialization is inline because the native server factory is also built without xrGame.
// ALife simulation remains in the game implementation below this category.
class CELCrowServer final : public CSE_ALifeMonsterBase
{
public:
	explicit CELCrowServer(const char* Section) : CSE_ALifeMonsterBase(Section) {}
	SELCrowState State;
	void STATE_Serialize(ISaveObject& Object) override
	{
		CSE_ALifeMonsterBase::STATE_Serialize(Object);
		BEGIN_CHUNK(Object, "ScavengerCrowOffline")
		{
			u8 Version = 2;
			Object << Version;
			if (Version != 1 && Version != 2)
			{
				State = SELCrowState();
				Msg("! Ворона: неподдерживаемая версия сохранённого состояния %u", u32(Version));
				return;
			}
			u8 Rest = u8(State.Rest);
			Object << State.Valid << State.UpdatedOffline << State.Enabled;
			Object << State.Satiety << State.Fear << State.Remaining << State.Altitude << Rest;
			if (!Object.IsSave() && Version == 1)
			{
				u16 LegacyCorpse = u16(-1);
				Object << LegacyCorpse;
				State.Corpse = LegacyCorpse == u16(-1) ? u32(-1) : u32(LegacyCorpse);
			}
			else
			{
				Object << State.Corpse;
			}
			Object << State.Time;
			Object << State.Danger.x << State.Danger.y << State.Danger.z;
			Object << State.Rain.Observed << State.Rain.Active << State.Rain.Remaining;
			Object << State.Night.Observed << State.Night.Active << State.Night.Remaining;
			if (!Object.IsSave())
			{
				if (Rest > u8(EELCrowRest::Night) || !_valid(State.Satiety) || !_valid(State.Fear) || !_valid(State.Remaining) ||
					!_valid(State.Altitude) || !_valid(State.Danger) || !_valid(State.Rain.Remaining) || !_valid(State.Night.Remaining))
				{
					State = SELCrowState();
					return;
				}
				State.Rest = EELCrowRest(Rest);
				clamp(State.Satiety, 0.f, 1.f);
				clamp(State.Fear, 0.f, 1.f);
				State.Remaining = std::max(0.f, State.Remaining);
				State.Altitude = std::max(1.f, State.Altitude);
				State.Rain.Remaining = std::max(0.f, State.Rain.Remaining);
				State.Night.Remaining = std::max(0.f, State.Night.Remaining);
				State.NeedsPlacement = State.Valid;
				HasConfiguration = false;
			}
		}
	}
#ifdef XRGAME_EXPORTS
	void update() override;
	void add_offline(const xr_vector<ALife::_OBJECT_ID>& SavedChildren, const bool& UpdateRegistries) override;

private:
	void Initialize();
	void ChooseGoal();
	void UpdateWeather(float DeltaTime, ALife::_TIME_ID CurrentTime);
	CSE_ALifeCreatureAbstract* ObserveNeighbors(bool& Threatened);
	void SelectRest(CSE_ALifeCreatureAbstract* Food, bool WasAfraid);
	void UpdateMovement();
#endif
private:
	bool HasConfiguration = false;
	bool IsRaining = false;
	float NextInterval = 1.f;
};

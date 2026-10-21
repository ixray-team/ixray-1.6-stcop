#pragma once
#include <algorithm>
#include "xrEngine/AI/alife_space.h"

struct SELCrowRange
{
	float Minimum = 0.f;
	float Maximum = 0.f;
	float Sample() const { return Random.randF(Minimum, Maximum); }
};

struct SELCrowReaction
{
	bool Observed = false;
	bool Active = false;
	float Remaining = 0.f;
	void Update(bool Present, float DeltaTime, const SELCrowRange& EnterDelay, const SELCrowRange& LeaveDelay)
	{
		if (Present != Observed)
		{
			Observed = Present;
			Remaining = (Present ? EnterDelay : LeaveDelay).Sample();
		}
		Remaining = std::max(0.f, Remaining - DeltaTime);
		if (Remaining <= 0.f)
		{
			Active = Observed;
		}
	}
};

enum class EELCrowRest : u8
{
	None,
	Ground,
	Roof,
	Corpse,
	Shelter,
	Night
};

struct SELCrowState
{
	bool Valid = false;
	bool UpdatedOffline = false;
	bool NeedsPlacement = false;
	bool Enabled = true;
	float Satiety = .8f;
	float Fear = 0.f;
	float Remaining = 0.f;
	float Altitude = 20.f;
	EELCrowRest Rest = EELCrowRest::None;
	u32 Corpse = u32(-1);
	Fvector Danger = {0.f, 0.f, 0.f};
	SELCrowReaction Rain;
	SELCrowReaction Night;
	ALife::_TIME_ID Time = 0;
};

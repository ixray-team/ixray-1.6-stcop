#pragma once
#include "DescRegistry.h"
#include "../EntityCondition.h"

// Built on the first use of a section, not on spawn: eat_* lines stay required only for items that are actually used.
struct SEatableEffectsDesc final
{
	using Registry = TDescRegistry<SEatableEffectsDesc>;

	SMedicineInfluenceValues Influence;
	xr_vector<SBooster> Boosters;

	void Load(const shared_str& Section);
};

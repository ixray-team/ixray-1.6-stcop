#pragma once
#include "DescRegistry.h"

struct SBottleItemDesc final
{
	using Registry = TDescRegistry<SBottleItemDesc>;

	shared_str BreakParticles;
	shared_str BreakSound;

	void Load(const shared_str& Section);
};

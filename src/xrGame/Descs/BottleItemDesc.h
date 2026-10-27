#pragma once
#include "DescRegistry.h"

XREFLECT(IniRead, Owner="CBottleItem")
struct SBottleItemDesc final
{
	using Registry = TDescRegistry<SBottleItemDesc>;

	XPROPERTY(IniReadExist, Name="break_particles")
	shared_str BreakParticles;
	XPROPERTY(IniReadExist, Name="break_sound")
	shared_str BreakSound;

	void Load(const shared_str& Section);
};

#pragma once
#include "DescRegistry.h"

XREFLECT(IniRead, Owner = "CMagazine")
struct SMagazineDesc final
{
	using Registry = TDescRegistry<SMagazineDesc>;

	XPROPERTY(IniRead, Name = "mag_size")
	u8 MagSize = 0u;

	RStringVec AmmoTypes = {};

	void Load(const shared_str& Section);
};

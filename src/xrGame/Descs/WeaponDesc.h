#pragma once
#include "DescRegistry.h"

XREFLECT(IniRead, Owner="CWeapon")
struct SWeaponDesc final
{
	using Registry = TDescRegistry<SWeaponDesc>;

	XPROPERTY(IniReadExist, Name = "fire_point2")
	Fvector FirePointGL = zero_vel;
	XPROPERTY(IniReadExist, Name = "fire_bone2")
	shared_str FireBoneGL;

	void Load(const shared_str& Section);
};

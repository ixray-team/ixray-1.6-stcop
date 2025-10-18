#pragma once

#include "WeaponMagazinedWGrenade.h"
#include "../xrScripts/script_export_space.h"

class CWeaponAK74 final : public CWeaponMagazinedWGrenade
{
	using inherited = CWeaponMagazinedWGrenade;
public:
	CWeaponAK74() = default;
	virtual ~CWeaponAK74() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
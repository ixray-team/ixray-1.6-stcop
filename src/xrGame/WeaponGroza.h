#pragma once

#include "WeaponMagazinedWGrenade.h"
#include "../xrScripts/script_export_space.h"

class CWeaponGroza final : public CWeaponMagazinedWGrenade
{
	using inherited = CWeaponMagazinedWGrenade;
public:
	CWeaponGroza() = default;
	virtual	~CWeaponGroza() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
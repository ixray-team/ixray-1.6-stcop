#pragma once

#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

class CWeaponVintorez final : public CWeaponMagazined
{
	using inherited = CWeaponMagazined;
public:
	CWeaponVintorez() = default;
	virtual ~CWeaponVintorez() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};